"""Synthetic-only worker checks. No live accounts, DB or API calls."""
import asyncio
import copy
import hashlib
import json
import os
import unittest
from datetime import datetime, timezone
from pathlib import Path
from unittest.mock import AsyncMock
import httpx
from worker.main import (Worker, WorkerFailure, Helper, canonical, decrypt_access, encrypt_access,
                         pack_snapshot, unpack_snapshot, plain, quiet_interval)

KEY = 'ab' * 32
AUTH = dict(userid='fixture', token='synthetic', user_name='fixture@example.invalid')
CONTEXT = dict(account_id='fixture', championship_id='l', user_team_id='', player_id='',
               competition_id='unknown', season='unknown', scoring_version='unknown')

def descriptor(handler='get_real_clubs', args=None):
    value = dict(version=1, handler=handler, args=args or dict(championship_id='l'), context=CONTEXT)
    value['resource_key'] = hashlib.sha256(canonical(value).encode()).hexdigest()
    return value

class FakeStore:
    def __init__(self):
        self.allowed = True
        self.checkpoints = {}
        self.chunks = {}
        self.snapshots = {}
        self.calls = []
        self.row = dict(account_id='fixture', generation=1, enabled=True, auth_status='active',
                       verified_leagues={'l': 't'}, excluded_leagues=[],credential_id='1',
                       encrypted_token=encrypt_access(AUTH, 'fixture', 1, KEY))
    async def account(self, identity):
        return self.row
    async def rpc(self, name, **p):
        self.calls.append((name, copy.deepcopy(p)))
        if name == 'guard': return self.allowed
        if name == 'checkpoint':
            key = p['p_request']
            if p.get('p_response') is not None: self.checkpoints[key] = copy.deepcopy(p['p_response'])
            return copy.deepcopy(self.checkpoints.get(key))
        if name in ('publish', 'publish_child'):
            key = p.get('p_key') or p['p_descriptor']['resource_key']
            self.snapshots[key] = copy.deepcopy(p['p_manifest'])
        return self.allowed
    async def request(self, method, table, **kw):
        self.calls.append((table, copy.deepcopy(kw)))
        if table == 'worker_chunks':
            row = kw['json'];self.chunks.setdefault(row['version'], []).append(row['data'])
        return []

class ContractTests(unittest.TestCase):
    def test_credential_binding_and_tampering(self):
        row = FakeStore().row
        self.assertEqual(decrypt_access(row, KEY), AUTH)
        for modified in (dict(row, account_id='foreign'), dict(row, credential_id='foreign'), dict(row, encrypted_token=row['encrypted_token'][:-5]+'AAAAA')):
            with self.assertRaises(WorkerFailure): decrypt_access(modified, KEY)
        with self.assertRaises(WorkerFailure): decrypt_access(row, 'cd'*32)
    def test_compression_limits_and_integrity(self):
        value = {'kind':'double', 'values':[0,-2,None]}
        parts, size, digest = pack_snapshot(value)
        m = dict(parts=len(parts), json_bytes=size, sha256=digest)
        self.assertEqual(unpack_snapshot(m, parts), value)
        for bad in (dict(m, sha256='0'*64), dict(m, json_bytes=size-1), dict(m, parts=2)):
            with self.assertRaises(WorkerFailure): unpack_snapshot(bad, parts)
        with self.assertRaises(WorkerFailure): pack_snapshot({'token':'synthetic'})
    def test_quiet_hours_timezone_and_dst(self):
        account = dict(timezone='America/Los_Angeles', quiet_start=0, quiet_end=7)
        for hour in (8,9):
            self.assertEqual(quiet_interval(account,60,datetime(2026,11,1,hour,tzinfo=timezone.utc)),3600)
        self.assertEqual(quiet_interval(account,60,datetime(2026,11,1,16,tzinfo=timezone.utc)),60)
        self.assertEqual(quiet_interval(account,21600,datetime(2026,11,1,9,tzinfo=timezone.utc)),21600)
        account.update(quiet_start=22,quiet_end=6)
        self.assertEqual(quiet_interval(account,60,datetime(2026,11,1,8,tzinfo=timezone.utc)),3600)
    def test_R_contracts(self):
        value=json.loads(Path('/tmp/futmondo-worker-descriptor.json').read_text())
        resource=value.pop('resource_key')
        self.assertEqual(hashlib.sha256(canonical(value).encode()).hexdigest(),resource)
        row=dict(account_id='fixture',encrypted_token=Path('/tmp/futmondo-worker-encrypted.txt').read_text().strip(),credential_id=Path('/tmp/futmondo-worker-credential-id.txt').read_text().strip())
        self.assertEqual(decrypt_access(row,KEY),AUTH)

class WorkerTests(unittest.IsolatedAsyncioTestCase):
    async def asyncSetUp(self):
        self.store=FakeStore();self.requests=[]
        async def transport(request):
            self.requests.append(request)
            self.assertEqual(request.url.host,'api.futmondo.com')
            self.assertEqual(json.loads(request.content)['header']['token'],'synthetic')
            return httpx.Response(200,json={'header':{'token':'synthetic'},'query':{},'answer':[{'id':'club','name':'Club','logo':'logo'}]})
        self.client=httpx.AsyncClient(transport=httpx.MockTransport(transport))
        self.worker=Worker(self.store,self.client,KEY,Path.cwd());self.worker.admission_blocked=False
        self.job=dict(account_id='fixture',resource_key=descriptor()['resource_key'],generation=1,interval_seconds=300,descriptor=descriptor())
    async def asyncTearDown(self):
        await self.client.aclose()
        await asyncio.gather(*(h.close() for h in self.worker.helpers))
    def message(self,url='https://api.futmondo.com/1/league/championshipteams'):
        return dict(type='http',url=url,body=dict(query={'championshipId':'l'},answer=[]))
    async def test_acquire_checkpoint_restart_reuse(self):
        first=await self.worker.source(self.job,'owner',self.message())
        self.assertNotIn('header',first['body']);self.assertNotIn('query',first['body'])
        second_worker=Worker(self.store,self.client,KEY,Path.cwd());second_worker.admission_blocked=False
        again=await second_worker.source(self.job,'owner',self.message())
        self.assertEqual(first,again);self.assertEqual(len(self.requests),1)
    async def test_no_mutations_or_ssrf(self):
        for route in ('https://api.futmondo.com/1/market/bid','https://api.futmondo.com/1/notification/markreaded',
                      'https://api.futmondo.com.evil.invalid/1/league/championshipteams',
                      'https://api.futmondo.com/1/league/championshipteams?token=synthetic'):
            with self.assertRaises(WorkerFailure):await self.worker.source(self.job,'owner',self.message(route))
        self.assertFalse(self.requests)
    async def test_revocation_and_membership_stop_dispatch(self):
        self.store.allowed=False
        with self.assertRaises(WorkerFailure):await self.worker.exchange(self.job,'owner',self.message())
        self.store.allowed=True;self.store.row['verified_leagues']={}
        with self.assertRaises(WorkerFailure):await self.worker.source(self.job,'owner',self.message())
        self.assertFalse(self.requests)
    async def test_fresh_persisted_child_resource_is_reused(self):
        value={'kind':'double','values':[0,-2,None]}
        chunks,size,digest=pack_snapshot(value)
        manifest=dict(context=CONTEXT,parts=len(chunks),json_bytes=size,sha256=digest,status='ok',
            observed_at='2026-11-01T00:00:00+00:00',checked_at='2026-11-01T00:00:00+00:00',valid_until='2099-01-01T00:00:00+00:00')
        original=self.store.rpc
        async def rpc(name,**p):
            if name=='read':return {'resources':[dict(manifest=manifest,chunks=chunks)]}
            return await original(name,**p)
        self.store.rpc=rpc
        result=await self.worker.exchange(self.job,'owner',dict(type='resource',descriptor=descriptor()))
        self.assertEqual(result['value'],value)
        self.assertEqual(self.worker.source_stamps['owner'],[manifest['checked_at']])
        self.assertFalse(self.requests)

    async def test_rate_limit_honors_retry_after(self):
        self.client._transport=httpx.MockTransport(lambda request:httpx.Response(429,headers={'Retry-After':'120'}))
        with self.assertRaises(WorkerFailure) as raised:
            await self.worker.source(self.job,'owner',self.message())
        self.assertEqual(raised.exception.code,'source_rate_limited')
        self.assertEqual(raised.exception.retry_after,120)
        self.assertEqual(self.worker.request_count,1)
        self.assertTrue(any(name=='worker_accounts' and 'rate_limited_until' in args.get('json',{}) for name,args in self.store.calls))

    async def test_authentication_is_not_retried(self):
        self.client._transport=httpx.MockTransport(lambda request:httpx.Response(401))
        with self.assertRaisesRegex(WorkerFailure,'source_authentication'):
            await self.worker.source(self.job,'owner',self.message())
        self.assertEqual(self.worker.request_count,1)
    async def test_real_R_acquisition_and_durable_result(self):
        result=await self.worker.helpers[0].run(self.job['descriptor'],lambda message:self.worker.exchange(self.job,'owner',message))
        self.assertEqual(plain(result['value'])[0]['teamId'],'club')
        await self.worker.publish(self.job,'owner',result)
        manifest=self.store.snapshots[self.job['resource_key']]
        restored=unpack_snapshot(manifest,self.store.chunks[manifest['version']])
        self.assertEqual(plain(restored)[0]['teamId'],'club')
        self.assertEqual(len(self.requests),1)
        chunks=[i for i,x in enumerate(self.store.calls) if x[0]=='worker_chunks']
        publish=[i for i,x in enumerate(self.store.calls) if x[0]=='publish']
        self.assertLess(max(chunks),min(publish))
    async def test_empty_named_args_bootstrap_and_notifications(self):
        # R strips login from named calls, leaving an empty named list. JSONB
        # retains names=[] and args={}, unlike the unnamed enrollment args=[].
        for handler in ('get_active_championships', 'get_notification_unread'):
            with self.subTest(handler=handler):
                d = dict(version=1, handler=handler, args={},
                         context=dict(CONTEXT, championship_id=''),
                         encoded_args=dict(kind='list', names=[], values=[], context=False, meta=[]))
                d['resource_key'] = hashlib.sha256(canonical(d).encode()).hexdigest()
                job = dict(self.job, descriptor=d, resource_key=d['resource_key'])
                answer = {'championships': [{'id': 'l', 'name': 'League', 'league': 'premier',
                           'ps': 'stats', 'userteam': {'id': 't'}}],
                          'leagues': [{'_id': 'premier', 'season': 'S-current'}]} if handler == 'get_active_championships' else 3
                async def transport(request):
                    self.requests.append(request)
                    return httpx.Response(200, json={'answer': answer})
                self.client._transport = httpx.MockTransport(transport)
                before = len(self.requests)
                result = await self.worker.helpers[0].run(d, lambda m: self.worker.exchange(job, 'owner', m))
                self.assertEqual(len(self.requests) - before, 1)
                decoded = plain(result['value'])
                if handler == 'get_active_championships':
                    self.assertEqual(decoded['championships'][0]['id'], 'l')
                    self.assertTrue(any(x['handler'] == 'get_teams' for x in result['discoveries']))
                else:
                    self.assertEqual(decoded, 3)
                await self.worker.publish(job, 'owner', result)
                manifest = self.store.snapshots[d['resource_key']]
                restored = unpack_snapshot(manifest, self.store.chunks[manifest['version']])
                self.assertEqual(plain(restored), decoded)

    async def test_replayed_response_does_not_renew_R_observation_time(self):
        async def old_response(message):
            if message['type']=='http':
                return dict(status=200,body={'answer':[{'id':'club','name':'Club','logo':'logo'}]},observed_at='2020-01-01T00:00:00Z')
            if message['type']=='database':return {'rows':[]}
            if message['type']=='resource':return {'value':None}
            return {'ok':True}
        result=await self.worker.helpers[0].run(self.job['descriptor'],old_response)
        self.assertTrue(result['observed_at'].startswith('2020-01-01'),result['observed_at'])

    async def test_failed_fact_ack_is_never_success(self):
        self.store.allowed=False
        with self.assertRaises(WorkerFailure):
            await self.worker.exchange(self.job,'owner',dict(type='persist',table='players',payload={'id':'p','name':'Player'},conflict='id'))

if __name__=='__main__':unittest.main(verbosity=2)
