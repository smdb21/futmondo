"""Synthetic-only worker checks. No live accounts, DB or API calls."""
import asyncio
import copy
import hashlib
import json
import os
import time
import unittest
from datetime import datetime, timezone
from pathlib import Path
from unittest.mock import AsyncMock
from types import SimpleNamespace
import httpx
from worker.main import (Worker, WorkerFailure, Helper, canonical, decrypt_access, encrypt_access,
                         pack_snapshot, unpack_snapshot, plain, quiet_interval, filter_football_leagues, source_response_rejected, stamp)

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
        if name == 'read':
            resources = []
            for key in p.get('p_versions', {}):
                if key in self.snapshots:
                    manifest = self.snapshots[key]
                    resources.append(dict(manifest=manifest, chunks=self.chunks[manifest['version']]))
            return dict(resources=resources)
        if name == 'guard': return self.allowed
        if name == 'enqueue_batch': return len(p['p_descriptors']) if self.allowed else None
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
    def test_native_rejections_are_not_empty_successes(self):
        for body in ({'answer':'api.error.general'},{'answer':{'error':'api.error.general'}},
                {'answer':{'code':'api.error.general'}},{'error':True}):
            self.assertTrue(source_response_rejected(body))
        for body in ({'answer':[]},{'answer':0},{'code':'api.general.ok','answer':[]},
                {'answer':{'error':False,'players':[]}}):
            self.assertFalse(source_response_rejected(body))
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
    def test_basketball_is_source_filtered(self):
        body = {'answer': {'championships': [
            {'id': 'soccer', 'sport': 'soccer'}, {'id': 'hoops', 'sport': 'basket'},
            {'id': 'via-league', 'league': 'nba'}, {'id': 'unknown', 'name': 'Basketball fans'}],
            'leagues': [{'_id': 'nba', 'sport': 'basketball'}]}}
        filtered = filter_football_leagues(body)
        self.assertEqual([x['id'] for x in filtered['answer']['championships']], ['soccer', 'unknown'])

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
    async def test_active_league_http_and_legacy_checkpoints_exclude_basketball(self):
        raw = {'answer': {'championships': [{'id':'l','sport':'soccer'}, {'id':'b','sport':'basket'}]}}
        async def transport(request):
            self.requests.append(request)
            return httpx.Response(200, json=raw)
        self.client._transport = httpx.MockTransport(transport)
        job = dict(self.job, descriptor=descriptor('get_active_championships'))
        job['descriptor']['context'] = dict(CONTEXT, championship_id='')
        message = lambda: dict(type='http',url='https://api.futmondo.com/2/user/activechampionships',body={'query':{}})
        live = await self.worker.source(job, 'owner', message())
        self.assertEqual([x['id'] for x in live['body']['answer']['championships']], ['l'])
        legacy = dict(status=200,body=raw,observed_at='2020-01-01T00:00:00Z')
        chunks,size,digest = pack_snapshot(legacy)
        key = next(iter(self.store.checkpoints))
        self.store.checkpoints[key] = dict(manifest=dict(parts=len(chunks),json_bytes=size,sha256=digest),chunks=chunks)
        replay = await self.worker.source(job, 'owner', message())
        self.assertEqual([x['id'] for x in replay['body']['answer']['championships']], ['l'])
        self.assertEqual(replay['observed_at'], legacy['observed_at'])
        self.assertEqual(len(self.requests), 1)

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

    async def test_clause_rejects_old_general_summary_but_reuses_recent_source(self):
        value={'kind':'double','values':[1]}
        chunks,size,digest=pack_snapshot(value)
        manifest=dict(context=CONTEXT,parts=len(chunks),json_bytes=size,sha256=digest,status='ok',
            observed_at=stamp(time.time()-45),checked_at=stamp(),valid_until=stamp(time.time()+250))
        original=self.store.rpc
        async def rpc(name,**p):
            if name=='read':return {'resources':[dict(manifest=manifest,chunks=chunks)]}
            return await original(name,**p)
        self.store.rpc=rpc
        job=dict(self.job,descriptor=descriptor('get_clause_transfer_summary'))
        message=dict(type='resource',descriptor=descriptor('get_player_summary'))
        self.assertIsNone((await self.worker.exchange(job,'owner',message))['value'])
        manifest['observed_at']=stamp(time.time()-5)
        result=await self.worker.exchange(job,'owner',message)
        self.assertEqual(result['value'],value)
        self.assertEqual(result['observed_at'],manifest['observed_at'])
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
    async def test_expired_history_snapshot_reuses_only_acknowledged_fact_hashes(self):
        paths = []
        news = dict(_id='tx-one', created='2026-11-01T12:00:00Z', price=120,
            _player=dict(_id='p', name='Fixture player'), _buyer=dict(_id='t', name='Team'),
            bids=[dict(_id='bid-one', u=dict(_id='t', name='Team'), bid=110)])
        def transport(request):
            paths.append(request.url.path)
            query = json.loads(request.content).get('query', {})
            items = [news] if request.url.path == '/1/locker/pressroom' and not query.get('from') else []
            return httpx.Response(200, json={'answer': {'news': items}})
        self.client._transport = httpx.MockTransport(transport)
        d = descriptor('get_championship_pressroom')
        job = dict(self.job, descriptor=d, resource_key=d['resource_key'], interval_seconds=60)
        result = await self.worker.helpers[0].run(d, lambda m: self.worker.exchange(job, 'owner', m))
        self.assertIn('history_write_ack', result['value']['meta'])
        await self.worker.publish(job, 'owner', result)
        for manifest in self.store.snapshots.values():
            manifest['valid_until'] = '2001-01-01T00:00:00Z'
        before = len(self.store.calls)
        restarted = Worker(self.store, self.client, KEY, Path.cwd())
        restarted.admission_blocked = False
        try:
            again = await restarted.helpers[0].run(d, lambda m: restarted.exchange(job, 'restart', m))
            self.assertEqual(plain(again['value']), plain(result['value']))
            self.assertEqual(again['observed_at'], result['observed_at'])
            native_writes = [p for name, p in self.store.calls[before:] if name == 'write_fact'
                and p['p_table'] in ('players', 'market_transactions', 'auction_observations', 'bid_observations')]
            self.assertFalse(native_writes)
            self.assertEqual(len(paths), 3)
        finally:
            await asyncio.gather(*(h.close() for h in restarted.helpers))

    async def test_finance_optional_rejection_reaches_R_but_auth_still_aborts(self):
        for code in ('source_rejected', 'source_authentication', 'access_revoked'):
            helper = Helper(Path.cwd())
            message = dict(type='http', url='https://api.futmondo.com/1/userteam/information', body={})
            result = dict(type='result', value={'kind':'double', 'values':[1]})
            writes = []
            helper.process = SimpleNamespace(returncode=None,
                stdout=SimpleNamespace(readline=AsyncMock(side_effect=[(canonical(message)+'\n').encode(), (canonical(result)+'\n').encode()])),
                stdin=SimpleNamespace(write=lambda raw: writes.append(json.loads(raw)), drain=AsyncMock()))
            if code == 'source_rejected':
                answer = await helper.run(descriptor('fm_worker_finances'), AsyncMock(side_effect=WorkerFailure(code)))
                self.assertEqual(answer, result)
                self.assertEqual(writes[-1], {'error':'source_rejected'})
            else:
                with self.assertRaisesRegex(WorkerFailure, code):
                    await helper.run(descriptor('fm_worker_finances'), AsyncMock(side_effect=WorkerFailure(code)))

    async def test_unrelated_old_source_cannot_backdate_new_child_snapshot(self):
        d = descriptor('fm_worker_finances')
        job = dict(self.job, descriptor=d, resource_key=d['resource_key'])
        self.worker.source_stamps['owner'] = ['2001-01-01T00:00:00Z', '2026-11-13T00:00:00Z']
        result = dict(value={'kind':'double','values':[1]}, observed_at='2026-11-13T00:00:00Z',
            checked_at='2026-11-13T00:00:05Z')
        await self.worker.publish(job, 'owner', result, child=descriptor())
        manifest = self.store.snapshots[descriptor()['resource_key']]
        self.assertEqual(manifest['checked_at'], result['observed_at'])
        self.assertEqual(manifest['observed_at'], result['observed_at'])
        self.assertEqual(datetime.fromisoformat(manifest['valid_until']).timestamp(),
            datetime.fromisoformat(result['observed_at'].replace('Z','+00:00')).timestamp()+60)

    async def test_optional_finance_failure_cannot_poison_unrelated_successful_child(self):
        d = descriptor('fm_worker_finances')
        job = dict(self.job, descriptor=d, resource_key=d['resource_key'])
        self.worker.source_errors['owner'] = {'source_rejected_1_userteam_information'}
        self.worker.source_stamps['owner'] = ['2026-11-13T00:00:00Z']
        result = dict(value={'kind':'double','values':[1]},status='ok',completeness='complete')
        await self.worker.publish(job, 'owner', result, child=descriptor())
        self.assertEqual(self.store.snapshots[descriptor()['resource_key']]['status'], 'ok')
        await self.worker.publish(job, 'owner', result)
        manifest = self.store.snapshots[job['resource_key']]
        self.assertEqual(manifest['status'], 'partial')
        self.assertEqual(manifest['completeness'], 'partial')
        self.assertIn('source_rejected', manifest['error'])

    async def test_discovery_failure_does_not_count_as_completed_and_can_retry(self):
        result = dict(value={'kind':'double', 'values':[1]},
            discoveries=[dict(descriptor(), interval_seconds=300)])
        original = self.store.rpc
        async def fail(name, **params):
            if name == 'enqueue_batch': return None
            return await original(name, **params)
        self.store.rpc = fail
        with self.assertRaisesRegex(WorkerFailure, 'discovery_unacknowledged'):
            await self.worker.publish(self.job, 'owner', result)
        self.assertEqual(self.worker.job_count, 0)
        self.store.rpc = original
        await self.worker.publish(self.job, 'owner', result)
        self.assertEqual(self.worker.job_count, 1)
        self.assertFalse(self.requests)

    async def test_catalog_discoveries_are_bounded_acknowledged_batches(self):
        discoveries = []
        for i in range(2108):
            d = descriptor('get_player_summary', dict(player_id=str(i)))
            d['interval_seconds'] = 3600
            discoveries.append(d)
        original = copy.deepcopy(discoveries)
        await self.worker.enqueue_discoveries(self.job, 'owner', discoveries)
        calls = [p for name, p in self.store.calls if name == 'enqueue_batch']
        self.assertEqual(len(calls), 17)
        self.assertTrue(all(len(p['p_descriptors']) <= 128 for p in calls))
        self.assertEqual([d for p in calls for d in p['p_descriptors']], original)
        self.assertEqual(discoveries, original)
        self.assertFalse(self.requests)
        self.store.allowed = False
        with self.assertRaisesRegex(WorkerFailure, 'discovery_unacknowledged'):
            await self.worker.enqueue_discoveries(self.job, 'owner', discoveries)

    async def test_discovery_batches_respect_bytes_and_propagate_database_failure(self):
        d = descriptor(args=dict(padding='x' * 200000))
        await self.worker.enqueue_discoveries(self.job, 'owner', [d] * 3)
        batches = [p['p_descriptors'] for name, p in self.store.calls if name == 'enqueue_batch']
        self.assertEqual([len(b) for b in batches], [2, 1])
        original = self.store.rpc
        async def fail(name, **params):
            if name == 'enqueue_batch': raise WorkerFailure('database_connection')
            return await original(name, **params)
        self.store.rpc = fail
        with self.assertRaisesRegex(WorkerFailure, 'database_connection'):
            await self.worker.enqueue_discoveries(self.job, 'owner', [d])

    async def test_nested_listing_bids_preserve_roster_identity_and_durable_reuse(self):
        player = dict(id='000123', name='Fixture player', role='defensa', teamId='club',
            value=1000, change=0, points=-2, price=1200,
            average=dict(average=0, matches=2, fitness=[0, -2]),
            bids=[dict(id='offer', price=700, userteam=dict(id='rival', name='Rival'))])
        routes = {
            '/1/userteam/roster': [player, dict(player, id='other-player')],
            '/1/league/championshipteams': [], '/1/market/rosterbids': [],
            '/1/market/myplayers': [player]}
        paths = []
        def transport(request):
            paths.append(request.url.path)
            return httpx.Response(200, json={'answer': routes[request.url.path]})
        self.client._transport = httpx.MockTransport(transport)
        d = descriptor('get_players_from_team', dict(championship_id='l', user_team_id='t'))
        job = dict(self.job, descriptor=d, resource_key=d['resource_key'], interval_seconds=60)
        result = await self.worker.helpers[0].run(d, lambda m: self.worker.exchange(job, 'owner', m))
        rows = plain(result['value'])
        self.assertEqual([r['id'] for r in rows], ['000123', 'other-player'])
        self.assertEqual(rows[0]['market_asking_price'], 1200)
        self.assertEqual(rows[0]['points'], -2)
        self.assertEqual([r['market_inMarket'] for r in rows], [True, False])
        await self.worker.publish(job, 'owner', result)
        manifest = self.store.snapshots[job['resource_key']]
        self.assertEqual(plain(unpack_snapshot(manifest, self.store.chunks[manifest['version']])), rows)
        # A fresh helper/process reuses the same durable source checkpoints.
        restarted = Worker(self.store, self.client, KEY, Path.cwd())
        restarted.admission_blocked = False
        try:
            again = await restarted.helpers[0].run(d, lambda m: restarted.exchange(job, 'restarted', m))
            self.assertEqual(plain(again['value']), rows)
            self.assertEqual(len(paths), 4)
        finally:
            await asyncio.gather(*(h.close() for h in restarted.helpers))

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

    async def test_real_market_frame_persists_and_reuses_checkpoint(self):
        d = descriptor('get_market_players', dict(championship_id='l', user_team_id='t'))
        d['context'] = dict(CONTEXT, user_team_id='t')
        d['resource_key'] = hashlib.sha256(canonical({k:v for k,v in d.items() if k != 'resource_key'}).encode()).hexdigest()
        job = dict(self.job, descriptor=d, resource_key=d['resource_key'])
        async def transport(request):
            self.requests.append(request)
            return httpx.Response(200, json={'answer': [{'id':'p','name':'Player','slug':'player',
                                  'value':100,'change':1,'points':-2}]})
        self.client._transport = httpx.MockTransport(transport)
        result = await self.worker.helpers[0].run(d, lambda m: self.worker.exchange(job, 'owner', m))
        self.assertEqual(plain(result['value'])[0]['points'], -2)
        await self.worker.publish(job, 'owner', result)
        manifest = self.store.snapshots[d['resource_key']]
        restored = unpack_snapshot(manifest, self.store.chunks[manifest['version']])
        self.assertEqual(plain(restored)[0]['id'], 'p')
        self.assertEqual(len(self.requests), 1)
        await self.worker.helpers[0].close()
        replay = await self.worker.helpers[0].run(d, lambda m: self.worker.exchange(job, 'owner', m))
        self.assertEqual(plain(replay['value'])[0]['points'], -2)
        self.assertEqual(len(self.requests), 1)

    async def test_replayed_response_does_not_renew_R_observation_time(self):
        async def old_response(message):
            if message['type']=='http':
                return dict(status=200,body={'answer':[{'id':'club','name':'Club','logo':'logo'}]},observed_at='2020-01-01T00:00:00Z')
            if message['type']=='database':return {'rows':[]}
            if message['type']=='resource':return {'value':None}
            return {'ok':True}
        result=await self.worker.helpers[0].run(self.job['descriptor'],old_response)
        self.assertTrue(result['observed_at'].startswith('2020-01-01'),result['observed_at'])

    async def test_finance_progress_is_durable_partial_and_rate_limited(self):
        d = descriptor('fm_worker_finances')
        job = dict(self.job, descriptor=d, resource_key=d['resource_key'])
        message = dict(type='progress',result=dict(value={'kind':'double','values':[90]},
                       observed_at='2026-11-11T12:00:00Z',checked_at='2026-11-11T12:00:00Z',
                       status='ok',completeness='complete'))
        result = await self.worker.exchange(job,'owner',message)
        self.assertTrue(result['published'])
        manifest = self.store.snapshots[job['resource_key']]
        self.assertEqual((manifest['status'],manifest['completeness']),('partial','partial'))
        self.assertEqual(plain(unpack_snapshot(manifest,self.store.chunks[manifest['version']])),90)
        again = await self.worker.exchange(job,'owner',message)
        self.assertFalse(again['published'])
        self.assertEqual(sum(name=='publish_child' for name,_ in self.store.calls),1)
        message['result']['ready'] = True
        self.assertTrue((await self.worker.exchange(job,'owner',message))['published'])
        self.assertFalse((await self.worker.exchange(job,'owner',message))['published'])
        self.assertEqual(sum(name=='publish_child' for name,_ in self.store.calls),2)
        self.assertFalse(any(name=='publish' for name,_ in self.store.calls))
        self.assertEqual(self.worker.job_count,0)
        with self.assertRaisesRegex(WorkerFailure,'helper_protocol'):
            await self.worker.exchange(self.job,'other',message)

    async def test_finance_progress_ack_failure_remains_retryable(self):
        d = descriptor('fm_worker_finances')
        job = dict(self.job, descriptor=d, resource_key=d['resource_key'])
        original = self.store.rpc
        async def failing(name,**params):
            if name=='publish_child':raise WorkerFailure('database_connection')
            return await original(name,**params)
        self.store.rpc = failing
        message = dict(type='progress',result=dict(value={'kind':'double','values':[90]},
                       observed_at='2026-11-11T12:00:00Z',checked_at='2026-11-11T12:00:00Z'))
        with self.assertRaisesRegex(WorkerFailure,'database_connection'):
            await self.worker.exchange(job,'owner',message)
        self.assertNotIn('owner',self.worker.progress_times)
        self.assertFalse(self.store.snapshots)
        self.store.rpc = original
        self.assertTrue((await self.worker.exchange(job,'owner',message))['published'])

    async def test_helper_acknowledges_progress_then_waits_for_final_result(self):
        helper=Helper(Path.cwd())
        progress={'type':'progress','result':{'value':{'kind':'double','values':[90]}}}
        final={'type':'result','value':{'kind':'double','values':[90]}}
        helper.process=SimpleNamespace(returncode=None,
            stdout=SimpleNamespace(readline=AsyncMock(side_effect=[(canonical(progress)+'\n').encode(),(canonical(final)+'\n').encode()])),
            stdin=SimpleNamespace(write=lambda _:None,drain=AsyncMock()))
        exchange=AsyncMock(return_value={'ok':True})
        self.assertEqual(await helper.run(descriptor('fm_worker_finances'),exchange),final)
        exchange.assert_awaited_once_with(progress)

    async def test_timeout_diagnostics_are_strictly_allowlisted(self):
        valid = 'finance_timeout_s1_b2_q3_o1_t10_tsready_saved0_seed0_team0_lineup1_ranking0_notifications0_ownership1_ownership_catalog0'
        for code, expected in ((valid, valid), ('finance_timeout', 'finance_timeout'),
                (valid + '_private', 'resource_parse'), (valid.replace('tsready', 'tsprivate'), 'resource_parse'),
                (valid.replace('_b2_', '_b10000_'), 'resource_parse'), (None, 'resource_parse'),
                ('private exception body', 'resource_parse')):
            helper = Helper(Path.cwd())
            helper.process = SimpleNamespace(returncode=None,
                stdout=SimpleNamespace(readline=AsyncMock(return_value=(canonical(dict(type='failure',code=code))+'\n').encode())),
                stdin=SimpleNamespace(write=lambda _:None,drain=AsyncMock()))
            with self.assertRaises(WorkerFailure) as caught:
                await helper.run(descriptor('fm_worker_finances'), AsyncMock())
            self.assertEqual(caught.exception.code, expected)

    async def test_failed_fact_ack_is_never_success(self):
        self.store.allowed=False
        with self.assertRaises(WorkerFailure):
            await self.worker.exchange(self.job,'owner',dict(type='persist',table='players',payload={'id':'p','name':'Player'},conflict='id'))

    async def test_classification_recovers_cache_error_and_persists_source(self):
        d=descriptor('get_classification_round_ranking',dict(championship_id='l',user_team_id='t',round_id='r1',round_number=1))
        d['context']=dict(CONTEXT,user_team_id='t')
        job=dict(self.job,descriptor=d,resource_key=d['resource_key'])
        async def transport(request):
            self.requests.append(request)
            self.assertEqual(json.loads(request.content)['query']['roundNumber'],'r1')
            return httpx.Response(200,json={'answer':{'metadata':{'status':'closed'},
                'ranking':[{'id':'t','name':'Fixture','points':-2,'live':0}]}})
        self.client._transport=httpx.MockTransport(transport)
        async def exchange(message):
            if message['type']=='database':raise WorkerFailure('database_400')
            return await self.worker.exchange(job,'owner',message)
        result=await self.worker.helpers[0].run(d,exchange)
        self.assertEqual(plain(result['value'])['rows'][0]['points'],-2)
        await self.worker.publish(job,'owner',result)
        manifest=self.store.snapshots[d['resource_key']]
        self.assertEqual(plain(unpack_snapshot(manifest,self.store.chunks[manifest['version']]))['rows'][0]['points'],-2)
        await self.worker.helpers[0].close()
        replay=await self.worker.helpers[0].run(d,exchange)
        self.assertEqual(plain(replay['value'])['rows'][0]['points'],-2)
        self.assertEqual(len(self.requests),1)

    async def test_source_failure_codes_are_strictly_allowlisted(self):
        for code,expected in (('resource_parse_http_database_400','resource_parse_http_database_400'),
                ('resource_parse_schema_roster_bids','resource_parse_schema_roster_bids'),
                ('resource_parse_codec_class','resource_parse_codec_class'),
                ('resource_parse_http_private_account_400','resource_parse'),
                ('resource_parse_http_database_400_private','resource_parse')):
            helper=Helper(Path.cwd())
            helper.process=SimpleNamespace(returncode=None,
                stdout=SimpleNamespace(readline=AsyncMock(return_value=(canonical(dict(type='failure',code=code))+'\n').encode())),
                stdin=SimpleNamespace(write=lambda _:None,drain=AsyncMock()))
            with self.assertRaises(WorkerFailure) as caught:await helper.run(descriptor(),AsyncMock())
            self.assertEqual(caught.exception.code,expected)

    async def test_native_error_is_not_checkpointed(self):
        async def rejected(request):
            self.requests.append(request)
            return httpx.Response(200,json={'answer':{'error':'api.error.general'}})
        self.client._transport=httpx.MockTransport(rejected)
        with self.assertRaisesRegex(WorkerFailure,'source_rejected'):
            await self.worker.source(self.job,'owner',self.message())
        self.assertFalse(self.store.checkpoints)
        self.assertFalse(self.worker.source_stamps.get('owner'))

    async def test_legacy_native_error_checkpoint_reacquires(self):
        message=self.message()
        request_key=hashlib.sha256(canonical(dict(path='/1/league/championshipteams',body=message['body'])).encode()).hexdigest()
        old=dict(status=200,body={'answer':{'error':'api.error.general'}},observed_at='2001-01-01T00:00:00+00:00')
        parts,size,digest=pack_snapshot(old)
        self.store.checkpoints[request_key]={'manifest':dict(parts=len(parts),json_bytes=size,sha256=digest),'chunks':parts}
        result=await self.worker.source(self.job,'owner',message)
        self.assertEqual(result['body']['answer'][0]['id'],'club')
        self.assertNotEqual(result['observed_at'],old['observed_at'])
        self.assertEqual(len(self.requests),1)
        self.assertNotIn(old['observed_at'],self.worker.source_stamps['owner'])

    async def test_context_and_native_failures_reach_worker_owner(self):
        for code in ('context_superseded','league_membership_unverified','source_context','source_rejected','credential_in_fact'):
            helper=Helper(Path.cwd())
            message=dict(type='http',url='https://api.futmondo.com/1/userteam/roster',body={})
            helper.process=SimpleNamespace(returncode=None,
                stdout=SimpleNamespace(readline=AsyncMock(return_value=(canonical(message)+'\n').encode())),
                stdin=SimpleNamespace(write=lambda _:None,drain=AsyncMock()))
            with self.assertRaises(WorkerFailure) as caught:
                await helper.run(descriptor(),AsyncMock(side_effect=WorkerFailure(code)))
            self.assertEqual(caught.exception.code,code)

if __name__=='__main__':unittest.main(verbosity=2)
