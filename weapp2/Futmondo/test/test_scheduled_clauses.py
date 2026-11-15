"""Synthetic purchase dispatch tests. No real credentials or external requests."""
import asyncio
import copy
import hashlib
import json
import time
import unittest
from pathlib import Path
from unittest.mock import AsyncMock, patch

import httpx

from worker.main import Worker, encrypt_access, stamp, canonical, READ_PATHS
from worker.clauses import ClauseScheduler, CLOSED_CLAUSE_CODES, epoch

KEY='ab'*32
CTX=dict(account_id='u',championship_id='l',user_team_id='t',player_id='p',competition_id='c',season='S',scoring_version='v')
AUTH=dict(userid='u',token='synthetic',user_name='fixture@example.invalid')

def job():
    return dict(id='a'*32,account_id='u',context=copy.deepcopy(CTX),kind='scheduled',mode='clause',state='preparing',
                player_id='p',player_slug='fixture-player',player_name='Fixture player',original_owner='rival',max_price=100,
                opens_at=stamp(time.time()-1),expires_at=stamp(time.time()+299),attempts=0,result_code='')

def facts():
    return dict(context=copy.deepcopy(CTX),status='ok',player=dict(id='p',owner_id='rival',price=90,transferred=False,
                opens_at=stamp(time.time()-1)),valid_until=stamp(time.time()+14),offers=[],transactions=[],owned_ids=[],
                roster=dict(status='ok',effective_count=9,cap=12,pending_inbound_ids=[],pending_outbound_ids=[]))

class Store:
    def __init__(self):
        self.calls=[];self.current=job();self.allow=True;self.evidence=[];self.evidence_by_key={}
        self.authorization=dict(account_id='u',enabled=True,auth_status='active',credential_id='1',
            encrypted_token=encrypt_access(AUTH,'u','1',KEY))
    async def request(self,method,path,**kw):
        self.calls.append((method,path,copy.deepcopy(kw)))
        if path=='clause_authorizations':return [self.authorization]
        if path=='clause_evidence':
            key=kw.get('params',{}).get('evidence_key','')[3:]
            return self.evidence or ([self.evidence_by_key[key]] if key in self.evidence_by_key and time.time()<__import__('worker.clauses',fromlist=['epoch']).epoch(self.evidence_by_key[key]['valid_until']) else [])
        if path=='clause_jobs':return [self.current]
        p=kw.get('json',{})
        if path=='rpc/clause_save_evidence':
            self.evidence_by_key[p['p_key']]=dict(context=self.current['context'],payload=p['p_payload'],observed_at=p['p_observed'],valid_until=p['p_until'])
            return True
        if path=='rpc/clause_dispatch':
            if not self.allow or self.current['state']!='preparing':return False
            self.current['state']='dispatched';return True
        if path=='rpc/clause_settle':
            if self.current['state']=='dispatched' and p['p_state']=='scheduled' and not p['p_code'].startswith('closed_verified:'):return False
            self.current['state']=p['p_state'];self.current['result_code']=p['p_code'];return True
        if path=='rpc/clause_claim':return None
        if path=='rpc/clause_reconcile_reservations':return None
        return self.allow
    async def rpc(self,name,**kw):return {'resources':[]}

class PurchaseTests(unittest.IsolatedAsyncioTestCase):
    async def asyncSetUp(self):
        self.requests=[]
        def transport(request):
            self.requests.append(request)
            return httpx.Response(200,json={'answer':{'code':'api.general.ok'}})
        self.client=httpx.AsyncClient(transport=httpx.MockTransport(transport))
        self.store=Store();self.worker=Worker(self.store,self.client,KEY,Path.cwd())
        self.worker.leader=True;self.worker.admission_blocked=False;self.worker.request_interval=0
        self.scheduler=ClauseScheduler(self.worker,Path.cwd());self.scheduler.check=AsyncMock(return_value=facts())
    async def asyncTearDown(self):
        await self.scheduler.helper.close();await self.client.aclose()
    def settlements(self):
        return [kw['json'] for _,path,kw in self.store.calls if path=='rpc/clause_settle']
    async def test_dedicated_payload_and_lower_price(self):
        await self.scheduler.execute(job(),'leader:clause')
        self.assertEqual(len(self.requests),1)
        self.assertEqual(self.requests[0].url.path,'/1/market/rosterclause')
        body=json.loads(self.requests[0].content)
        self.assertEqual(body['answer'],{})
        self.assertEqual(body['query'],dict(championshipId='l',userteamId='t',player_slug='fixture-player',player_id='p',price=90))
        self.assertEqual(self.store.current['state'],'accepted')
        self.assertNotIn('/1/market/rosterclause',READ_PATHS)
    async def test_response_loss_is_never_automatically_retried(self):
        def lost(request):
            self.requests.append(request);raise httpx.ReadTimeout('synthetic lost acknowledgement')
        self.client._transport=httpx.MockTransport(lost)
        await self.scheduler.execute(job(),'leader:clause')
        self.assertEqual(self.store.current['state'],'uncertain')
        resumed=copy.deepcopy(job());resumed['state']='uncertain';resumed['result_code']='purchase_response_uncertain'
        await self.scheduler.execute(resumed,'leader:restart')
        self.assertEqual(len(self.requests),1)
        self.assertEqual(self.store.current['state'],'uncertain')
    async def test_unknown_closed_looking_code_is_terminal_without_retry(self):
        self.assertEqual(CLOSED_CLAUSE_CODES,frozenset())
        self.client._transport=httpx.MockTransport(lambda r:httpx.Response(200,json={'answer':{'code':'api.fixture.clause_time'}}))
        await self.scheduler.execute(job(),'leader:clause')
        self.assertEqual(self.store.current['state'],'uncertain')
        self.assertEqual(self.store.current['result_code'],'api.fixture.clause_time')
        self.assertFalse(any(x['p_state']=='scheduled' for x in self.settlements()))
    async def test_verified_rejection_retry_and_expiry(self):
        self.client._transport=httpx.MockTransport(lambda r:httpx.Response(200,json={'answer':{'code':'api.fixture.closed'}}))
        with patch('worker.clauses.CLOSED_CLAUSE_CODES',frozenset({'api.fixture.closed'})):
            await self.scheduler.execute(job(),'leader:clause')
        self.assertEqual(self.store.current['state'],'scheduled')
        self.assertEqual(self.settlements()[-1]['p_code'],'closed_verified:api.fixture.closed')
        self.assertIsNotNone(self.settlements()[-1]['p_due'])
        expired=job();expired['expires_at']=stamp(time.time()-1)
        await self.scheduler.execute(expired,'leader:clause')
        self.assertEqual(self.settlements()[-1]['p_state'],'expired')
    async def test_new_owner_and_pending_transfer_cancel_before_http(self):
        for change in (dict(owner_id='another-rival'),dict(transferred=True)):
            self.store.current=job()
            f=facts();f['player'].update(change);self.scheduler.check.return_value=f
            await self.scheduler.execute(job(),'leader:clause')
            self.assertEqual(self.settlements()[-1]['p_state'],'cancelled')
        self.assertEqual(self.requests,[])
    async def test_changed_price_and_extended_lock(self):
        f=facts();f['player']['price']=101;self.scheduler.check.return_value=f
        await self.scheduler.execute(job(),'leader:clause')
        self.assertEqual(self.settlements()[-1]['p_code'],'price_exceeds_authorized_cap')
        self.store.current=job()
        f=facts();f['player']['opens_at']='2066-11-15T12:00:00+00:00';self.scheduler.check.return_value=f
        await self.scheduler.execute(job(),'leader:clause')
        self.assertEqual(self.settlements()[-1]['p_state'],'scheduled')
        self.assertEqual(self.settlements()[-1]['p_open'],'2066-11-15T12:00:00+00:00')
        self.assertEqual(self.requests,[])
    async def test_revocation_and_failed_dispatch_prevent_http(self):
        self.store.allow=False
        with self.assertRaises(Exception):await self.scheduler.execute(job(),'leader:clause')
        self.assertEqual(self.requests,[])
        self.store.allow=True;self.store.authorization['enabled']=False
        with self.assertRaises(Exception):await self.scheduler.execute(job(),'leader:clause')
        self.assertEqual(self.requests,[])
    async def test_source_reads_persist_and_reuse_original_observation_time(self):
        self.requests=[]
        self.client._transport=httpx.MockTransport(lambda r:self.requests.append(r) or httpx.Response(200,json={'answer':{'championship':{'clause':{'price':100}}}}))
        msg=dict(type='http',url='https://api.futmondo.com/1/player/summary',body=dict(query=dict(championshipId='l',userteamId='t',playerId='p'),answer={}))
        first=await self.scheduler.read_source(job(),'leader:clause',msg)
        saved=[kw['json'] for _,path,kw in self.store.calls if path=='rpc/clause_save_evidence'][-1]
        self.assertNotIn('synthetic',canonical(saved));self.assertNotIn('token',canonical(saved))
        self.store.evidence=[dict(context=CTX,payload=first,observed_at=first['observed_at'])]
        second=await self.scheduler.read_source(job(),'leader:clause',msg)
        self.assertEqual(first,second);self.assertEqual(len(self.requests),1)
    async def test_delayed_source_does_not_block_event_loop(self):
        ticks=0
        async def held(request):
            await asyncio.sleep(.12);return httpx.Response(200,json={'answer':[]})
        self.client._transport=httpx.MockTransport(held)
        async def heartbeat():
            nonlocal ticks
            for _ in range(10):await asyncio.sleep(.01);ticks+=1
        msg=dict(type='http',url='https://api.futmondo.com/1/market/players',body=dict(query=dict(championshipId='l'),answer={}))
        await asyncio.gather(self.scheduler.read_source(job(),'leader:clause',msg),heartbeat())
        self.assertEqual(ticks,10)
    async def test_full_headless_preflight_uses_real_r_parsers_and_durable_reuse(self):
        self.scheduler=ClauseScheduler(self.worker,Path.cwd())
        current=job();current['context']['scoring_version']=hashlib.sha256(canonical(dict(custom=None,point_system='stats')).encode()).hexdigest()
        self.store.current=current
        active=dict(championships=[dict(id='l',league='c',ps='stats',userteam=dict(id='t'))],leagues=[dict(_id='c',season='S')])
        native_summary=dict(data=dict(id='p',slug='fixture-player',name='Fixture'),points=[],prices=[],
            championship=dict(owner=dict(_id='rival'),clause=dict(price=100,transferred=False,date='2066-11-15T12:00:00Z')))
        routes={'/2/user/activechampionships':active,'/1/player/summary':native_summary,
            '/1/userteam/information':dict(budget=1000,withheld=0,teamValue=1000,maxBid=1200,
                configuration=dict(maxPlayersInRoster=20,mnmp=.2)),
            '/1/userteam/roster':[dict(id='own',value=1000)],'/1/market/players':[],
            '/1/locker/pressroom':dict(news=[]),'/2/locker/news':dict(news=[])}
        paths=[]
        def source(request):
            paths.append(request.url.path)
            self.assertIn(request.url.path,routes)
            return httpx.Response(200,json={'answer':routes[request.url.path]})
        self.client._transport=httpx.MockTransport(source)
        started=time.perf_counter()
        result=await self.scheduler.check(current,'leader:clause')
        self.assertEqual(result['status'],'ok')
        self.assertEqual(result['player']['id'],'p')
        self.assertEqual(result['player']['owner_id'],'rival')
        self.assertEqual(result['funds']['spendable_budget'],1200)
        self.assertEqual(result['owned_ids'],['own'])
        self.assertEqual(result['roster']['pending_inbound_ids'],[])
        self.assertEqual(result['offers'],[])
        first=len(paths)
        elapsed=time.perf_counter()-started
        status=Path('/proc/%d/status'%self.scheduler.helper.process.pid).read_text()
        rss=next(line.split()[1] for line in status.splitlines() if line.startswith('VmRSS:'))
        print('MEASURE cold_helper_preflight_ms=%.1f helper_rss_kib=%s'%(elapsed*1000,rss))
        # Close the actual R process, then restore facts from persisted evidence.
        await self.scheduler.helper.close()
        restored=await self.scheduler.check(current,'leader:clause')
        self.assertEqual(restored['player'],result['player'])
        self.assertEqual(len(paths),first)
        print('MEASURE headless_preflight_requests=%d restart_extra_source_requests=%d'%(first,len(paths)-first))
        # A valid older membership observation must not backdate fresh cash,
        # roster or clause fields in the headless composite.
        await self.scheduler.helper.close();self.store.evidence_by_key.clear()
        old=time.time()-25
        def wire(value):
            if value is None:return dict(kind='null')
            if isinstance(value,dict):return dict(kind='list',names=list(value),values=[wire(x) for x in value.values()],meta={})
            if isinstance(value,list):return dict(kind='list',names=None,values=[wire(x) for x in value],meta={})
            return dict(kind='character',values=[value],names=None,meta={})
        saved=wire(active)
        saved['meta']=dict(observed_at=dict(kind='double',values=[old],names=None,time='datetime',meta={}),fetch_status=wire('ok'))
        original=self.scheduler.exchange
        async def older_membership(job,owner,message):
            if message['type']=='resource' and message['descriptor']['handler']=='get_active_championships':
                return dict(value=saved,observed_at=stamp(old))
            return await original(job,owner,message)
        self.scheduler.exchange=older_membership
        fresh=await self.scheduler.check(current,'leader:clause')
        self.assertLess(time.time()-epoch(fresh['financial_observed_at']),5)
        self.assertGreater(epoch(fresh['valid_until']),time.time())
        print('PASS older membership evidence preserves original source time without aging fresh financial checks')


    async def test_prepared_purchase_waits_until_opening_and_cancellation_stops_it(self):
        f=facts();opening=time.time()+.04;f['player']['opens_at']=stamp(opening)
        self.scheduler.check.return_value=f
        await self.scheduler.execute(job(),'leader:clause')
        self.assertGreaterEqual(time.time(),opening)
        self.assertEqual(len(self.requests),1)
        self.store.current=job();self.requests=[];f=facts();f['player']['opens_at']=stamp(time.time()+.08)
        self.scheduler.check.return_value=f
        running=asyncio.create_task(self.scheduler.execute(job(),'leader:clause'))
        await asyncio.sleep(.02);self.store.allow=False
        with self.assertRaises(Exception):await running
        self.assertEqual(self.requests,[])

    async def test_verified_membership_or_season_change_is_distinct_from_missing_data(self):
        for answer,code in [(dict(championships=[],leagues=[]),'purchase_membership_lost'),
            (dict(championships=[dict(id='l',league='c',userteam=dict(id='new-team'))],leagues=[]),'purchase_context_superseded'),
            (dict(championships=[dict(id='l',league='c',userteam=dict(id='t'))],leagues=[dict(_id='c',season='new-season')]),'purchase_context_superseded')]:
            with self.assertRaisesRegex(Exception,code):self.scheduler.validate_membership(job(),dict(answer=answer))

    async def test_scheduler_idle_is_one_global_lookup_not_per_account_or_player(self):
        task=asyncio.create_task(self.scheduler.run())
        await asyncio.sleep(.05);self.worker.stop.set();await task
        self.assertEqual(sum(path=='rpc/clause_claim' for _,path,_ in self.store.calls),1)
        self.assertEqual(self.requests,[])

if __name__=='__main__':unittest.main()
