"""Offline StrikerAPI contract, identity and request-budget checks."""
import json
from datetime import datetime, timezone
import os
import subprocess
from pathlib import Path
from unittest.mock import AsyncMock, patch
import time
import unittest
import httpx

from worker.main import Helper, plain, pack_snapshot, unpack_snapshot
from worker.striker import Striker, identify

LALIGA = "504e4f584d8bec9a67000079"


def player(team, number):
    return dict(player_id=str(team*100+number),name=f'Ángel García {number}',jersey=str(number),
                date_of_birth='1999-01-01')


class ClubSheets(unittest.IsolatedAsyncioTestCase):
    async def asyncSetUp(self):
        self.fixture = dict(unix=int(time.time())+1800,home_id='home',away_id='away',
                            home_name='Atlético de Madrid',away_name='Athletic Club')
        self.game = dict(game_id='42',season=2026,starts_at=self.start(),home=dict(team_id='1',name='Atlético Madrid'),
                         away=dict(team_id='2',name='Athletic Club Bilbao'))
        self.sides = [dict(team_id=str(t),starting_xi=[player(t,i) for i in range(1,12)],
                           unused_substitutes=[player(t,12)],substitutes_used=[]) for t in (1,2)]
        self.local = [dict(id=str(i),teamId='home',name=f'Angel Garcia {i}',number=i) for i in (1,12,13)]
        self.local.append(dict(id='unknown',teamId='home',name='unmapped'))
        self.calls = []
        self.code = 200
        self.headers = {}
        self.expected_league = "laliga"
        self.squad_outage = False
        self.client = httpx.AsyncClient(transport=httpx.MockTransport(self.serve))
        self.provider = self.service()

    def start(self):
        return datetime.fromtimestamp(self.fixture['unix'],timezone.utc).isoformat()

    async def asyncTearDown(self):
        await self.client.aclose()

    def service(self):
        store = AsyncMock()
        store.rpc.return_value = 'synthetic-reservation'
        return Striker(self.client,'fixture-key',store)

    def serve(self,req):
        self.assertEqual(req.method,'POST')
        self.assertEqual(req.headers['x-api-key'],'fixture-key')
        self.assertNotIn('fixture-key',str(req.url))
        endpoint = req.url.path.split('/')[-1]
        args = json.loads(req.content)
        self.assertEqual(args['league'],self.expected_league)
        self.calls.append(endpoint)
        if endpoint=='list_games':
            body = dict(games=[self.game])
        elif endpoint=='get_lineups':
            self.assertEqual(args['game_id'],'42')
            body = dict(game=self.game,lineups=self.sides,source='live')
        else:
            self.assertEqual(endpoint,'get_squad')
            tid = args['team']
            if tid=='2' and self.squad_outage:
                return httpx.Response(503)
            body = dict(team_id=tid,season=2026,count=13,players=[player(int(tid),i) for i in range(1,14)])
        return httpx.Response(self.code,json=dict(data=body), headers=self.headers)

    async def check(self, clubs=None):
        return await self.provider.check(LALIGA,self.fixture,self.local,clubs or {})

    async def test_monthly_limit_is_checked_only_on_cache_misses(self):
        self.provider.store.rpc.return_value = None
        result = await self.check()
        self.assertEqual(result['status'], 'monthly_limit')
        self.assertEqual(self.calls, [])
        self.provider.store.rpc.return_value = 'reservation'
        await self.check()
        self.assertEqual(len(self.calls), 4)
        self.provider.store.rpc.reset_mock()
        self.provider.store.rpc.return_value = None
        await self.check()
        self.provider.store.rpc.assert_not_called()
        self.assertEqual(len(self.calls), 4)

    async def test_refund_retry_keeps_fetched_result(self):
        self.headers = {'X-Credits-Used': '0'}
        self.provider.store.rpc.side_effect = ['permit', RuntimeError('store offline')]
        games, _ = await self.provider.read('laliga','list_games', {'date':'2026-11-26'}, 1800)
        self.assertEqual(games['games'], [self.game])
        self.assertEqual(self.provider.pending_settlement, ('permit', 0))
        self.provider.store.rpc.side_effect = [None, None]
        result = await self.check()
        self.assertEqual(result['status'], 'monthly_limit')
        self.assertIn((('settle_striker_call',), {'p_id':'permit','p_used':0}),
                      self.provider.store.rpc.await_args_list)
        self.assertIsNone(self.provider.pending_settlement)
        self.assertEqual(self.calls, ['list_games'])

    async def test_competition_routing_and_cache_isolation(self):
        for competition, league, teams in [
                (LALIGA, 'laliga', ('Valencia', 'Getafe')),
                ('51c96d15e67621cf7000000d', 'soccer', ('Arsenal', 'Chelsea')),
                ('51ad1f8f52c92e260f00000a', 'seriea', ('Napoli', 'Roma'))]:
            self.expected_league = league
            for side, name in zip(('home', 'away'), teams):
                self.fixture[side + '_name'] = name
                self.game[side]['name'] = name
            self.calls.clear()
            result = await self.provider.check(competition, self.fixture, self.local, {})
            self.assertEqual(result['status'], 'confirmed')
            self.assertEqual(self.calls, ['list_games', 'get_lineups', 'get_squad', 'get_squad'])
            await self.provider.check(competition, self.fixture, self.local, {})
            self.assertEqual(len(self.calls), 4)
        self.provider.store.rpc.reset_mock()
        self.assertEqual((await self.provider.check('unknown', self.fixture, self.local, {}))['status'], 'unsupported')
        self.provider.store.rpc.assert_not_called()
        self.assertEqual(len(self.calls), 4)

    async def test_sheet_restart_and_reuse(self):
        result = await self.check()
        self.assertEqual(result['players'],{'1':'starting','12':'benched','13':'absent'})
        self.assertEqual(result['unresolved'],['unknown'])
        self.assertEqual(len(self.calls),4)
        self.assertEqual(result,await self.check())
        self.assertEqual(len(self.calls),4)
        self.provider = self.service()
        self.fixture.update(fixture_id='42',evidence=json.loads(json.dumps(result['evidence'])))
        self.local[1]['name']='changed display name'
        replay = await self.check(result['clubs'])
        self.assertEqual(replay['players'],result['players'])
        self.assertEqual(len(self.calls),4)
        with patch('worker.striker.time.time',return_value=self.fixture['unix']-500):
            self.assertEqual((await self.check(result['clubs']))['players']['12'],'benched')
        self.assertEqual(len(self.calls),4)

    async def test_partial_squad_reused_on_manual_retry(self):
        self.squad_outage = True
        first = await self.check()
        self.assertEqual(first['status'], 'unavailable')
        self.assertEqual(set(first['clubs']), {'home'})
        self.fixture.update(evidence=first['evidence'], fixture_id=first['fixture_id'])
        self.provider = self.service()
        self.squad_outage = False
        self.calls.clear()
        result = await self.check(first['clubs'])
        self.assertEqual(result['players']['13'], 'absent')
        self.assertEqual(self.calls, ['get_squad'])

    async def test_confirmation_identity_and_failures(self):
        self.sides, original = [], self.sides
        self.assertEqual((await self.check())['status'],'pending')
        self.sides=original
        self.provider=self.service()
        self.sides[0]['starting_xi'].pop()
        self.assertEqual((await self.check())['status'],'incomplete')
        self.fixture['unix']+=3600
        self.assertEqual((await self.check())['status'],'fixture_unmatched')
        for code,status in ((402,'credits_exhausted'),(403,'access_denied'),(429,'rate_limited')):
            self.provider=self.service()
            self.code=code
            self.assertEqual((await self.check())['status'],status)
            count=len(self.calls)
            self.assertEqual((await self.check())['status'],status)
            self.assertEqual(len(self.calls),count)
        self.provider.key=''
        self.assertEqual((await self.check())['status'],'not_configured')
        self.assertEqual(len(self.calls),count)

    async def test_real_helper_and_snapshot_replay(self):
        wire = json.loads(subprocess.check_output(['Rscript','test/test_club_xi.R','--wire']))
        descriptor = wire['descriptor']
        latest = None
        async def exchange(message):
            if message['type'] == 'club_xi_request':
                return dict(request=wire['request'] if latest is None else None)
            if message['type'] == 'discovery_resource':
                handler = message['descriptor']['handler']
                self.assertIn(handler, ('get_club_lineups','get_championship_players','get_finished_rounds'))
                value = {'get_club_lineups':latest,'get_finished_rounds':wire['rounds'],
                         'get_championship_players':wire['catalog']}[handler]
                return dict(discovery_only=True,value=value)
            self.assertEqual(message['type'],'club_xi')
            fixture = message['fixture']
            self.fixture['unix'] = fixture['unix']
            self.game['starts_at'] = self.start()
            return await self.provider.check(LALIGA,fixture,message['players'],message['clubs'] or {})
        with patch.dict(os.environ, STRIKER_API_KEY='synthetic'):
            helper = Helper(Path.cwd())
            try:
                first = await helper.run(descriptor, exchange)
                self.assertEqual(first['status'],'ok')
                chunks,size,digest = pack_snapshot(first['value'])
                latest = unpack_snapshot(dict(parts=len(chunks),json_bytes=size,sha256=digest),chunks)
                verdicts = plain(latest)['club_xi']['matches']['fixture']['players']
                self.assertEqual(verdicts,dict(f1='starting',f12='benched',f13='absent',a1='starting'))
                self.assertEqual(len(self.calls),4)
                replay = await helper.run(descriptor, exchange)
                self.assertEqual(plain(replay['value'])['club_xi']['matches']['fixture']['players'],verdicts)
                self.assertEqual(len(self.calls),4)
            finally:
                await helper.close()

    def test_collisions_and_prior_links(self):
        pool={'1':dict(id='1',name='alberto garcia perez',dob='2000-01-01',number='4'),
              '2':dict(id='2',name='alberto garcia perez',dob='2001-02-02',number='8')}
        for name in ('García','Alberto','Alberto García Pérez','Alber Garcia'):
            self.assertIsNone(identify(dict(name=name),pool,None))
        self.assertEqual(identify(dict(name='García',number=8),pool,None),'2')
        self.assertEqual(identify(dict(name='Alberto García Pérez',dob='2001-02-02',number=4),pool,None),'2')
        self.assertIsNone(identify(dict(name='García',dob='2001-02-02'),pool,'1'))
        self.assertIsNone(identify(dict(name='García'),pool,'3'))
        self.assertIsNone(identify(dict(name='García'),{'1':pool['1']},None))

if __name__ == '__main__':
    unittest.main()
