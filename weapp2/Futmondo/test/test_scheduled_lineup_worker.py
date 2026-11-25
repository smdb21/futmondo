"""Run the real Python scheduler and R source parsers against synthetic HTTP."""
import asyncio
import copy
import hashlib
import json
from collections import Counter
from pathlib import Path
from types import SimpleNamespace

import httpx

from worker.lineups import LineupScheduler
from worker.main import encrypt_access, stamp


class Store:
    def __init__(self, scoring=None):
        self.snapshots = []
        self.evidence = {}
        self.transitions = []
        self.scoring = scoring
        self.scoring_reads = 0

    async def request(self, method, path, **args):
        if path == 'lineup_schedule_evidence':
            if method == 'POST':
                row = args['json']
                self.evidence[row['resource']] = row
                return None
            return [self.evidence[args['params']['resource'][3:]]] if args['params']['resource'][3:] in self.evidence else []
        if path == 'team_lineup_snapshots':
            if method == 'POST':
                assert args['json']['competition_id'] == 'competition'
                self.snapshots[:] = [copy.deepcopy(args['json'])]
                return None
            return self.snapshots
        if path == 'league_context_observations':
            self.scoring_reads += 1
            return [self.scoring] if self.scoring else []
        if path in ('worker_accounts', 'rival_finance_evidence_latest'):
            return []
        if path == 'rpc/lineup_schedule_dispatch':
            self.transitions.append('dispatch')
            return True
        if path == 'rpc/lineup_schedule_settle':
            self.transitions.append(args['json']['p_code'])
            return True
        raise AssertionError(path)


async def main(scoring_source):
    import time
    cutoff = stamp(int(time.time()) + 300)
    custom = dict(media=[dict(media='official', pct=100)], ps=None, stats='fixture')
    scoring = (dict(point_system='custom', custom=custom) if scoring_source else
               dict(point_system='source', custom=None))
    fingerprint = hashlib.sha256(json.dumps(scoring, sort_keys=True, separators=(',', ':')).encode()).hexdigest()
    context = dict(account_id='account', championship_id='league', user_team_id='team',
                   competition_id='competition', season='season',
                   scoring_version=fingerprint)
    roles = ['delantero'] * 2 + ['centrocampista'] * 4 + ['defensa'] * 4 + ['portero']
    roster = [dict(id=f'p{i}', name=f'Player {i}', role=role, status='ok', buyPrice=100) for i, role in enumerate(roles)]
    baseline = dict(strategy='4-4-2', starters=[p['id'] for p in roster], bench=[''] * 4, captain='')
    approved = {**baseline, 'starters': ['incoming', *baseline['starters'][1:]]}
    key = '05' * 32
    job = dict(id='b' * 32, account_id='account', context=context, round_id='round', kickoff=cutoff, baseline=baseline,
               stages=[dict(player_id='incoming', lineup=approved)], doubtful=False, stage_index=0, state='preparing',
               credential_id='binding', encrypted_token=encrypt_access(
                   dict(userid='account', token='synthetic'), 'account', 'binding', key))
    calls = Counter()
    official = baseline
    arrived = False
    lose_response = False
    unexpected = []
    kickoff = cutoff

    def response(request):
        nonlocal official
        path = request.url.path
        body = json.loads(request.content)
        calls[path] += 1
        if path == '/1/userteam/lineup':
            answer = dict(strategy='4-4-2', pointSystem='' if scoring_source else 'source',
                          players=[dict(id=pid, position=i, buyPrice=100) for i, pid in enumerate(official['starters'])])
            if scoring_source:
                answer['custom'] = custom
        elif path == '/1/userteam/roster':
            answer = roster + ([dict(id='incoming', name='Signing', role='delantero', status='ok', buyPrice=100)] if arrived else [])
        elif path == '/2/user/activechampionships':
            champ = dict(id='league', league='competition', userteam={'id': 'team'})
            if scoring_source == 'calendar':
                champ.update(ps='custom', custom=custom)
            answer = dict(championships=[champ],
                          leagues=[dict(id='competition', season='season')])
        elif path == '/2/league/list':
            answer = [dict(id='competition', rounds=[dict(id='round', number=1, beginProcess=kickoff, status='next')])]
        elif path == '/2/league/matches':
            answer = dict(rounds=[dict(id='round', number=1, finished=False, status='next',
                                       matches=[dict(id='match', h={'id': 'home'}, a={'id': 'away'},
                                                     info={'date': kickoff})])])
        elif path == '/1/userteam/information':
            answer = dict(configuration={}, league={'id': 'competition', 'season': 'season'})
        elif path == '/5/strategy/availables':
            answer = dict(strategies=[dict(st='4-4-2')])
        elif path == '/5/userteam/multichanges':
            assert body['header'] == {'token': 'synthetic', 'userid': 'account'}
            assert body['query']['modified']['players'][0]['_id'] == 'incoming'
            assert body['query']['current']['players'][0]['_id'] == 'p0'
            official = approved
            if lose_response:
                raise httpx.ReadTimeout('synthetic dropped response')
            answer = dict(code='api.general.ok')
        else:
            unexpected.append(path)
            answer = {}
        return httpx.Response(200, json={'answer': answer})

    stored_scoring = dict(context, observed_at=stamp(), expires_at=cutoff, complete=True,
                          payload=dict(version=2, scoring_expires_at=cutoff, **scoring))
    del stored_scoring['user_team_id']
    store = Store(stored_scoring if scoring_source == 'database' else None)
    async with httpx.AsyncClient(transport=httpx.MockTransport(response)) as client:
        worker = SimpleNamespace(client=client, store=store, identity='worker', key=key, request_count=0,
                                 account_locks={}, connections=asyncio.Semaphore(1), request_interval=0,
                                 last_request={}, is_admitted=lambda *_: True)
        scheduler = LineupScheduler(worker, Path.cwd())
        try:
            await scheduler.execute(job)
            assert store.transitions == ['waiting_for_ownership'], (store.transitions, calls, unexpected)
            assert calls['/5/userteam/multichanges'] == 0
            arrived = True
            lose_response = True
            await scheduler.execute(job)
            assert store.transitions[-2:] == ['dispatch', 'uncertain'], (store.transitions, calls, unexpected)
            await scheduler.helper.close()
            await scheduler.execute({**job, 'state': 'uncertain', 'dispatched_at': stamp()})
            assert store.transitions[-1] == 'saved'
            assert calls['/5/userteam/multichanges'] == 1
            assert store.snapshots[0]['payload']['official']['starters'][0] == 'incoming'
            assert store.snapshots[0]['scoring_version'] == fingerprint
            assert calls['/2/league/list'] == 1
            await scheduler.execute(job)
            assert store.transitions[-1] == 'lineup_changed'
            assert (store.scoring_reads > 0) == (scoring_source == 'database')
            assert not unexpected
            print(f'PASS worker and R bridge ({scoring_source or "explicit"} scoring): ownership, scoped checkpoint reuse, one send, restart readback and external edits')
            print('Futmondo fixture requests:', dict(calls))
        finally:
            await scheduler.helper.close()


if __name__ == '__main__':
    for source in (None, 'calendar', 'database'):
        asyncio.run(main(source))
