"""Durable one-shot lineup submissions approved stage by stage."""
import asyncio
import time
from datetime import datetime

import httpx

TEAM_READS = {
    '/1/userteam/lineup', '/1/userteam/roster', '/1/userteam/information', '/5/strategy/availables',
}
CALENDAR_READS = {'/2/user/activechampionships', '/2/league/list', '/2/league/matches'}
MAX_READ_BYTES = 8 * 1024 * 1024


def timestamp(value):
    return datetime.fromisoformat(value.replace('Z', '+00:00')).timestamp()


class LineupScheduler:
    def __init__(self, worker, root):
        from worker.main import Helper
        self.worker = worker
        self.store = worker.store
        self.helper = Helper(root)
        self.last_used = time.monotonic()
        self.last_cleanup = 0

    async def rpc(self, name, **params):
        return await self.store.request('POST', 'rpc/lineup_schedule_' + name, json=params)

    async def settle(self, job, code, delay=None):
        from worker.main import stamp
        return await self.rpc('settle', p_id=job['id'], p_index=job['stage_index'],
                              p_owner=self.worker.identity, p_code=code,
                              p_due=None if delay is None else stamp(time.time() + delay))

    async def pace(self, account):
        delay = self.worker.request_interval - (time.monotonic() - self.worker.last_request.get(account, 0))
        if delay > 0:
            await asyncio.sleep(delay)
        self.worker.last_request[account] = time.monotonic()

    async def read(self, job, auth, message):
        from worker.main import SOURCE, WorkerFailure, assert_factual, source_response_rejected, stamp
        from urllib.parse import urlparse

        if message['type'] in ('resource', 'discovery_resource'):
            # Live eligibility uses the source; only short-lived calendar inputs
            # can be replayed from the durable evidence below.
            return {'value': None}
        if message['type'] == 'database':
            if message['table'] in {'league_context_observations', 'rival_finance_evidence_latest'}:
                return {'rows': await self.store.request('GET', message['table'], params=message['query'])}
            return {'rows': []}
        if message['type'] != 'http':
            raise WorkerFailure('lineup_bridge_denied')
        url = urlparse(message['url'])
        if (url.scheme, url.netloc) != ('https', 'api.futmondo.com') or url.query or url.fragment:
            raise WorkerFailure('lineup_source_denied')
        route = url.path
        query = message['body'].get('query', {})
        ctx = job['context']
        if route in TEAM_READS:
            if query.get('championshipId') != ctx['championship_id'] or query.get('userteamId') != ctx['user_team_id']:
                raise WorkerFailure('lineup_source_scope')
        elif route == '/2/league/matches':
            if query != {'leagueId': ctx['competition_id']}:
                raise WorkerFailure('lineup_source_scope')
        elif route not in CALENDAR_READS:
            raise WorkerFailure('lineup_source_denied')
        if route in CALENDAR_READS:
            existing = await self.store.request('GET', 'lineup_schedule_evidence', params={
                'plan_id': 'eq.' + job['id'], 'resource': 'eq.' + route, 'valid_until': 'gt.' + stamp(), 'limit': '1'})
            if existing:
                return existing[0]['payload']
        body = dict(query=query, header={k: auth[k] for k in ('userid', 'token')}, answer={})
        lock = self.worker.account_locks.setdefault(job['account_id'], asyncio.Lock())
        async with lock, self.worker.connections:
            await self.pace(job['account_id'])
            self.worker.request_count += 1
            response = await self.request(SOURCE + route, body, MAX_READ_BYTES, job)
        if source_response_rejected(response) or not isinstance(response.get('answer'), (dict, list)):
            raise WorkerFailure('lineup_source_unavailable')
        if route == '/2/user/activechampionships':
            from worker.clauses import ClauseScheduler
            ClauseScheduler.validate_membership(job, response)
        now = time.time()
        data = dict(status=200, body={'answer': response['answer']}, observed_at=stamp(now))
        assert_factual(data)
        await self.store.request('POST', 'lineup_schedule_evidence', params={'on_conflict': 'plan_id,resource'},
                                 headers={'Prefer': 'resolution=merge-duplicates,return=minimal'},
                                 json=dict(plan_id=job['id'], resource=route, observed_at=stamp(now),
                                           valid_until=stamp(now + 15), payload=data))
        return data

    async def request(self, url, body, limit, job):
        import json
        from worker.main import WorkerFailure, stamp

        async with self.worker.client.stream('POST', url, json=body, follow_redirects=False) as response:
            if response.status_code in (401, 403):
                raise WorkerFailure('lineup_auth_unavailable')
            if response.status_code == 429:
                from email.utils import parsedate_to_datetime
                retry = response.headers.get('Retry-After', '')
                try:
                    delay = int(retry)
                except ValueError:
                    try:
                        delay = parsedate_to_datetime(retry).timestamp() - time.time()
                    except (TypeError, ValueError, OverflowError):
                        delay = 30
                await self.store.request('PATCH', 'worker_accounts', params={'account_id': 'eq.' + job['account_id']},
                                         json={'rate_limited_until': stamp(time.time() + max(1, delay))})
                raise WorkerFailure('lineup_rate_limited', max(1, delay))
            if not 200 <= response.status_code < 300:
                raise WorkerFailure('lineup_source_unavailable')
            content = bytearray()
            async for chunk in response.aiter_bytes():
                content.extend(chunk)
                if len(content) > limit:
                    raise WorkerFailure('lineup_source_size')
            payload = json.loads(content)
            if not isinstance(payload, dict):
                raise WorkerFailure('lineup_source_unavailable')
            return payload

    async def check(self, job, auth, readback=False):
        index = job['stage_index']
        stage = job['stages'][index]
        baseline = job['stages'][index - 1]['lineup'] if index else job['baseline']
        descriptor = dict(version=1, handler='lineup_schedule_check', context=job['context'],
                          args=dict(stage=stage, baseline=baseline, round_id=job['round_id'],
                                    kickoff=job['kickoff'], doubtful=job['doubtful'], readback=readback))
        try:
            result = await self.helper.run(descriptor, lambda message: self.read(job, auth, message))
        except Exception:
            await self.helper.close()
            raise
        return result['lineup_check']

    async def persist_snapshot(self, job, snapshot):
        from worker.main import assert_factual
        ctx = job['context']
        assert_factual(snapshot)
        await self.store.request('POST', 'team_lineup_snapshots',
            params={'on_conflict': 'account_id,championship_id,competition_id,user_team_id,target_round,season,scoring_version'},
            headers={'Prefer': 'resolution=merge-duplicates,return=minimal'},
            json=dict(account_id=ctx['account_id'], championship_id=ctx['championship_id'],
                      user_team_id=ctx['user_team_id'], target_round=job['round_id'], competition_id=ctx['competition_id'],
                      season=ctx['season'],
                      scoring_version=ctx['scoring_version'], payload=snapshot, observed_at=snapshot['observed_at'],
                      expires_at=snapshot['expires_at'], complete=True))

    async def reconcile(self, job, auth):
        # The endpoint has no round selector. A post-kickoff read can't prove
        # which round received the edit.
        if time.time() >= timestamp(job['kickoff']):
            await self.settle(job, 'readback_unverified')
            return
        result = await self.check(job, auth, readback=True)
        if time.time() >= timestamp(job['kickoff']):
            await self.settle(job, 'readback_unverified')
        elif result['code'] == 'saved':
            ctx = job['context']
            filters = {key: 'eq.' + ctx[key] for key in
                       ('account_id', 'championship_id', 'user_team_id', 'competition_id', 'season', 'scoring_version')}
            rows = await self.store.request('GET', 'team_lineup_snapshots', params={
                **filters, 'target_round': 'eq.' + job['round_id'], 'select': 'payload', 'limit': '1'})
            if rows:
                snapshot = rows[0]['payload']
                snapshot['official'] = result['official']
                snapshot['source'].update(wire=result['wire'], locked=result['locked'])
                await self.persist_snapshot(job, snapshot)
            await self.settle(job, 'saved')
        else:
            # Give a successful transport response one bounded propagation
            # window. Later reconciliation remains read-only.
            if time.time() - timestamp(job['dispatched_at']) < 30:
                await self.settle(job, 'uncertain', 15)
            else:
                await self.settle(job, 'readback_mismatch')

    async def execute(self, job):
        from worker.main import SOURCE, WorkerFailure, decrypt_access, source_response_rejected, stamp
        started_dispatched = job['state'] == 'uncertain'
        if started_dispatched and time.time() >= timestamp(job['kickoff']):
            return await self.settle(job, 'readback_unverified')
        try:
            auth = decrypt_access(job, self.worker.key)
            if not self.worker.is_admitted(job, auth):
                raise WorkerFailure('lineup_auth_unavailable')
            rows = await self.store.request('GET', 'worker_accounts', params={
                'account_id': 'eq.' + job['account_id'], 'select': 'rate_limited_until', 'limit': '1'})
            limited = rows[0].get('rate_limited_until') if rows else None
            if limited and timestamp(limited) > time.time():
                raise WorkerFailure('lineup_rate_limited', timestamp(limited) - time.time())
            if started_dispatched:
                return await self.reconcile(job, auth)
            result = await self.check(job, auth)
            if result.get('snapshot'):
                await self.persist_snapshot(job, result['snapshot'])
            code = result['code']
            if code != 'ready':
                return await self.settle(job, code, 30 if code == 'waiting_for_ownership' else None)
            ctx = job['context']
            until = result['snapshot']['expires_at']
            lock = self.worker.account_locks.setdefault(job['account_id'], asyncio.Lock())
            async with lock, self.worker.connections:
                await self.pace(job['account_id'])
                if not await self.rpc('dispatch', p_id=job['id'], p_index=job['stage_index'],
                                      p_owner=self.worker.identity, p_until=until):
                    return await self.settle(job, 'source_unavailable', 15)
                started_dispatched = True
                job['dispatched_at'] = stamp()
                if time.time() >= min(timestamp(job['kickoff']), timestamp(until)):
                    return await self.settle(job, 'readback_unverified')
                body = dict(header={k: auth[k] for k in ('userid', 'token')}, answer=[], query=dict(
                    championshipId=ctx['championship_id'], userteamId=ctx['user_team_id'],
                    **{key: result[key] for key in ('current', 'modified', 'changes')}))
                self.worker.request_count += 1
                response = await self.request(SOURCE + '/5/userteam/multichanges', body, 65536, job)
                if source_response_rejected(response):
                    return await self.settle(job, 'rejected')
            return await self.reconcile(job, auth)
        except WorkerFailure as error:
            if error.code in ('lineup_auth_unavailable', 'credential_configuration', 'purchase_membership_lost'):
                return await self.settle(job, 'auth_unavailable')
            if error.code == 'purchase_context_superseded' and not started_dispatched:
                return await self.settle(job, 'context_changed')
            return await self.settle(job, 'uncertain' if started_dispatched else 'source_unavailable',
                                     max(15, error.retry_after or 0))
        except (httpx.HTTPError, ValueError, asyncio.TimeoutError):
            return await self.settle(job, 'uncertain' if started_dispatched else 'source_unavailable', 30)

    async def run(self):
        from worker.main import LOG
        try:
            while not self.worker.stop.is_set():
                try:
                    job = await self.rpc('claim', p_owner=self.worker.identity) if self.worker.leader else None
                    if job:
                        job['account_id'] = job['context']['account_id']
                        self.last_used = time.monotonic()
                        await asyncio.wait_for(self.execute(job), timeout=85)
                        continue
                    if self.helper.process and time.monotonic() - self.last_used > 30:
                        await self.helper.close()
                    if self.worker.leader and time.monotonic() - self.last_cleanup > 3600:
                        await self.rpc('cleanup')
                        self.last_cleanup = time.monotonic()
                except Exception:
                    # An expired lease recovers preparation, or reconciles a
                    # durable dispatch without allowing another mutation.
                    LOG.warning('lineup_scheduler code=cycle_failed')
                    await self.helper.close()
                try:
                    await asyncio.wait_for(self.worker.stop.wait(), 5)
                except asyncio.TimeoutError:
                    pass
        finally:
            await self.helper.close()
