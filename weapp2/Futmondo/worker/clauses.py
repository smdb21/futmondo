"""One elected, bounded scheduler for explicitly authorized one-shot purchases.

Never route a mutation through the factual worker's retry/checkpoint transport.
"""
import asyncio
import contextlib
import hashlib
import json
import math
import time
import uuid
from datetime import datetime
from urllib.parse import urlparse

import httpx

# The locked-clause diagnostic returned api.error.not_found, which does not
# identify a timing rejection. Keep it terminal alongside other explicit errors.
CLOSED_CLAUSE_CODES = frozenset()
ACTIVE = {'scheduled', 'preparing', 'dispatched', 'uncertain', 'accepted'}


def epoch(value):
    return datetime.fromisoformat(value.replace('Z', '+00:00')).timestamp()


def scalar(value):
    return value[0] if isinstance(value, list) and len(value) == 1 else value


def flatten(value):
    if isinstance(value, dict):
        return {key: flatten(item) for key, item in value.items()}
    if isinstance(value, list):
        return scalar([flatten(item) for item in value])
    return value


class ClauseScheduler:
    def __init__(self, worker, root):
        from worker.main import Helper
        self.worker = worker
        self.store = worker.store
        self.helper = Helper(root)  # lazy; never one process per scheduled player
        self.last_used = 0
        self.active = None
        self.source_times = []

    async def rpc(self, name, **params):
        return await self.store.request('POST', 'rpc/clause_' + name, json=params)

    async def guard(self, job, owner):
        from worker.main import WorkerFailure
        if not await self.rpc('touch', p_id=job['id'], p_owner=owner):
            raise WorkerFailure('purchase_authorization_lost')

    async def authorization(self, job, dispatch=False):
        from worker.main import WorkerFailure, decrypt_access
        rows = await self.store.request('GET', 'clause_authorizations', params={
            'account_id': 'eq.' + job['account_id'], 'limit': '1'})
        if len(rows) != 1 or rows[0].get('auth_status') != 'active' or (dispatch and not rows[0]['enabled']):
            raise WorkerFailure('purchase_access_revoked')
        limited = rows[0].get('rate_limited_until')
        if limited and epoch(limited) > time.time():
            raise WorkerFailure('purchase_rate_limited', epoch(limited) - time.time())
        auth = decrypt_access(rows[0], self.worker.key)
        auth = dict(auth, _credential_id=rows[0]['credential_id'])
        if not self.worker.is_admitted(rows[0], auth):
            raise WorkerFailure('purchase_admission_blocked')
        return auth

    async def evidence(self, context, key):
        rows = await self.store.request('GET', 'clause_evidence', params={
            'account_id': 'eq.' + context['account_id'], 'evidence_key': 'eq.' + key,
            'valid_until': 'gt.' + self.stamp(), 'complete': 'eq.true', 'limit': '1'})
        return rows[0] if len(rows) == 1 and rows[0]['context'] == context else None

    @staticmethod
    def stamp(when=None):
        from worker.main import stamp
        return stamp(when)

    async def save_evidence(self, job, owner, key, payload, observed, until):
        from worker.main import assert_factual
        assert_factual(payload)
        # Atomic SQL enforces the authorization, scope and per-account byte bound.
        if not await self.rpc('save_evidence', p_id=job['id'], p_owner=owner, p_key=key,
                              p_payload=payload, p_observed=observed, p_until=until):
            from worker.main import WorkerFailure
            raise WorkerFailure('purchase_evidence_unacknowledged')

    async def read_source(self, job, owner, message):
        from worker.main import READ_PATHS, WorkerFailure, canonical, filter_football_leagues, source_response_rejected
        url = urlparse(message['url'])
        if url.scheme != 'https' or url.netloc != 'api.futmondo.com' or url.path not in READ_PATHS or url.query or url.fragment:
            raise WorkerFailure('purchase_source_route_denied')
        body = message['body']
        if isinstance(body, str):
            body = json.loads(body)
        body = json.loads(canonical(body))
        body.pop('header', None)
        query = body.get('query', {})
        ctx = job['context']
        if query.get('championshipId') and str(query['championshipId']) != ctx['championship_id']:
            raise WorkerFailure('purchase_source_context')
        key = hashlib.sha256(canonical(dict(context=ctx, route=url.path, body=body)).encode()).hexdigest()
        saved = await self.evidence(ctx, key)
        if saved:
            self.source_times.append(saved['observed_at'])
            return saved['payload']
        auth = await self.authorization(job)
        body['header'] = dict(token=auth['token'], userid=auth['userid'])
        lock = self.worker.account_locks.setdefault(job['account_id'], asyncio.Lock())
        async with lock, self.worker.connections:
            await self.guard(job, owner)
            await self.pace(job['account_id'])
            self.worker.request_count += 1
            try:
                async with self.worker.client.stream('POST', message['url'], json=body, follow_redirects=False) as response:
                    content = bytearray()
                    async for part in response.aiter_bytes():
                        content.extend(part)
                        if len(content) > 16 * 1024 * 1024:
                            raise WorkerFailure('purchase_source_size')
                    if response.status_code == 401:
                        await self.reject_auth(job, auth['_credential_id'])
                        raise WorkerFailure('purchase_authentication')
                    if response.status_code == 429:
                        delay = await self.rate_limit(job, response.headers.get('Retry-After', ''))
                        raise WorkerFailure('purchase_rate_limited', delay)
                    if response.status_code not in range(200, 300):
                        raise WorkerFailure('purchase_read_http_' + str(response.status_code))
                    parsed = json.loads(content)
            except (httpx.HTTPError, ValueError):
                raise WorkerFailure('purchase_read_unavailable') from None
        answer = parsed.get('answer') if isinstance(parsed, dict) else None
        code = answer.get('code') if isinstance(answer, dict) else None
        if source_response_rejected(parsed) or isinstance(code, str) and code.startswith('api.') and code != 'api.general.ok':
            raise WorkerFailure('purchase_read_rejected')
        # Consumers use answer only. Never checkpoint a reflected auth header.
        result_body = {'answer': parsed.get('answer')} if isinstance(parsed, dict) and 'answer' in parsed else parsed
        if url.path == '/2/user/activechampionships':
            result_body = filter_football_leagues(result_body)
            self.validate_membership(job, result_body)
        now = time.time()
        self.source_times.append(self.stamp(now))
        result = dict(status=200, body=result_body, observed_at=self.stamp(now))
        ttl = 5 if url.path in ('/1/player/summary', '/1/userteam/information', '/1/market/players') else 15
        await self.save_evidence(job, owner, key, result, result['observed_at'], self.stamp(now + ttl))
        return result

    @staticmethod
    def validate_membership(job, body):
        from worker.main import WorkerFailure
        answer = body.get('answer', {})
        championships = answer.get('championships')
        if not isinstance(championships, list):
            raise WorkerFailure('purchase_membership_unverified')
        selected = [c for c in championships if isinstance(c, dict) and str(c.get('id') or c.get('_id') or '') == job['context']['championship_id']]
        if len(selected) != 1:
            raise WorkerFailure('purchase_membership_lost')
        selected = selected[0]
        team = selected.get('userteam') or {}
        if str(team.get('id') or team.get('_id') or '') != job['context']['user_team_id']:
            raise WorkerFailure('purchase_context_superseded')
        league = selected.get('league')
        league = str(league.get('id') or league.get('_id') or '') if isinstance(league, dict) else str(league or '')
        if league and league != job['context']['competition_id']:
            raise WorkerFailure('purchase_context_superseded')
        sources = [x for x in answer.get('leagues', []) if isinstance(x, dict) and str(x.get('id') or x.get('_id') or '') == league]
        if len(sources) == 1 and sources[0].get('season') not in (None, '', 'unknown') and str(sources[0]['season']) != job['context']['season']:
            raise WorkerFailure('purchase_context_superseded')

    async def pace(self, account):
        delay = self.worker.request_interval - (time.monotonic() - self.worker.last_request.get(account, 0))
        if delay > 0:
            await asyncio.sleep(delay)
        self.worker.last_request[account] = time.monotonic()

    async def exchange(self, job, owner, message):
        from worker.main import WorkerFailure, assert_factual, canonical, unpack_snapshot
        await self.guard(job, owner)
        kind = message['type']
        if kind == 'http':
            return await self.read_source(job, owner, message)
        if kind in ('resource', 'discovery_resource', 'history_ack'):
            descriptor = message['descriptor']
            context = descriptor['context']
            for dimension in ('account_id', 'championship_id', 'competition_id', 'season', 'scoring_version'):
                # Account-level membership reads intentionally have no league scope.
                if dimension == 'account_id' or context.get('championship_id'):
                    if context[dimension] != job['context'][dimension]:
                        raise WorkerFailure('purchase_resource_context')
            data = await self.store.rpc('read', p_account=job['account_id'], p_versions={descriptor['resource_key']: ''})
            for item in data.get('resources', []) if isinstance(data, dict) else []:
                m = item['manifest']
                if m['context'] != context or m['status'] not in ('ok', 'empty'):
                    continue
                if kind == 'history_ack':
                    return {k: v for k, v in unpack_snapshot(m, item['chunks']).get('meta', {}).items()
                            if k in ('history_write_ack', 'locker_write_ack')}
                freshness = self.resource_freshness(descriptor, job)
                if epoch(m['valid_until']) <= time.time() or time.time() - epoch(m['observed_at']) >= freshness:
                    continue
                self.source_times.append(m['observed_at'])
                return dict(value=unpack_snapshot(m, item['chunks']), observed_at=m['observed_at'])
            cached = await self.evidence(job['context'], descriptor['resource_key'])
            if cached and kind == 'resource' and time.time() - epoch(cached['observed_at']) < self.resource_freshness(descriptor, job):
                self.source_times.append(cached['observed_at'])
                return cached['payload']
            return {'value': None}
        table = message.get('table', '')
        if kind == 'database':
            # Same private factual read boundary as the acquisition helper.
            if not table.replace('_', '').isalnum() or table.startswith(('clause_', 'worker_')) or table == 'remembered_logins':
                raise WorkerFailure('purchase_database_route_denied')
            return {'rows': await self.store.request('GET', table, params=message.get('query', {}))}
        if kind == 'persist':
            assert_factual(message['payload'])
            ok = await self.rpc('write_fact', p_id=job['id'], p_owner=owner, p_table=table,
                                p_payload=message['payload'], p_conflict=message.get('conflict'),
                                p_ignore=bool(message.get('ignore')), p_observed=message.get('observed_at') or (min(self.source_times) if self.source_times else self.stamp()))
            if not ok:
                raise WorkerFailure('purchase_fact_unacknowledged')
            return {'ok': True}
        raise WorkerFailure('purchase_helper_protocol')

    @staticmethod
    def resource_freshness(descriptor, job):
        handler = descriptor['handler']
        if handler == 'get_player_summary':
            return 5 if descriptor['context'].get('player_id') == job['player_id'] else 60
        return {'get_user_team_info': 5, 'get_market_players': 5, 'get_players_from_team': 15,
                'get_championship_pressroom': 15, 'get_active_championships': 60,
                'get_resolved_league_context': 60}.get(handler, float('inf'))

    async def check(self, job, owner):
        from worker.main import canonical, plain
        context = job['context']
        args = dict(championship_id=context['championship_id'], user_team_id=context['user_team_id'], player_id=job['player_id'], require_clause=True)
        descriptor = dict(version=1, handler='get_scheduled_clause_check', args=args, context=context)
        descriptor['resource_key'] = hashlib.sha256(canonical(descriptor).encode()).hexdigest()
        self.source_times = []
        result = await self.helper.run(descriptor, lambda m: self.exchange(job, owner, m))
        for resource in result.get('resources', []):
            if resource.get('status') in ('ok', 'empty') and epoch(resource['valid_until']) > time.time():
                await self.save_evidence(job, owner, resource['descriptor']['resource_key'],
                    dict(value=resource['value'], observed_at=resource['observed_at']),
                    resource['observed_at'], resource['valid_until'])
        await self.publish_resources(job, owner, result.get('resources', []))
        self.last_used = time.monotonic()
        # R vectors preserve their shape in the wire codec; unwrap scalar fields,
        # retaining identity arrays even when they contain a single item.
        raw = plain(result['value'])
        facts = flatten(raw)
        facts['transactions'] = [flatten(x) for x in raw.get('transactions') or []]
        facts['offers'] = raw.get('offers') or []
        if isinstance(facts['offers'], dict):
            facts['offers'] = [facts['offers']]
        facts['offers'] = [flatten(x) for x in facts['offers']]
        facts['owned_ids'] = raw.get('owned_ids') or []
        if not isinstance(facts['owned_ids'], list):
            facts['owned_ids'] = [facts['owned_ids']]
        for field in ('pending_inbound_ids', 'pending_outbound_ids'):
            ids = raw['roster'].get(field) or []
            facts['roster'][field] = ids if isinstance(ids, list) else [ids]
        return facts

    async def publish_resources(self, job, owner, resources):
        from worker.main import pack_snapshot, WorkerFailure
        if not resources:
            return
        accounts = await self.store.request('GET', 'worker_accounts', params={
            'account_id': 'eq.' + job['account_id'], 'select': 'account_id', 'limit': '1'})
        if not accounts:
            return
        for resource in resources:
            if resource.get('status') not in ('ok', 'empty'):
                continue
            parts, size, digest = pack_snapshot(resource['value'])
            descriptor = resource['descriptor']
            version = str(uuid.uuid4())
            for i, part in enumerate(parts):
                await self.guard(job, owner)
                await self.store.request('POST', 'worker_chunks', json=dict(account_id=job['account_id'],
                    resource_key=descriptor['resource_key'], version=version, part=i, data=part),
                    headers={'Prefer': 'resolution=ignore-duplicates'})
            observed = resource['observed_at']
            manifest = dict(version=version, context=descriptor['context'], parts=len(parts), json_bytes=size,
                sha256=digest, observed_at=observed, checked_at=observed, valid_until=resource['valid_until'],
                status=resource['status'], completeness=resource.get('completeness', 'unknown'))
            if not await self.rpc('publish', p_id=job['id'], p_owner=owner, p_descriptor=descriptor, p_manifest=manifest):
                raise WorkerFailure('purchase_publication_unacknowledged')

    async def settle(self, job, owner, state, code, due=None, opening=None):
        from worker.main import WorkerFailure
        if not await self.rpc('settle', p_id=job['id'], p_owner=owner, p_state=state,
                              p_code=code, p_due=self.stamp(due) if due is not None else None,
                              p_open=self.stamp(opening) if opening is not None else None):
            raise WorkerFailure('purchase_result_unacknowledged')

    async def rate_limit(self, job, header):
        from email.utils import parsedate_to_datetime
        try:
            delay = float(header)
        except (TypeError, ValueError):
            try:
                delay = parsedate_to_datetime(header).timestamp() - time.time()
            except (ValueError, TypeError, OverflowError):
                delay = 30
        if not math.isfinite(delay):
            delay = 30
        delay = max(1, delay)
        deadline = self.stamp(time.time() + delay)
        await self.store.request('PATCH', 'clause_authorizations', params={'account_id': 'eq.' + job['account_id']},
                                 json={'rate_limited_until': deadline})
        await self.store.request('PATCH', 'worker_accounts', params={'account_id': 'eq.' + job['account_id']},
                                 json={'rate_limited_until': deadline})
        return delay

    async def reject_auth(self, job, credential):
        await self.store.request('PATCH', 'clause_authorizations', params={'account_id': 'eq.' + job['account_id'], 'credential_id': 'eq.' + credential},
                                 json={'auth_status': 'reauthorize'})

    async def purchase(self, job, owner, facts):
        """Single dispatch. No transport retries, redirects or source checkpoints."""
        from worker.main import WorkerFailure
        auth = await self.authorization(job, dispatch=True)
        lock = self.worker.account_locks.setdefault(job['account_id'], asyncio.Lock())
        async with lock, self.worker.connections:
            await self.pace(job['account_id'])
            # This transaction durably records intent and validates facts BEFORE HTTP.
            if not await self.rpc('dispatch', p_id=job['id'], p_owner=owner, p_facts=facts):
                await self.settle(job, owner, 'failed', 'execution_preflight_changed')
                return
            body = dict(header=dict(token=auth['token'], userid=auth['userid']), query=dict(
                championshipId=job['context']['championship_id'], userteamId=job['context']['user_team_id'],
                player_slug=job['player_slug'], player_id=job['player_id'], price=facts['player']['price']), answer={})
            self.worker.request_count += 1
            try:
                async with self.worker.client.stream('POST', 'https://api.futmondo.com/1/market/rosterclause', json=body,
                                                      follow_redirects=False) as response:
                    content = bytearray()
                    async for part in response.aiter_bytes():
                        content.extend(part)
                        if len(content) > 64 * 1024:
                            raise ValueError()
                    answer = json.loads(content).get('answer', {})
                    code = answer.get('code') if isinstance(answer, dict) else None
                    if response.status_code == 429:
                        await self.rate_limit(job, response.headers.get('Retry-After', ''))
                    if response.status_code not in range(200, 300):
                        raise ValueError()
            except (httpx.HTTPError, ValueError, TypeError, AttributeError):
                await self.settle(job, owner, 'uncertain', 'purchase_response_uncertain')
                return
            if code == 'api.general.ok':
                await self.settle(job, owner, 'accepted', code)
            elif code in CLOSED_CLAUSE_CODES:
                delay = min(10, 2 ** min(job['attempts'], 4))
                if time.time() + delay > epoch(job['expires_at']):
                    await self.settle(job, owner, 'expired', 'opening_window_expired')
                else:
                    await self.settle(job, owner, 'scheduled', 'closed_verified:' + code, time.time() + delay)
            elif isinstance(code, str) and len(code) <= 120 and code.isascii() and all(c.isalnum() or c in '._' for c in code) and (code.startswith('api.error.') or code == 'api.market.max_number_players_in_roster' or answer.get('error') is True):
                # Only an explicit error envelope or an evidenced rejection is terminal.
                await self.settle(job, owner, 'failed', code)
            else:
                safe_code = code if isinstance(code, str) and 0 < len(code) <= 120 and code.isascii() and all(c.isalnum() or c in '._' for c in code) else 'purchase_response_unclassified'
                await self.settle(job, owner, 'uncertain', safe_code)

    async def execute(self, job, owner):
        from worker.main import WorkerFailure
        await self.guard(job, owner)
        if job['state'] not in ('uncertain', 'accepted') and time.time() > epoch(job['expires_at']):
            await self.settle(job, owner, 'expired', 'opening_window_expired')
            return
        facts = await self.check(job, owner)
        await self.rpc('reconcile_reservations', p_context=job['context'], p_facts=facts)
        rows = await self.store.request('GET', 'clause_jobs', params={'id': 'eq.' + job['id'], 'limit': '1'})
        if rows and rows[0]['state'] in ('succeeded', 'cancelled', 'expired', 'failed'):
            return
        if job['state'] in ('uncertain', 'accepted'):
            # Ownership alone cannot prove who paid a clause pending delivery.
            # Preserve the uncertain state and never dispatch a second purchase.
            delay = min(3600, 30 * 2 ** min(job.get('reconciliation_attempts', 1), 7))
            await self.settle(job, owner, job['state'], job['result_code'], time.time() + delay)
            return
        player = facts['player']
        if player['owner_id'] != job['original_owner'] or player['transferred'] is not False:
            await self.settle(job, owner, 'cancelled', 'ownership_or_transfer_changed')
            return
        price = player.get('price')
        if not isinstance(price, (int, float)) or not math.isfinite(price) or price <= 0 or price > float(job['max_price']):
            await self.settle(job, owner, 'cancelled', 'price_exceeds_authorized_cap')
            return
        opening = epoch(player['opens_at'])
        if opening > time.time():
            remaining = opening - time.time()
            if remaining > 2:
                # A newly extended distant lock needs its own cold preparation.
                lead = 10 if opening != epoch(job['opens_at']) or remaining > 12 else 2
                await self.settle(job, owner, 'scheduled', 'waiting_for_opening', max(time.time() + .05, opening - lead), opening)
                return
            await asyncio.sleep(max(0, remaining))
            await self.guard(job, owner)
        if time.time() < epoch(job['opens_at']):
            # An early unlock observed during preparation may execute immediately.
            job['opens_at'] = player['opens_at']
            await self.settle(job, owner, 'scheduled', 'opening_changed', time.time(), opening)
            return
        await self.purchase(job, owner, facts)

    async def run(self):
        from worker.main import WorkerFailure, LOG
        owner = self.worker.identity + ':clause'
        try:
            while not self.worker.stop.is_set():
                job = None
                try:
                    if self.worker.leader:
                        job = await self.rpc('claim', p_owner=owner)
                    if job:
                        self.active = job
                        async def renew():
                            while True:
                                await asyncio.sleep(15)
                                await self.guard(job, owner)
                        renewal = asyncio.create_task(renew())
                        execution = asyncio.create_task(self.execute(job, owner))
                        try:
                            done, _ = await asyncio.wait((renewal, execution), return_when=asyncio.FIRST_COMPLETED)
                            if renewal in done:
                                await renewal
                            await execution
                        finally:
                            renewal.cancel()
                            if not execution.done():
                                execution.cancel()
                            await asyncio.gather(renewal, execution, return_exceptions=True)
                    elif self.helper.process and time.monotonic() - self.last_used > 30:
                        await self.helper.close()
                except asyncio.CancelledError:
                    raise
                except Exception as error:
                    code = error.code if isinstance(error, WorkerFailure) else 'purchase_internal'
                    LOG.warning('clause_job code=%s', code)
                    await self.helper.close()
                    if job:
                        # SQL refuses dispatched->scheduled here: an ambiguous send
                        # is recovered as uncertain when the durable lease expires.
                        with contextlib.suppress(Exception):
                            state = 'failed' if job['state'] not in ('uncertain', 'accepted') and code in ('purchase_authentication', 'purchase_access_revoked', 'purchase_admission_blocked') else job['state']
                            if job['state'] not in ('uncertain', 'accepted') and code in ('purchase_membership_lost', 'purchase_context_superseded'):
                                state = 'cancelled'
                            if state == 'preparing':
                                state = 'scheduled'
                            await self.settle(job, owner, state, code, time.time() + max(10, getattr(error, 'retry_after', 0)))
                finally:
                    self.active = None
                try:
                    await asyncio.wait_for(self.worker.stop.wait(), 1)
                except asyncio.TimeoutError:
                    pass
        finally:
            await self.helper.close()
