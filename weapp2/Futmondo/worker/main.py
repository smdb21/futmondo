"""Durable Python acquisition service; R owns existing parsing/accounting semantics."""
from __future__ import annotations
import asyncio
import base64
import contextlib
import gzip
import hashlib
import io
import json
import logging
import os
import re
import signal
import ssl
import time
import uuid
from datetime import datetime, timezone
from email.utils import parsedate_to_datetime
from pathlib import Path
from urllib.parse import urlparse
from zoneinfo import ZoneInfo
import httpx
from nacl.secret import SecretBox

LOG = logging.getLogger('futmondo.worker')
SOURCE = 'https://api.futmondo.com'
READ_PATHS = frozenset('''/1/user/information /2/user/activechampionships /2/championship/teams
/1/userteam/roster /1/userteam/rounds /1/userteam/dreamteam /1/userteam/nightmareteam
/1/userteam/roundlineup /1/userteam/lineup /1/market/players /1/market/myplayers
/1/market/rosterbids /1/player/summary /1/player/fullprofile /2/player/matches
/1/locker/pressroom /2/locker/news /5/league/championshipplayers
/1/league/championshipteams /1/userteam/information /1/userteam/moneymovements
/1/notification/list /1/notification/unread /1/ranking/round /1/league/rounds
/1/league/getdtconfig /1/championship/information /1/league/information
/1/league/matches /1/league/players /1/userteam/availables /1/match/list /2/league/list /2/league/matches /5/strategy/availables'''.split())
# Database writes are prepared by the existing factual persistence functions only.
FACT_TABLES = frozenset('''championships real_clubs players user_teams user_team_history player_history
market_transactions round_dream_team player_daily_snapshots auction_observations bid_observations
market_observations player_match_observations league_rule_snapshots player_round_history_checks
rival_finance_evidence complete_history_baselines league_context_observations player_membership_observations'''.split())
SECRET_FIELD = re.compile(r'(^|_)(token|password|pwd|authorization|cookie|secret|login|api_?key)($|_)', re.I)
MAX_JSON = 16 * 1024 * 1024
CHUNK = 128 * 1024

class WorkerFailure(Exception):
    """Only fixed safe codes may leave a worker boundary."""
    def __init__(self, code, retry_after=0):
        super().__init__(code)
        self.code = code
        self.retry_after = retry_after


def canonical(value):
    return json.dumps(value, sort_keys=True, ensure_ascii=False, separators=(',', ':'), allow_nan=False)


def stamp(epoch=None):
    return datetime.fromtimestamp(time.time() if epoch is None else epoch, timezone.utc).isoformat()


def source_response_rejected(body):
    """Explicit Futmondo application errors are not successful observations."""
    def rejected(value):
        if isinstance(value, str):
            return value.startswith('api.error.')
        if isinstance(value, dict):
            code = value.get('code')
            error = value.get('error')
            return error is True or (isinstance(error, str) and error.startswith('api.error.')) or (isinstance(code, str) and code.startswith('api.error.'))
        return False
    return rejected(body) or (isinstance(body, dict) and rejected(body.get('answer')))


def plain(node):
    """Decode safe R nodes for discovery, never executable R serialization."""
    kind = node['kind']
    if kind == 'null':
        return None
    if kind == 'list':
        values = [plain(x) for x in node['values']]
        return dict(zip(node['names'], values)) if node.get('names') is not None else values
    if kind == 'frame':
        columns = {k: plain(v) for k, v in node['columns'].items()}
        return [{k: (v if isinstance(v, list) else [v])[i] for k, v in columns.items()} for i in range(node['count'])]
    values = node['values']
    return values[0] if len(values) == 1 else values


def assert_factual(value, depth=0):
    if depth > 30:
        raise WorkerFailure('payload_depth')
    if isinstance(value, dict):
        for key, item in value.items():
            if SECRET_FIELD.search(key):
                raise WorkerFailure('credential_in_fact')
            assert_factual(item, depth + 1)
    elif isinstance(value, list):
        for item in value:
            assert_factual(item, depth + 1)


BASKETBALL_SPORTS = frozenset(('basket', 'basketball', 'baloncesto'))


def filter_football_leagues(body):
    """Remove source-identified basketball; do not guess sport from names."""
    answer = body.get('answer') if isinstance(body, dict) else None
    if not isinstance(answer, dict) or not isinstance(answer.get('championships'), list):
        return body
    leagues = {str(x.get('id') or x.get('_id') or ''): x for x in answer.get('leagues', []) if isinstance(x, dict)}
    def supported(champ):
        league = champ.get('league', '')
        lid = str(league.get('id') or league.get('_id') or '') if isinstance(league, dict) else str(league)
        return all(str(sport or '').strip().lower() not in BASKETBALL_SPORTS
                   for sport in (champ.get('sport'), leagues.get(lid, {}).get('sport')))
    answer['championships'] = [x for x in answer['championships'] if isinstance(x, dict) and supported(x)]
    return body


def encrypt_access(auth, account, generation, key):
    payload = canonical(dict(version=1, account_id=account, generation=str(generation), auth=auth))
    return base64.b64encode(SecretBox(bytes.fromhex(key)).encrypt(payload.encode())).decode()


def decrypt_access(account, key):
    try:
        raw = SecretBox(bytes.fromhex(key)).decrypt(base64.b64decode(account['encrypted_token'], validate=True))
        data = json.loads(raw)
        if data['version'] != 1 or data['account_id'] != account['account_id'] or str(data['generation']) != str(account.get('credential_id', '')):
            raise ValueError()
        # Generation is an encryption binding, independent of job-generation increments.
        auth = data['auth']
        if str(auth['userid']) != account['account_id'] or not auth['token']:
            raise ValueError()
        return auth
    except Exception:
        raise WorkerFailure('credential_configuration') from None


def quiet_interval(account, seconds, now=None):
    local = (now or datetime.now(timezone.utc)).astimezone(ZoneInfo(account['timezone']))
    start, end = account['quiet_start'], account['quiet_end']
    quiet = start <= local.hour < end if start < end else (local.hour >= start or local.hour < end) if start > end else False
    return max(seconds, 3600) if quiet else seconds


def pack_snapshot(value):
    assert_factual(value)
    data = canonical(value).encode()
    if len(data) > MAX_JSON:
        raise WorkerFailure('snapshot_size')
    compressed = gzip.compress(data, mtime=0)
    encoded = base64.b64encode(compressed).decode()
    parts = [encoded[i:i + CHUNK] for i in range(0, len(encoded), CHUNK)]
    if len(parts) > 128:
        raise WorkerFailure('snapshot_size')
    return parts, len(data), hashlib.sha256(compressed).hexdigest()


def unpack_snapshot(manifest, chunks):
    if len(chunks) != manifest['parts'] or not 0 < manifest['json_bytes'] <= MAX_JSON:
        raise WorkerFailure('snapshot_incomplete')
    compressed = base64.b64decode(''.join(chunks), validate=True)
    if hashlib.sha256(compressed).hexdigest() != manifest['sha256']:
        raise WorkerFailure('snapshot_corrupt')
    with gzip.GzipFile(fileobj=io.BytesIO(compressed)) as stream:
        raw = stream.read(manifest['json_bytes'] + 1)
    if len(raw) != manifest['json_bytes']:
        raise WorkerFailure('snapshot_size')
    result = json.loads(raw)
    assert_factual(result)
    return result


class Store:
    def __init__(self, client, url, key):
        self.client = client
        self.url = re.sub(r'(/rest/v1)+$', '', url.rstrip('/')) + '/rest/v1/'
        self.headers = {'apikey': key, 'Authorization': 'Bearer ' + key}

    async def request(self, method, path, **kwargs):
        headers = dict(self.headers, **kwargs.pop('headers', {}))
        try:
            response = await self.client.request(method, self.url + path, headers=headers, **kwargs)
        except httpx.HTTPError:
            raise WorkerFailure('database_connection') from None
        if response.status_code not in range(200, 300):
            raise WorkerFailure('database_' + str(response.status_code))
        return response.json() if response.content else None

    async def rpc(self, name, **params):
        return await self.request('POST', 'rpc/worker_' + name, json=params)

    async def account(self, identity):
        rows = await self.request('GET', 'worker_accounts', params={'account_id': 'eq.' + identity, 'limit': '1'})
        if len(rows) != 1:
            raise WorkerFailure('access_revoked')
        return rows[0]


class Helper:
    def __init__(self, root):
        self.root = root
        self.process = None
        self.stderr_task = None
        self.lock = asyncio.Lock()

    async def start(self):
        # The helper cannot inherit credentials or source authentication from its parent.
        env = {k: v for k, v in os.environ.items() if k in
               ('PATH', 'LANG', 'LC_ALL', 'R_LIBS', 'R_LIBS_USER', 'R_LIBS_SITE', 'R_HOME', 'TMPDIR', 'OPENBLAS_NUM_THREADS', 'OMP_NUM_THREADS')}
        env['FUTMONDO_WORKER_HELPER'] = 'true'
        self.process = await asyncio.create_subprocess_exec('Rscript', '--vanilla', str(self.root / 'worker' / 'helper.R'),
            cwd=str(self.root), env=env, stdin=asyncio.subprocess.PIPE, stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE, limit=24 * 1024 * 1024)
        self.stderr_task = asyncio.create_task(self.discard_stderr())
        first = await asyncio.wait_for(self.process.stdout.readline(), 60)
        if json.loads(first).get('type') != 'ready':
            raise WorkerFailure('helper_startup')

    async def discard_stderr(self):
        # R/parser exception strings can contain private values. Never forward them.
        while await self.process.stderr.read(65536):
            pass

    async def send(self, data):
        self.process.stdin.write((canonical(data) + '\n').encode())
        await self.process.stdin.drain()

    async def run(self, descriptor, exchange):
        async with self.lock:
            if not self.process or self.process.returncode is not None:
                await self.start()
            await self.send({'type': 'run', 'descriptor': descriptor})
            while True:
                line = await asyncio.wait_for(self.process.stdout.readline(), 90)
                if not line:
                    raise WorkerFailure('helper_exit')
                message = json.loads(line)
                if message.get('type') == 'result':
                    return message
                if message.get('type') == 'failure':
                    code = message.get('code')
                    # Only bounded counters and enumerated queue/status labels
                    # may reach logs/database errors; never exception messages.
                    timeout_pattern = (r'finance_timeout_s[01]_b\d{1,4}_q\d{1,4}_o\d{1,4}_t\d{1,4}'
                        r'_ts(?:ready|empty|loading|unavailable|unknown)'
                        + ''.join('_' + kind + r'\d{1,4}' for kind in
                            ('saved', 'seed', 'team', 'lineup', 'ranking', 'notifications', 'ownership', 'ownership_catalog')))
                    safe_timeout = isinstance(code, str) and (code == 'finance_timeout' or re.fullmatch(timeout_pattern, code, flags=re.ASCII))
                    diagnostic_codes = {'resource_parse_codec_class', 'resource_parse_codec_value',
                        'resource_parse_codec_secret', 'resource_parse_incomplete', 'resource_parse_database_shape'}
                    resources = ('get_financial_snapshot|get_acquisition_capacity|get_team_acquisition_capacity|'
                        'get_players_from_team|get_market_players|get_real_clubs|get_user_team_info|get_championships|'
                        'get_championship_players|get_teams|get_lineup_from_team|get_roster_bids|get_my_market_players|'
                        'get_player_summary|get_player_full_profile|get_player_matches|get_player_card_history|'
                        'get_resolved_league_context|get_classification_round_catalog|get_classification_round_ranking|'
                        'get_user_team_rounds|get_round_lineup_snapshot|classification_ranking|league_finances_calc|'
                        'get_notifications|get_notification_unread|information|activechampionships|teams|roster|rounds|'
                        'dreamteam|nightmareteam|roundlineup|lineup|championshipplayers|championshipteams|players|summary|'
                        'fullprofile|matches|pressroom|news|moneymovements|rosterbids|myplayers|getdtconfig|unread|list|database|read|'
                        'get_player_recent_points|get_championship_pressroom|get_money_movements|get_finished_rounds|'
                        'get_finished_round_catalog|get_round_dreamteam|get_active_championships|read_player_points_history|'
                        'calculate_league_finances|market|real_clubs|championships|team_info|user_team_info|roster_bids|'
                        'my_market_players|acq_capacity|financial_snapshot|player_summary|player_summary_points|'
                        'player_profile|player_matches|notifications_list|notifications_unread')
                    safe_diagnostic = isinstance(code, str) and (code in diagnostic_codes or re.fullmatch(
                        r'resource_parse_(?:http|network|parser|schema|read_budget|unknown)_(?:' + resources + r')(?:_[1-5][0-9]{2})?', code, flags=re.ASCII))
                    raise WorkerFailure(code if safe_timeout or safe_diagnostic else 'resource_parse')
                if message.get('type') not in ('http', 'database', 'persist', 'resource', 'progress', 'history_ack'):
                    raise WorkerFailure('helper_protocol')
                try:
                    response = await exchange(message)
                except WorkerFailure as error:
                    # Send fixed codes only; do not convert auth/revocation into optional-source success.
                    if error.code in ('access_revoked', 'source_authentication', 'source_rate_limited',
                            'context_superseded', 'league_membership_unverified', 'source_context',
                            'source_route_denied', 'credential_in_fact',
                            'snapshot_corrupt', 'snapshot_size', 'snapshot_incomplete') or (error.code == 'source_rejected' and descriptor['handler'] != 'fm_worker_finances') or message['type'] in ('persist', 'progress'):
                        raise
                    response = {'error': error.code}
                await self.send(response)

    async def close(self):
        if self.process and self.process.returncode is None:
            self.process.terminate()
            try:
                await asyncio.wait_for(self.process.wait(), 5)
            except asyncio.TimeoutError:
                self.process.kill()
                await self.process.wait()
        if self.stderr_task:
            await self.stderr_task
        self.process = None


class Worker:
    def __init__(self, store, client, key, root):
        self.store, self.client, self.key = store, client, key
        self.identity = str(uuid.uuid4())
        self.helpers = [Helper(root) for _ in range(max(1, min(2, int(os.getenv('FUTMONDO_WORKER_LANES', '2')))))]
        self.stop = asyncio.Event()
        self.connections = asyncio.Semaphore(max(1, min(8, int(os.getenv('FUTMONDO_WORKER_CONNECTIONS', '2')))))
        self.request_interval = 1 / max(.1, min(10, float(os.getenv('FUTMONDO_WORKER_ACCOUNT_RPS', '2'))))
        self.leader = False
        self.last_cleanup = 0
        self.account_locks = {}
        self.last_request = {}
        self.request_count = self.job_count = 0
        self.source_stamps = {}
        self.source_errors = {}
        self.progress_times = {}
        self.progress_ready = set()

    async def guard(self, job, owner, checkpoint=None):
        valid = await self.store.rpc('guard', p_account=job['account_id'], p_key=job['resource_key'],
            p_owner=owner, p_generation=job['generation'], p_checkpoint=checkpoint)
        if not valid:
            raise WorkerFailure('access_revoked')

    async def source(self, job, owner, message):
        url = urlparse(message['url'])
        if url.scheme != 'https' or url.netloc != 'api.futmondo.com' or url.path not in READ_PATHS or url.query or url.fragment:
            raise WorkerFailure('source_route_denied')
        body = message['body']
        if isinstance(body, str):
            body = json.loads(body)
        account = await self.store.account(job['account_id'])
        limit = account.get('rate_limited_until')
        if limit:
            delay = datetime.fromisoformat(limit.replace('Z', '+00:00')).timestamp() - time.time()
            if delay > 0:
                raise WorkerFailure('source_rate_limited', delay)
        auth = decrypt_access(account, self.key)
        if not self.is_admitted(account, auth):
            raise WorkerFailure('access_revoked')
        request_key = hashlib.sha256(canonical(dict(path=url.path, body=body)).encode()).hexdigest()
        body['header'] = {'token': auth['token'], 'userid': auth['userid']}
        query_league = str((body.get('query') or {}).get('championshipId', ''))
        expected = job['descriptor']['context'].get('championship_id', '')
        if query_league and expected and query_league != expected:
            raise WorkerFailure('source_context')
        if query_league in account['excluded_leagues']:
            raise WorkerFailure('access_revoked')
        if expected and expected not in account.get('verified_leagues', {}):
            raise WorkerFailure('league_membership_unverified')
        membership = account.get('verified_leagues', {}).get(expected, {})
        if isinstance(membership, dict):
            for dimension in ('competition_id', 'season', 'scoring_version'):
                selected = job['descriptor']['context'].get(dimension, 'unknown')
                current = membership.get(dimension, 'unknown')
                if selected != 'unknown' and current != 'unknown' and selected != current:
                    raise WorkerFailure('context_superseded')
        cached = await self.store.rpc('checkpoint', p_account=job['account_id'], p_key=job['resource_key'],
            p_owner=owner, p_generation=job['generation'], p_request=request_key)
        if cached:
            response = unpack_snapshot(cached['manifest'], cached['chunks'])
            if url.path == '/2/user/activechampionships':
                response['body'] = filter_football_leagues(response['body'])
            if not source_response_rejected(response['body']):
                self.source_stamps.setdefault(owner, []).append(response['observed_at'])
                return response
            # Older workers checkpointed HTTP-200 application errors. Retain
            # that diagnostic record, but acquire a new source response rather
            # than replaying the same rejection forever after a parser failure.
        lock = self.account_locks.setdefault(job['account_id'], asyncio.Lock())
        async with lock, self.connections:
            for attempt in range(3):
                await self.guard(job, owner)
                delay = self.request_interval - (time.monotonic() - self.last_request.get(job['account_id'], 0))
                if delay > 0:
                    await asyncio.sleep(delay)
                self.last_request[job['account_id']] = time.monotonic()
                self.request_count += 1
                try:
                    async def bounded_request():
                        async with self.client.stream('POST', SOURCE + url.path, json=body, follow_redirects=False) as stream:
                            headers = {'Retry-After': stream.headers.get('Retry-After', '')}
                            content = bytearray()
                            if stream.status_code in range(200, 300):
                                async for piece in stream.aiter_bytes():
                                    content.extend(piece)
                                    if len(content) > MAX_JSON:
                                        raise WorkerFailure('source_size')
                            return httpx.Response(stream.status_code, content=bytes(content), headers=headers)
                    response = await asyncio.wait_for(bounded_request(), 15)
                except (httpx.HTTPError, asyncio.TimeoutError):
                    response = None
                if response is not None and response.status_code == 429:
                    retry = response.headers.get('Retry-After', '')
                    try:
                        delay = float(retry)
                    except ValueError:
                        try:
                            delay = parsedate_to_datetime(retry).timestamp() - time.time()
                        except (ValueError, TypeError, OverflowError):
                            delay = 60
                    delay = max(1, delay)
                    await self.store.request('PATCH', 'worker_accounts', params={'account_id': 'eq.' + job['account_id'],
                        'generation': 'eq.' + str(job['generation'])}, json={'rate_limited_until': stamp(time.time() + delay)})
                    raise WorkerFailure('source_rate_limited', delay)
                if response is not None and response.status_code == 401:
                    raise WorkerFailure('source_authentication')
                if response is not None and response.status_code in range(200, 300):
                    if len(response.content) > MAX_JSON:
                        raise WorkerFailure('source_size')
                    # Never persist or forward echoed authentication envelopes.
                    try:
                        parsed = response.json()
                    except ValueError:
                        raise WorkerFailure('source_parse') from None
                    if not isinstance(parsed, dict):
                        raise WorkerFailure('source_schema')
                    parsed.pop('header', None)
                    parsed.pop('query', None)
                    if url.path == '/2/user/activechampionships':
                        parsed = filter_football_leagues(parsed)
                    assert_factual(parsed)
                    if source_response_rejected(parsed):
                        failure = WorkerFailure('source_rejected')
                        failure.source_route = url.path
                        raise failure
                    answer = {'status': response.status_code, 'body': parsed, 'observed_at': stamp()}
                    parts, size, digest = pack_snapshot(answer)
                    await self.store.rpc('checkpoint', p_account=job['account_id'], p_key=job['resource_key'],
                        p_owner=owner, p_generation=job['generation'], p_request=request_key,
                        p_response={'manifest': dict(parts=len(parts), json_bytes=size, sha256=digest), 'chunks': parts})
                    self.source_stamps.setdefault(owner, []).append(answer['observed_at'])
                    return answer
                if response is not None and response.status_code not in (408, 429, 500, 502, 503, 504):
                    raise WorkerFailure('source_' + str(response.status_code))
                if attempt == 2:
                    raise WorkerFailure('source_transient')
                retry = response.headers.get('Retry-After', '') if response is not None else ''
                await asyncio.sleep(min(30, max(1, int(retry))) if retry.isdigit() else .5 * 2 ** attempt)
        raise WorkerFailure('source_transient')

    def is_admitted(self, account, auth):
        admin = os.getenv('admin', '').strip().lower()
        return not getattr(self, 'admission_blocked', True) or bool(admin and auth.get('user_name', '').strip().lower() == admin)

    async def exchange(self, job, owner, message):
        await self.guard(job, owner)
        if message['type'] == 'progress':
            if job['descriptor']['handler'] != 'fm_worker_finances':
                raise WorkerFailure('helper_protocol')
            now = time.monotonic()
            ready = message['result'].get('ready') is True
            final_preview = ready and owner not in self.progress_ready
            if not final_preview and now - self.progress_times.get(owner, float('-inf')) < 15:
                return {'ok': True, 'published': False}
            result = dict(message['result'], status='partial', completeness='partial')
            # The same scoped descriptor publishes a preview without settling
            # its parent lease; final publication still certifies completion.
            await self.publish(job, owner, result, child=job['descriptor'])
            self.progress_times[owner] = now
            if ready:
                self.progress_ready.add(owner)
            return {'ok': True, 'published': True}
        if message['type'] == 'http':
            try:
                return await self.source(job, owner, message)
            except WorkerFailure as error:
                route = getattr(error, 'source_route', '')
                suffix = '_' + route.strip('/').replace('/', '_') if route in READ_PATHS else ''
                self.source_errors.setdefault(owner, set()).add(error.code + suffix)
                raise
        if message['type'] in ('resource', 'history_ack'):
            descriptor = message['descriptor']
            if message['type'] == 'history_ack' and descriptor.get('handler') != 'get_championship_pressroom':
                raise WorkerFailure('resource_context')
            scope = descriptor['context']
            if scope['account_id'] != job['account_id'] or (scope.get('championship_id') and
                    scope['championship_id'] != job['descriptor']['context'].get('championship_id')):
                raise WorkerFailure('resource_context')
            result = await self.store.rpc('read', p_account=job['account_id'],
                p_versions={descriptor['resource_key']: ''})
            if isinstance(result, dict):
                for item in result.get('resources', []):
                    manifest = item['manifest']
                    if canonical(manifest['context']) != canonical(scope):
                        continue
                    if message['type'] == 'history_ack':
                        # Only acknowledged immutable fact fingerprints may be
                        # reused from expired data; no stale source rows escape.
                        value = unpack_snapshot(manifest, item['chunks'])
                        metadata = value.get('meta', {})
                        return {k: metadata[k] for k in ('history_write_ack', 'locker_write_ack') if k in metadata}
                    if manifest['status'] not in ('ok', 'empty'):
                        continue
                    if datetime.fromisoformat(manifest['valid_until'].replace('Z', '+00:00')).timestamp() <= time.time():
                        continue
                    if (job['descriptor']['handler'] == 'get_clause_transfer_summary' and
                            descriptor['handler'] == 'get_player_summary' and
                            time.time() - datetime.fromisoformat(manifest['observed_at'].replace('Z', '+00:00')).timestamp() >= 30):
                        continue
                    value = unpack_snapshot(manifest, item['chunks'])
                    self.source_stamps.setdefault(owner, []).append(manifest['checked_at'])
                    return {'value': value, 'observed_at': manifest['observed_at']}
            return {'value': None}
        table = message.get('table', '')
        if not re.fullmatch('[a-z_]+', table) or table.startswith('worker_') or table in ('remembered_logins',):
            raise WorkerFailure('database_route_denied')
        if message['type'] == 'database':
            return {'rows': await self.store.request('GET', table, params=message.get('query', {}))}
        if table not in FACT_TABLES:
            raise WorkerFailure('fact_table_denied')
        payload = message['payload']
        assert_factual(payload)
        conflict = message.get('conflict')
        if conflict is not None and not re.fullmatch('[a-z_,]+', conflict):
            raise WorkerFailure('fact_conflict')
        for row in payload if isinstance(payload, list) else [payload]:
            if 'account_id' in row and row['account_id'] != job['account_id']:
                raise WorkerFailure('fact_account')
            cid = row.get('championship_id')
            if cid and cid != job['descriptor']['context'].get('championship_id') and job['descriptor']['handler'] != 'get_active_championships':
                raise WorkerFailure('fact_league')
        observed = min(self.source_stamps.get(owner, [stamp()]))
        acknowledged = await self.store.rpc('write_fact', p_account=job['account_id'], p_key=job['resource_key'],
            p_owner=owner, p_generation=job['generation'], p_table=table, p_payload=payload,
            p_conflict=conflict, p_ignore=bool(message.get('ignore')), p_observed=observed)
        if not acknowledged:
            raise WorkerFailure('access_revoked')
        return {'ok': True}

    async def publish(self, job, owner, result, child=None):
        await self.guard(job, owner)
        if self.source_errors.get(owner) and not self.source_stamps.get(owner):
            raise WorkerFailure('source_unavailable')
        descriptor = child or job['descriptor']
        resource_key = descriptor['resource_key']
        parts, size, digest = pack_snapshot(result['value'])
        version = str(uuid.uuid4())
        for i, part in enumerate(parts):
            await self.guard(job, owner)
            await self.store.request('POST', 'worker_chunks', json=dict(account_id=job['account_id'], resource_key=resource_key,
                version=version, part=i, data=part), headers={'Prefer': 'resolution=ignore-duplicates'})
        checked = result.get('checked_at', stamp())
        observed = result.get('observed_at')
        if observed is None:
            observed = min(self.source_stamps.get(owner, [checked]),
                key=lambda x: datetime.fromisoformat(x.replace('Z', '+00:00')))
        # The R result carries this resource's source time. Unrelated older
        # children in the same composite must not backdate a fresh sibling.
        checked = min((observed, checked), key=lambda x: datetime.fromisoformat(x.replace('Z', '+00:00')))
        # Refresh cadence and manual-action validity are separate; a snapshot never authorizes a trade.
        manifest = dict(context=descriptor['context'], version=version, parts=len(parts), json_bytes=size, sha256=digest,
            checked_at=checked, observed_at=observed, valid_until=stamp(datetime.fromisoformat(checked.replace('Z', '+00:00')).timestamp() + (60 if child else job['interval_seconds'])),
            completeness=result.get('completeness', 'unknown'), status=result.get('status', 'ok'))
        if self.source_errors.get(owner) and (not child or resource_key == job['resource_key']):
            manifest.update(status='partial', completeness='partial', error=','.join(sorted(self.source_errors[owner])))
        if not child and job['descriptor']['handler'] == 'get_active_championships':
            value = plain(result['value'])
            manifest['memberships'] = {}
            for league in value.get('championships', []):
                cid = str(league.get('id') or league.get('_id') or '')
                if cid:
                    context = next((d['context'] for d in result.get('discoveries', []) if d['context']['championship_id'] == cid), {})
                    manifest['memberships'][cid] = {k: context.get(k, 'unknown') for k in ('competition_id', 'season', 'scoring_version')}
        if not child:
            manifest['full_audit'] = bool(job.get('_full_audit'))
        if child:
            if not await self.store.rpc('publish_child', p_account=job['account_id'], p_parent=job['resource_key'],
                    p_owner=owner, p_generation=job['generation'], p_descriptor=descriptor, p_manifest=manifest):
                raise WorkerFailure('access_revoked')
            return
        for resource in result.get('resources', []):
            if resource['descriptor']['resource_key'] != job['resource_key']:
                await self.publish(job, owner, resource, child=resource['descriptor'])
        if not await self.store.rpc('publish', p_account=job['account_id'], p_key=job['resource_key'], p_owner=owner,
                                    p_generation=job['generation'], p_manifest=manifest):
            raise WorkerFailure('access_revoked')
        await self.enqueue_discoveries(job, owner, result.get('discoveries', []))
        self.job_count += 1

    async def enqueue_discoveries(self, job, owner, discoveries):
        # Thousands of sequential RPCs can occupy both acquisition lanes for
        # minutes after their parent lease has ended. Keep each durable write
        # bounded and acknowledged, without mutating helper descriptors.
        batch, size = [], 0
        async def flush():
            acknowledged = await self.store.rpc('enqueue_batch', p_account=job['account_id'],
                p_owner=owner, p_generation=job['generation'], p_descriptors=batch)
            if type(acknowledged) is not int or not 0 <= acknowledged <= len(batch):
                raise WorkerFailure('discovery_unacknowledged')
        for descriptor in discoveries:
            assert_factual(descriptor)
            encoded_size = len(canonical(descriptor).encode())
            if encoded_size > 262144:
                raise WorkerFailure('discovery_size')
            if batch and (len(batch) >= 128 or size + encoded_size > 512 * 1024):
                await flush()
                batch, size = [], 0
            batch.append(descriptor)
            size += encoded_size
        if batch:
            await flush()

    async def lane(self, index):
        owner = self.identity + ':' + str(index)
        while not self.stop.is_set():
            job = None
            try:
                if not self.leader:
                    await asyncio.sleep(1)
                    continue
                job = await self.store.rpc('claim', p_owner=owner, p_foreground=index == 1)
                if not job:
                    await asyncio.sleep(5)
                    continue
                await self.guard(job, owner)
                self.source_stamps[owner] = []
                self.source_errors[owner] = set()
                self.progress_times.pop(owner, None)
                self.progress_ready.discard(owner)
                async def renew():
                    while True:
                        await asyncio.sleep(15)
                        await self.guard(job, owner)
                renewal = asyncio.create_task(renew())
                run_descriptor = json.loads(canonical(job['descriptor']))
                audit = job.get('last_audit_at')
                job['_full_audit'] = job['descriptor']['handler'] == 'get_championship_pressroom' and (not audit or
                    time.time() - datetime.fromisoformat(audit.replace('Z', '+00:00')).timestamp() >= 86400)
                if job['_full_audit']:
                    run_descriptor['args']['full_audit'] = True
                run = asyncio.create_task(self.helpers[index].run(run_descriptor, lambda m: self.exchange(job, owner, m)))
                try:
                    done, _ = await asyncio.wait([run, renewal], return_when=asyncio.FIRST_COMPLETED)
                    if renewal in done:
                        await renewal
                    result = await run
                    renewal.cancel()
                    with contextlib.suppress(asyncio.CancelledError):
                        await renewal
                    await self.publish(job, owner, result)
                finally:
                    renewal.cancel()
                    with contextlib.suppress(asyncio.CancelledError):
                        await renewal
                    if not run.done():
                        run.cancel()
                        with contextlib.suppress(asyncio.CancelledError):
                            await run
                        await self.helpers[index].close()
            except asyncio.CancelledError:
                raise
            except Exception as error:
                code = error.code if isinstance(error, WorkerFailure) else 'worker_internal'
                handler = job.get('descriptor', {}).get('handler') if job else None
                if not isinstance(handler, str) or not re.fullmatch(r'(?:get_|read_)[a-z][a-z0-9_]{0,79}|fm_worker_finances', handler, flags=re.ASCII):
                    handler = 'unknown'
                route = getattr(error, 'source_route', '')
                route = route if route in READ_PATHS else 'unknown'
                LOG.warning('worker_job code=%s handler=%s lane=%d route=%s', code, handler, index, route)
                await self.helpers[index].close()
                if job:
                    with contextlib.suppress(Exception):
                        if code == 'source_authentication':
                            await self.store.request('PATCH', 'worker_accounts', params={'account_id': 'eq.' + job['account_id'],
                                'generation': 'eq.' + str(job['generation'])}, json={'auth_status': 'reauthorize'})
                        if code in ('context_superseded', 'league_membership_unverified'):
                            await self.store.request('DELETE', 'worker_jobs', params={'account_id': 'eq.' + job['account_id'],
                                'resource_key': 'eq.' + job['resource_key'], 'owner': 'eq.' + owner})
                        await self.store.request('PATCH', 'worker_jobs', params={'account_id': 'eq.' + job['account_id'],
                            'resource_key': 'eq.' + job['resource_key'], 'owner': 'eq.' + owner},
                            json={'owner': None, 'lease_until': None, 'last_error': code,
                                  'due_at': stamp(time.time() + max(getattr(error, 'retry_after', 0), min(3600, 15 * 2 ** min(job['attempts'], 8))))})
                await asyncio.sleep(2)

    async def maintenance(self):
        while not self.stop.is_set():
            try:
                self.leader = await self.store.rpc('elect', p_owner=self.identity)
                policy = await self.store.request('GET', 'application_access', params={'id': 'eq.main', 'select': 'admin_only'})
                self.admission_blocked = not (len(policy) == 1 and policy[0]['admin_only'] is False)
                await self.store.request('POST', 'worker_health', json={'id': self.identity, 'status': 'running' if self.leader else 'standby',
                    'heartbeat': stamp(), 'requests': self.request_count, 'jobs': self.job_count}, headers={'Prefer': 'resolution=merge-duplicates'})
                # Enrollment durably seeds account discovery. Restart reads jobs directly;
                # avoid a per-account database scan every heartbeat.
                if self.leader and time.monotonic() - self.last_cleanup > 3600:
                    await self.store.rpc('cleanup')
                    self.last_cleanup = time.monotonic()
                self.account_locks = {k: v for k, v in self.account_locks.items()
                    if v.locked() or time.monotonic() - self.last_request.get(k, 0) < 3600}
                self.last_request = {k: v for k, v in self.last_request.items() if k in self.account_locks}
            except Exception:
                self.admission_blocked = True
                self.leader = False
                LOG.warning('worker_service code=database_unavailable')
            try:
                await asyncio.wait_for(self.stop.wait(), 15)
            except asyncio.TimeoutError:
                pass

    async def run(self):
        tasks = [asyncio.create_task(self.maintenance())] + [asyncio.create_task(self.lane(i)) for i in range(len(self.helpers))]
        try:
            await self.stop.wait()
        finally:
            for task in tasks:
                task.cancel()
            await asyncio.gather(*tasks, return_exceptions=True)
            await asyncio.gather(*(h.close() for h in self.helpers))
            with contextlib.suppress(Exception):
                await self.store.rpc('release', p_owner=self.identity)


async def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)s %(message)s')
    logging.getLogger('httpx').setLevel(logging.WARNING)
    logging.getLogger('httpcore').setLevel(logging.WARNING)
    url, db_key, key = (os.getenv(x, '') for x in ('supabase_project_url', 'supabase_secret_key', 'FUTMONDO_WORKER_KEY'))
    enabled = os.getenv('FUTMONDO_WORKER_ENABLED', 'true').lower() == 'true'
    if not enabled or not url or not db_key or not re.fullmatch('[a-fA-F0-9]{64}', key):
        LOG.warning('worker_service code=%s', 'disabled' if not enabled else 'configuration_missing')
        # Remain supervised without a restart storm; deployment configuration changes require restart.
        await asyncio.Event().wait()
    ssl_context = ssl.create_default_context(cafile=os.getenv('SSL_CERT_FILE') or None)
    async with httpx.AsyncClient(timeout=httpx.Timeout(15, connect=5), verify=ssl_context,
                                  limits=httpx.Limits(max_connections=8, max_keepalive_connections=4)) as client:
        worker = Worker(Store(client, url, db_key), client, key, Path(__file__).resolve().parent.parent)
        loop = asyncio.get_running_loop()
        for sig in (signal.SIGTERM, signal.SIGINT):
            loop.add_signal_handler(sig, worker.stop.set)
        await worker.run()


if __name__ == '__main__':
    asyncio.run(main())
