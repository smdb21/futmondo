"""Read announced team sheets for verified Futmondo leagues using the StrikerAPI free-plan endpoints."""
import asyncio
import json
import re
import time
import unicodedata
from collections import Counter, OrderedDict
from datetime import datetime, timezone
from pathlib import Path

import httpx

LEAGUES = json.loads((Path(__file__).resolve().parents[1] / 'www/club-lineup-leagues.json').read_text())
BASE = 'https://mcp.strikerapi.com/v1/'


def normal(value):
    text = unicodedata.normalize('NFKD', str(value or '').casefold())
    text = ''.join(c for c in text if not unicodedata.combining(c))
    return ' '.join(re.findall(r'[^\W_]+', text, re.UNICODE))


def club_name(value):
    name = ' '.join(w for w in normal(value).split() if w not in {'cf', 'fc', 'rcd', 'ud', 'cd', 'rc'})
    aliases = {'atletico de madrid': 'atletico madrid', 'atletico': 'atletico madrid',
               'athletic club': 'athletic bilbao', 'athletic': 'athletic bilbao',
               'athletic club bilbao': 'athletic bilbao', 'betis': 'real betis',
               'celta': 'celta de vigo', 'celta vigo': 'celta de vigo',
               'rayo': 'rayo vallecano', 'deportivo alaves': 'alaves', 'ca osasuna': 'osasuna',
               'real oviedo': 'oviedo', 'real valladolid': 'valladolid'}
    return aliases.get(name, name)


class FeedError(Exception):
    """Safe status returned to the presentation layer."""


def identifier(value):
    if not isinstance(value, str) or not re.fullmatch(r'[1-9][0-9]{0,17}', value):
        raise FeedError('incomplete')
    return value


def same_fixture(game, fixture, clubs):
    starts = datetime.fromisoformat(game['starts_at'].replace('Z', '+00:00'))
    if starts.tzinfo is None or starts.timestamp() != fixture['unix']:
        return False
    if game['home']['team_id'] == game['away']['team_id']:
        return False
    for side in ('home', 'away'):
        team = game[side]
        prior = (clubs.get(fixture[side+'_id']) or {}).get('team_id')
        if prior:
            if prior != team['team_id']:
                return False
        elif not club_name(team['name']) or club_name(team['name']) != club_name(fixture[side+'_name']):
            return False
    return True


def bio(row):
    name = normal(row['name'])
    if not name:
        raise FeedError('incomplete')
    return dict(id=identifier(row['player_id']), name=name, dob=row.get('date_of_birth') or '',
                number=str(row.get('jersey') or ''))


def identify(local, pool, old):
    names = {normal(local.get(k)) for k in ('name', 'slug', 'full_name')} - {''}
    dob = str(local.get('dob') or '')[:10]
    number = str(local.get('number') or '')
    if old:
        p = pool.get(old)
        return old if p and not (dob and p['dob'] and p['dob'] != dob) else None
    hits = []
    for pid,p in pool.items():
        if dob and p['dob'] and dob != p['dob']:
            continue
        # A shortened spelling (including a surname or nickname) needs independent evidence.
        spellings = {p['name'],p.get('sheet_name','')} - {''}
        exact = names & spellings
        short = any(n in spelling.split() for spelling in spellings for n in names)
        if not exact and not short:
            continue
        strong = any(len(n.split()) >= 2 and all(len(w)>1 for w in n.split()) for n in exact)
        hits.append((pid,bool(dob and dob==p['dob']),bool(number and number==p['number']),strong))
    for index in (1,2):
        supported = [h[0] for h in hits if h[index]]
        if supported:
            return supported[0] if len(supported)==1 else None
    return hits[0][0] if len(hits)==1 and hits[0][3] else None


def sheet_rows(data, game):
    lineups = data.get('lineups')
    if lineups == []:
        raise FeedError('pending')
    if not isinstance(lineups,list) or len(lineups)!=2:
        raise FeedError('incomplete')
    result = {}
    seen = set()
    teams = {game[s]['team_id'] for s in ('home','away')}
    for side in lineups:
        tid = identifier(side['team_id'])
        if tid not in teams or tid in result or len(side['starting_xi']) != 11:
            raise FeedError('incomplete')
        rows = []
        for field in ('starting_xi','substitutes_used','unused_substitutes'):
            for raw in side.get(field) or []:
                p = bio(raw)
                if p['id'] in seen:
                    raise FeedError('incomplete')
                seen.add(p['id'])
                rows.append(dict(p,verdict='starting' if field=='starting_xi' else 'benched'))
        result[tid] = rows
    return result


def public_result_partial(endpoint, args, season, data, empty_sheet):
    partial = empty_sheet
    if endpoint == 'list_games':
        games = data['games']
        if not isinstance(games, list) or data.get('next_offset') is not None:
            raise FeedError('incomplete')
        if len({identifier(g['game_id']) for g in games}) != len(games):
            raise FeedError('incomplete')
        for game in games:
            starts = datetime.fromisoformat(game['starts_at'].replace('Z','+00:00'))
            if (starts.tzinfo is None or starts.astimezone(timezone.utc).date().isoformat() != args['date'] or
                    not all(identifier(game[side]['team_id']) and game[side]['name'] for side in ('home','away'))):
                raise FeedError('incomplete')
        partial = not games
    elif endpoint == 'get_squad':
        entries = data['players']
        if (data['team_id'] != args['team'] or data['season'] != season or not entries or
                len(entries) != data['count'] or len({bio(row)['id'] for row in entries}) != len(entries)):
            raise FeedError('incomplete')
    elif not empty_sheet:
        game = data['game']
        if game['game_id'] != args['game_id']:
            raise FeedError('fixture_unmatched')
        if data.get('source') != 'live':
            raise FeedError('pending')
        sheet_rows(data, game)
    return partial


class Striker:
    def __init__(self, client, key, store):
        self.client = client
        self.key = key
        self.store = store
        self.cache = OrderedDict()
        self.lock = asyncio.Lock()
        self.pending_settlement = None
        self.pending_save = None
        self.retry_at = 0
        self.retry_status = 'rate_limited'

    async def settle(self):
        reservation, used = self.pending_settlement
        await self.store.rpc('settle_striker_call', p_id=reservation, p_used=used)
        self.pending_settlement = None

    async def adjust_charge(self, reservation, used):
        self.pending_settlement = (reservation, used)
        try:
            await self.settle()
        except Exception:
            # Keep the original reservation and retry before the next paid read.
            pass

    async def save_pending(self):
        saved = await self.store.rpc('save_striker_response', **self.pending_save)
        if saved is not True and saved is not False:
            raise FeedError('unavailable')
        self.pending_save = None
        return saved

    def remember(self, key, data, observed, expires):
        self.cache[key] = (observed, data, expires)
        self.cache.move_to_end(key)
        while len(self.cache) > 44:
            self.cache.popitem(last=False)
        return data, observed

    async def read(self, league, endpoint, args, ttl, season=None):
        lookup = str(next(iter(args.values())))
        if season is not None:
            lookup += ':' + str(season)
        key = (league, endpoint, lookup)
        async with self.lock:
            if self.pending_save is not None:
                await self.save_pending()
            now = time.time()
            cached = self.cache.get(key)
            if cached and now < cached[2]:
                return cached[1], cached[0]
            if self.pending_settlement:
                await self.settle()
            claim_args = dict(p_league=league, p_endpoint=endpoint, p_lookup=lookup, p_fetch=now >= self.retry_at)
            deadline = time.monotonic() + 6
            while True:
                claim = await self.store.rpc('claim_striker_response', **claim_args)
                status = claim['status']
                if status != 'busy':
                    break
                if time.monotonic() >= deadline:
                    raise FeedError('unavailable')
                await asyncio.sleep(1)
            if status == 'hit':
                return self.remember(key, claim['data'], float(claim['observed']), float(claim['expires']))
            if status != 'fetch':
                raise FeedError('monthly_limit' if status == 'monthly_limit' else
                                self.retry_status if now < self.retry_at else 'unavailable')
            reservation = claim['reservation']
            async with self.client.stream('POST', BASE+endpoint, json=dict(league=league,**args),
                    headers={'x-api-key':self.key}, follow_redirects=False, timeout=5) as response:
                used = response.headers.get('x-credits-used', '')
                expected = 5 if endpoint == 'get_lineups' else 2
                charged = int(used) if used.isascii() and used.isdigit() else None
                if charged is not None and charged != expected:
                    await self.adjust_charge(reservation, charged)
                if response.status_code in (401,402,403,429):
                    self.retry_status = {401:'access_denied',402:'credits_exhausted',403:'access_denied',
                                         429:'rate_limited'}[response.status_code]
                    retry = response.headers.get('retry-after','')
                    self.retry_at = now+(min(86400,max(60,int(retry))) if retry.isdigit() else
                                         3600 if response.status_code!=429 else 300)
                    raise FeedError(self.retry_status)
                if response.status_code!=200:
                    raise FeedError('unavailable')
                content = bytearray()
                async for chunk in response.aiter_bytes():
                    content.extend(chunk)
                    if len(content)>512*1024:
                        raise FeedError('incomplete')
            data = json.loads(content)['data']
            if not isinstance(data,dict):
                raise FeedError('incomplete')
            # The published endpoint contract charges zero for an unpublished team sheet.
            if endpoint == 'get_lineups' and not isinstance(data.get('lineups'), list):
                raise FeedError('incomplete')
            empty_sheet = endpoint == 'get_lineups' and data['lineups'] == []
            if charged is None and empty_sheet:
                await self.adjust_charge(reservation, 0)
            partial = public_result_partial(endpoint, args, season, data, empty_sheet)
            self.pending_save = dict(p_reservation=reservation,p_observed=now,p_partial=partial,p_data=data)
            if not await self.save_pending():
                raise FeedError('unavailable')
            return self.remember(key, data, now, now + (120 if partial else ttl))

    async def check(self, competition, fixture, local, clubs):
        league = LEAGUES.get(competition)
        if not self.key or not league:
            return {'status':'not_configured' if not self.key else 'unsupported'}
        progress = {}
        try:
            return await asyncio.wait_for(self._check(league,fixture,local,clubs,progress),20)
        except FeedError as error:
            return dict(progress,status=error.args[0])
        except (httpx.HTTPError,asyncio.TimeoutError):
            return dict(progress,status='unavailable')
        except (KeyError,TypeError,ValueError,AttributeError):
            return dict(progress,status='incomplete')

    async def _check(self, league, fixture, local, clubs, progress):
        fid = fixture.get('fixture_id')
        if not fid:
            date = datetime.fromtimestamp(fixture['unix'],timezone.utc).date().isoformat()
            schedule,_ = await self.read(league,'list_games',{'date':date},86400)
            matches = [g for g in schedule['games'] if same_fixture(g,fixture,clubs)]
            if len(matches)!=1:
                raise FeedError('fixture_unmatched')
            fid = identifier(matches[0]['game_id'])
        progress['fixture_id'] = identifier(fid)
        evidence = fixture.get('evidence')
        if not evidence:
            data,observed = await self.read(league,'get_lineups',{'game_id':fid},3*3600)
            if data['lineups'] == [] and not data.get('game'):
                raise FeedError('pending')
            game = data['game']
            if game['game_id']!=fid or not same_fixture(game,fixture,clubs):
                raise FeedError('fixture_unmatched')
            if not data.get('lineups') or (observed<fixture['unix'] and data.get('source')!='live') or (
                    data.get('source') in ('predicted','projected','expected')):
                raise FeedError('pending')
            try:
                sheets = sheet_rows(data, game)
            except (FeedError, KeyError, TypeError, ValueError):
                self.cache.pop((league, 'get_lineups', fid), None)
                raise
            evidence = dict(observed=observed,season=game['season'],teams={
                s:identifier(game[s]['team_id']) for s in ('home','away')},rows=sheets)
        progress.update(evidence=evidence,observed=evidence['observed'])
        season = evidence['season']
        updated, verdicts, unresolved = {}, {}, []
        progress['clubs'] = updated
        for side,tid in evidence['teams'].items():
            cid = fixture[side+'_id']
            prior = clubs.get(cid) or {}
            if prior.get('team_id')!=tid or prior.get('season_id')!=season:
                prior = {}
            roster,checked = prior.get('roster') or [], prior.get('checked') or 0
            if not roster or time.time()-checked>=7*86400:
                squad,checked = await self.read(league,'get_squad',{'team':tid},7*86400,season=season)
                entries = squad['players']
                if (squad['team_id']!=tid or squad['season']!=season or not entries or
                        len(entries)!=squad['count']):
                    raise FeedError('incomplete')
                roster = [bio(row) for row in entries]
            pool = {p['id']:dict(p) for p in roster}
            if len(pool)!=len(roster):
                raise FeedError('incomplete')
            sheet = {}
            for p in evidence['rows'][tid]:
                pid = p['id']
                sheet[pid] = p['verdict']
                record = pool.setdefault(pid,dict(p))
                record['sheet_name']=p['name']
                record['number']=p['number'] or record['number']
            old = prior.get('links') or {}
            players = [p for p in local if p['teamId']==cid]
            proposed = {p['id']:identify(p,pool,old.get(p['id'])) for p in players}
            counts = Counter(proposed.values())
            links = {k:v for k,v in proposed.items() if v and counts[v]==1}
            unresolved.extend(p['id'] for p in players if p['id'] not in links)
            updated[cid]=dict(team_id=tid,season_id=season,roster=roster,checked=checked,links=links)
            for key,pid in links.items():
                if any(pid==p['id'] for t,rows in evidence['rows'].items() if t!=tid for p in rows):
                    unresolved.append(key)
                else:
                    verdicts[key]=sheet.get(pid,'absent')
        return dict(progress,status='confirmed',clubs=updated,players=verdicts,unresolved=unresolved)
