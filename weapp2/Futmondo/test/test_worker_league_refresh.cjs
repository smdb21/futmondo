const assert = require('node:assert/strict');
const fs = require('node:fs');
const { PGlite } = require((process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules') + '/@electric-sql/pglite');

(async () => {
  const db = new PGlite();
  const query = async (sql, args = []) => (await db.query(sql, args)).rows;
  try {
    await db.exec('CREATE ROLE anon; CREATE ROLE authenticated; CREATE ROLE service_role BYPASSRLS;');
    const migration = fs.readFileSync('scripts/migrations/20261122120000_worker_league_refresh.sql', 'utf8');
    const schema = fs.readFileSync('scripts/schema.sql', 'utf8');
    assert.ok(schema.endsWith(migration));
    await db.exec(schema.slice(0, -migration.length));
    await db.exec(migration);
    const scope = JSON.stringify({
      league: { competition_id: 'competition', season: 'season', scoring_version: 'score' },
      other: { competition_id: 'competition', season: 'season', scoring_version: 'score' }
    });
    await query(`INSERT INTO worker_accounts(account_id, enabled, auth_status, verified_leagues, encrypted_token, credential_id)
      VALUES ('mine', true, 'active', $1, 'fixture', 'fixture-credential'), ('foreign', true, 'active', $1, 'fixture', 'fixture-credential')`, [scope]);
    await db.exec("INSERT INTO worker_leader(id,owner,lease_until) VALUES(1,'test',now()+interval '10 minutes')");
    async function add(id, handler, { account = 'mine', league = 'league', team = 'team', player = '', season = 'season' } = {}) {
      const context = { account_id: account, championship_id: league, user_team_id: team,
        player_id: player, competition_id: 'competition', season, scoring_version: 'score' };
      const key = id.toString(16).padStart(64, '0');
      await query(`INSERT INTO worker_jobs(account_id,resource_key,descriptor,generation,due_at)
        VALUES($1,$2,$3,1,now()+interval '30 minutes')`, [account, key, { handler, context, args: {}, resource_key: key }]);
      return key;
    }
    const team = await add(1, 'get_user_team_info');
    const market = await add(2, 'get_market_players');
    const roster = await add(3, 'get_players_from_team');
    const history = await add(4, 'get_championship_pressroom', { team: '' });
    const catalog = await add(5, 'get_championship_players', { team: '' });
    const finance = await add(6, 'get_financial_snapshot');
    const player = await add(7, 'get_player_summary', { player: 'bought' });
    const pending = await add(8, 'get_player_summary', { player: 'pending' });
    const untouched = [
      await add(9, 'get_user_team_info', { team: 'rival' }),
      await add(10, 'get_user_team_info', { league: 'other' }),
      await add(11, 'get_user_team_info', { account: 'foreign' }),
      await add(12, 'get_championship_players', { team: '', season: 'old-season' }),
      await add(13, 'get_finished_rounds', { team: '' })
    ];
    await query("UPDATE worker_jobs SET owner='test:old',started_at=now()-interval '1 second',lease_until=now()+interval '1 minute' WHERE resource_key=$1", [roster]);
    await query("UPDATE worker_jobs SET last_error='source_rate_limited' WHERE resource_key=$1", [history]);
    await query(`INSERT INTO worker_jobs(account_id,resource_key,descriptor,generation,due_at)
      SELECT 'mine',lpad(to_hex(i),64,'0'),jsonb_build_object('handler','get_player_full_profile',
      'context',jsonb_build_object('championship_id','league')),1,now()+interval '30 minutes'
      FROM generate_series(100,5099) i`);
    const privileges = await query(`SELECT has_function_privilege('anon','worker_league_refresh(text,text,text,text[],boolean,timestamptz)','execute') anon,
      has_function_privilege('service_role','worker_league_refresh(text,text,text,text[],boolean,timestamptz)','execute') service`);
    assert.deepEqual(privileges, [{ anon: false, service: true }]);
    await db.exec('SET ROLE service_role');
    const refresh = async (players = [], core = true, at = null) => (await query(
      "SELECT worker_league_refresh('mine','league','team',$1,$2,$3) count", [players, core, at]))[0].count;
    const row = async key => (await query(`SELECT resource_key,owner,run_id,league_refresh_at,requested_at,
      league_refresh_at::text request_text,due_at<=now() due,last_error FROM worker_jobs WHERE resource_key=$1`, [key]))[0];
    const start = performance.now();
    assert.equal(await refresh(), 6);
    const elapsed = performance.now() - start;
    assert.equal(await refresh(), 1); // Immediate repeated click reuses the queued request.
    for (const key of [team, market, catalog, finance]) assert.equal((await row(key)).due, true);
    assert.equal((await row(roster)).owner, 'test:old');
    assert.equal((await row(history)).last_error, 'source_rate_limited');
    assert.equal((await row(history)).due, false);
    for (const key of [...untouched, player, pending]) assert.equal((await row(key)).league_refresh_at, null);
    assert.equal(await refresh(['bought']), 7);
    const requested = (await row(player)).request_text;
    assert.equal(await refresh(['pending'], false, requested), 1);
    assert.equal(await refresh(['pending'], false, requested), 0);
    const claimed = (await query("SELECT worker_claim('test:next',true) job"))[0].job;
    assert.ok([team, market, catalog, player, pending].includes(claimed.resource_key));
    assert.ok(claimed.league_refresh_at);
    await query('UPDATE worker_jobs SET run_id=gen_random_uuid() WHERE resource_key=$1', [roster]);
    assert.ok((await row(roster)).league_refresh_at, 'older leased work preserves the newer refresh');
    await query('UPDATE worker_jobs SET run_id=gen_random_uuid() WHERE resource_key=$1', [claimed.resource_key]);
    assert.equal((await row(claimed.resource_key)).league_refresh_at, null);
    await query("UPDATE worker_accounts SET excluded_leagues='[\"league\"]' WHERE account_id='mine'");
    assert.equal(await refresh(['bought']), 0);
    console.log(`PASS scoped queue, deduplication, leases, backoff, next claim and consumption. One six-job request with 5,000 unrelated jobs took ${elapsed.toFixed(1)}ms.`);
  } finally {
    await db.close();
  }
})().catch(error => { console.error(error.message); process.exitCode = 1; });
