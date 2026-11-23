const assert = require('node:assert/strict');
const fs = require('node:fs');
const { PGlite } = require((process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules') + '/@electric-sql/pglite');

(async () => {
  const db = new PGlite();
  try {
    await db.exec("CREATE ROLE anon; CREATE ROLE authenticated; CREATE ROLE service_role BYPASSRLS; CREATE TABLE market_transactions(id bigint);");
    for (const name of ['20261111214239_background_acquisition', '20261115075411_scheduled_clause_purchases', '20261123064300_manual_offer_dispatch']) {
      await db.exec(fs.readFileSync('scripts/migrations/' + name + '.sql', 'utf8'));
    }
    const context = {account_id:'account', championship_id:'league', user_team_id:'team',
      competition_id:'competition', season:'season', scoring_version:'version'};
    const dispatch = async (id, player, price = 40, capacity = null, scope = context) => {
      const result = await db.query('SELECT clause_dispatch_manual_offer($1,$2,$3,$4,$5,$6) AS result',
        [id.toString(16).padStart(32,'0'), {...scope, player_id:player}, 'bid', {id:player}, price, capacity]);
      return result.rows[0].result;
    };
    const state = (id, outcome) => db.query('SELECT clause_settle($1,$2,$3,$4)',
      [id.toString(16).padStart(32,'0'), '', outcome, 'fixture']);
    assert.equal(await dispatch(1, 'first'), 'dispatched');
    assert.equal(await dispatch(2, 'second'), 'clause_team_busy');
    await state(1, 'uncertain');
    assert.equal(await dispatch(3, 'second'), 'clause_team_busy');
    assert.equal(await dispatch(4, 'isolated', 40, null, {...context, user_team_id:'other'}), 'dispatched');
    await state(1, 'failed');
    assert.equal(await dispatch(1, 'first'), 'clause_request_conflict');
    await db.exec("UPDATE clause_jobs SET kind='scheduled', state='scheduled' WHERE player_id='first';" +
      "UPDATE clause_jobs SET reserved_amount=50, reserved_slot=1 WHERE player_id='first';");
    assert.equal(await dispatch(5, 'first'), 'clause_team_busy');
    assert.equal(await dispatch(6, 'new'), 'clause_reservation_capacity_unavailable');
    const capacity = {status:'ok', valid_until:Date.now()/1000 + 10,
      roster:{status:'ok', effective_count:16, cap:20}, outstanding:{count:0},
      funds:{spendable_budget:100, api_bid_limit:100}};
    assert.equal(await dispatch(7, 'new', 51, capacity), 'clause_offer_funds');
    assert.equal(await dispatch(8, 'new', 40, {...capacity, valid_until:1}), 'clause_reservation_capacity_unavailable');
    assert.equal(await dispatch(9, 'new', 40, {...capacity, roster:{status:'ok', effective_count:19, cap:20}}), 'clause_offer_capacity');
    assert.equal(await dispatch(10, 'new', 40, capacity), 'dispatched');
    const result = await db.query("SELECT has_function_privilege('anon','clause_dispatch_manual_offer(text,jsonb,text,jsonb,numeric,jsonb)','execute') AS allowed, (SELECT count(*)::integer FROM clause_attempts WHERE job_id=repeat('0',31)||'a') AS attempts");
    assert.deepEqual(result.rows, [{allowed:false, attempts:1}]);
    console.log('PASS local PostgreSQL dispatch, duplicate suppression, team isolation, reservation limits, native submission without capacity and service-only access');
  } finally {
    await db.close();
  }
})().catch(error => { console.error(error.message); process.exitCode = 1; });
