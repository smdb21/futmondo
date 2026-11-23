const assert = require('node:assert/strict');
const fs = require('node:fs');
const { PGlite } = require((process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules') + '/@electric-sql/pglite');

(async () => {
  const db = new PGlite();
  const query = async (sql, args = []) => (await db.query(sql, args)).rows;
  try {
    await db.exec('CREATE ROLE anon; CREATE ROLE authenticated; CREATE ROLE service_role BYPASSRLS;');
    const migration = fs.readFileSync('scripts/migrations/20261122130000_reconcile_completed_market_bids.sql', 'utf8');
    const schema = fs.readFileSync('scripts/schema.sql', 'utf8');
    const functions = migration.slice(migration.indexOf('CREATE FUNCTION'), migration.indexOf('NOTIFY pgrst'));
    assert.ok(schema.includes(functions));
    await db.exec(schema.replace(functions, ''));
    await db.exec(migration);
    await db.exec("INSERT INTO worker_accounts(account_id) VALUES('fixture')");
    const job = 'f'.repeat(32);
    const history = 'a'.repeat(64), balance = 'b'.repeat(64);
    const context = { account_id: 'fixture', championship_id: 'league', user_team_id: 'mine',
      player_id: '', competition_id: 'league-competition', season: 'season', scoring_version: 'score' };
    const times = (await query("SELECT now()-interval '10 minutes' dispatched, now()-interval '5 minutes' purchased, now()-interval '4 minutes' snapshot"))[0];
    await query(`INSERT INTO clause_jobs(id,account_id,context,kind,mode,player_id,player_slug,player_name,original_owner,
      max_price,reserved_amount,reserved_slot,opens_at,due_at,expires_at,state,dispatched_at)
      VALUES($1,'fixture',$2,'manual','bid','player','player','Player','',3210413,3210413,1,now(),now(),now(),'accepted',$3)`,
      [job,{...context,player_id:'player'},times.dispatched]);
    async function snapshot(key, handler, scope, observed) {
      return (await query(`INSERT INTO worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,
        valid_until,completeness,status,json_bytes,sha256,parts)
        VALUES('fixture',$1,$2,$3,gen_random_uuid(),$4,$4,now(),'complete','ok',1,repeat('c',64),1) RETURNING version`,
        [key,scope,handler,observed]))[0].version;
    }
    const hv = await snapshot(history,'get_championship_pressroom',{...context,user_team_id:''},times.snapshot);
    const bv = await snapshot(balance,'get_user_team_info',context,times.dispatched);
    const tx = { job_id:job, player_id:'player', buyer_team_id:'mine', seller_team_id:'', price:3210413, created:times.purchased };
    const apply = async (scope = context, fact = tx) => (await query(
      'SELECT clause_reconcile_completed_bids($1,$2,$3,$4,$5,$6) count',[scope,history,hv,balance,bv,JSON.stringify([fact])]))[0].count;
    await db.exec('SET ROLE service_role');
    assert.equal(await apply(),0);
    await query('UPDATE worker_snapshots SET observed_at=$1 WHERE resource_key=$2',[times.snapshot,balance]);
    assert.equal(await apply({...context,season:'foreign'}),0);
    assert.equal(await apply(context,{...tx,buyer_team_id:'other'}),0);
    assert.equal(await apply(context,{...tx,price:25}),0);
    assert.equal(await apply(),1);
    assert.equal(await apply(),0);
    const state = (await query('SELECT state,result_code,reserved_slot,reserved_amount FROM clause_jobs WHERE id=$1',[job]))[0];
    assert.deepEqual(state,{ state:'succeeded',result_code:'market_purchase_confirmed',reserved_slot:0,reserved_amount:'0' });
    console.log('PASS saved market win releases durable cash and slot once, with stale balances and foreign evidence rejected');
  } finally { await db.close(); }
})().catch(error => { console.error(error.message); process.exitCode = 1; });
