const assert = require('node:assert/strict');
const fs = require('node:fs');
const { PGlite } = require((process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules') + '/@electric-sql/pglite');

(async () => {
  const db = new PGlite();
  const query = async (sql, args = []) => (await db.query(sql, args)).rows;
  try {
    await db.exec('CREATE ROLE anon; CREATE ROLE authenticated; CREATE ROLE service_role BYPASSRLS;');
    const migration = fs.readFileSync('scripts/migrations/20261123071652_reconcile_active_manual_offers.sql', 'utf8');
    const schema = fs.readFileSync('scripts/schema.sql', 'utf8');
    assert.ok(schema.includes(migration));
    await db.exec(schema.replace(migration, ''));
    await db.exec(migration);
    await db.exec("INSERT INTO worker_accounts(account_id) VALUES('fixture')");
    const job = 'f'.repeat(32), market = 'a'.repeat(64), balance = 'b'.repeat(64);
    const context = { account_id: 'fixture', championship_id: 'league', user_team_id: 'mine',
      player_id: '', competition_id: 'competition', season: 'season', scoring_version: 'score' };
    await query(`INSERT INTO clause_jobs(id,account_id,context,kind,mode,player_id,player_slug,player_name,original_owner,
      max_price,reserved_amount,reserved_slot,opens_at,due_at,expires_at,state,dispatched_at)
      VALUES($1,'fixture',$2,'manual','bid','player','player','Player','',38016571,38016571,1,now(),now(),now(),
      'accepted',now()-interval '10 minutes')`, [job,{...context,player_id:'player'}]);
    async function snapshot(key, handler, age) {
      return (await query(`INSERT INTO worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,
        valid_until,completeness,status,json_bytes,sha256,parts)
        VALUES('fixture',$1,$2,$3,gen_random_uuid(),now()-$4::interval,now(),now(),'unknown','ok',1,repeat('c',64),1)
        RETURNING version`,[key,context,handler,age]))[0].version;
    }
    const mv = await snapshot(market,'get_market_players','5 minutes');
    const bv = await snapshot(balance,'get_user_team_info','12 minutes');
    const match = {job_id:job,player_id:'player',bid_id:'own-bid',price:38016571};
    const apply = async (scope=context, fact=match, version=mv) => (await query(
      'SELECT clause_reconcile_active_offers($1,$2,$3,$4,$5,$6) count',
      [scope,market,version,balance,bv,JSON.stringify([fact])]))[0].count;
    await db.exec('SET ROLE authenticated');
    await assert.rejects(apply(), /permission denied/);
    await db.exec('SET ROLE service_role');
    assert.equal(await apply(),0);
    await query("UPDATE worker_snapshots SET observed_at=now()-interval '4 minutes' WHERE resource_key=$1",[balance]);
    assert.equal(await apply({...context,user_team_id:'other'}),0);
    assert.equal(await apply(context,{...match,price:1}),0);
    assert.equal(await apply(context,{...match,bid_id:''}),0);
    assert.equal(await apply(context,match,bv),0);
    await query("UPDATE worker_snapshots SET completeness='partial' WHERE resource_key=$1",[market]);
    assert.equal(await apply(),0);
    await query("UPDATE worker_snapshots SET completeness='complete', observed_at=now()-interval '12 minutes' WHERE resource_key=$1",[market]);
    assert.equal(await apply(),0);
    await query("UPDATE worker_snapshots SET observed_at=now()-interval '5 minutes' WHERE resource_key=$1",[market]);
    assert.equal(await apply(),1);
    assert.equal(await apply(),0);
    const state = (await query('SELECT state,result_code,reserved_slot,reserved_amount FROM clause_jobs WHERE id=$1',[job]))[0];
    assert.deepEqual(state,{state:'succeeded',result_code:'active_offer_confirmed',reserved_slot:0,reserved_amount:'0'});
    console.log('PASS version, scope, freshness, exact offer, grants and one-time durable release');
  } finally { await db.close(); }
})().catch(error => { console.error(error.message); process.exitCode = 1; });
