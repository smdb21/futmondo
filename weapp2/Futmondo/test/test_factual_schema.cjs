// Isolated PostgreSQL checks for the factual schema and existing-install upgrade.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const { PGlite } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules', '@electric-sql/pglite'));
const read = file => fs.readFileSync(file,'utf8');
const indexes = read('scripts/migrations/20261027034734_factual_history_indexes.sql');
const retirement = read('scripts/migrations/20261027034735_retire_prediction_automation.sql');
const roles = 'CREATE ROLE anon; CREATE ROLE authenticated; CREATE ROLE service_role BYPASSRLS;';
let groups = 0;
const pass = text => {groups++;console.log('PASS '+text);};
async function counts(db) {
  return (await db.query("SELECT count(*)::int AS rows, count(*) FILTER(WHERE observed_at IS NULL)::int AS unknown FROM player_history")).rows[0];
}
(async () => {
  const fresh = new PGlite();
  try {
    await fresh.exec(roles);
    await fresh.exec(read('scripts/schema.sql'));
    await fresh.exec(read('scripts/schema.sql'));
    const names=(await fresh.query("SELECT tablename FROM pg_tables WHERE schemaname='public'")).rows.map(x=>x.tablename);
    assert.ok(names.includes('player_match_observations'));
    assert.ok(!names.some(x=>/automation|forecast|model_evaluations|transfer_scenarios|manager_dna|decision_log|smart_alerts/.test(x)));
    pass('fresh schema is repeatable and creates no excluded feature tables');
    await fresh.exec("SET ROLE service_role; INSERT INTO championships(id,name) VALUES('c','League'); INSERT INTO players(id,name,slug) VALUES('p','Player','player'); INSERT INTO user_teams(id,championship_id,name) VALUES('t','c','Team');");
    await fresh.exec("INSERT INTO player_history(player_id,championship_id,value,points,observed_at) VALUES('p','c',100,9,'2026-10-01') ON CONFLICT(player_id,championship_id,observed_at) DO NOTHING;");
    await fresh.exec("INSERT INTO player_history(player_id,championship_id,value,points,observed_at) VALUES('p','c',100,9,'2026-10-01') ON CONFLICT(player_id,championship_id,observed_at) DO NOTHING;");
    assert.equal((await counts(fresh)).rows,1);
    pass('server role can persist replay-idempotent observed price history');
    await fresh.exec("INSERT INTO player_match_observations(player_id,championship_id,season,scoring_version,round,points,score_status,observed_at) VALUES('p','c','2026','s',1,0,'final','2026-10-01'),('p','c','2026','s',2,-2,'final','2026-10-02'),('p','c','2026','s',2,-1,'final','2026-10-03'),('p','c','2025','s',2,15,'final','2025-10-03');");
    const scores=(await fresh.query("SELECT DISTINCT ON(round) round::int,points::int FROM player_match_observations WHERE championship_id='c' AND player_id='p' AND season='2026' AND scoring_version='s' ORDER BY round,observed_at DESC")).rows;
    assert.deepEqual(scores,[{round:1,points:0},{round:2,points:-1}]);
    assert.equal((await fresh.query("SELECT count(*)::int AS n FROM player_match_observations")).rows[0].n,4);
    pass('round corrections preserve history, negative/zero scores and season isolation');
    await assert.rejects(fresh.exec("INSERT INTO user_team_history(user_team_id,championship_id,points,budget) VALUES('t','other',1,NULL)"));
    await fresh.exec("INSERT INTO user_team_history(user_team_id,championship_id,points,budget) VALUES('t','c',1,NULL)");
    pass('team history rejects league mismatch and preserves unknown cash');
    await fresh.exec('RESET ROLE; SET ROLE anon');
    await assert.rejects(fresh.query('SELECT * FROM player_match_observations'));
    await fresh.exec('RESET ROLE; SET ROLE authenticated');
    await assert.rejects(fresh.query('SELECT * FROM player_history'));
    await fresh.exec('RESET ROLE');
    pass('browser roles cannot read server-only history');
    await fresh.exec(indexes);
    await fresh.exec(retirement);
    await fresh.exec(retirement);
    pass('forward migrations tolerate a fresh installation and repeated execution');
  } finally { await fresh.close(); }
  const upgrade = new PGlite();
  try {
    await upgrade.exec(roles);
    for(const file of ['test/fixtures/legacy_schema_before_redesign.sql','scripts/migrations/20260905_reliable_insights.sql','scripts/migrations/20260907_private_history.sql','scripts/migrations/20260907_observation_lifecycle.sql']) await upgrade.exec(read(file));
    await upgrade.exec("INSERT INTO championships(id,name) VALUES('c','League'); INSERT INTO players(id,name,slug) VALUES('p','Player','player'); INSERT INTO user_teams(id,championship_id,name) VALUES('t','c','Team'); INSERT INTO player_history(player_id,championship_id,value,observed_at) VALUES('p','c',100,NULL),('p','c',120,'2026-10-01'); INSERT INTO manager_dna_profiles(team_id,championship_id,total_trades) VALUES('t','c',3);");
    const before=await counts(upgrade);
    await upgrade.exec(indexes);
    assert.deepEqual(await counts(upgrade),before);
    pass('additive migration preserves factual rows and unknown provenance');
    const rolledBack=retirement.replace(/COMMIT;\s*$/,'ROLLBACK;');
    await upgrade.exec(rolledBack);
    assert.equal((await upgrade.query("SELECT to_regclass('public.manager_dna_profiles')::text AS name")).rows[0].name,'manager_dna_profiles');
    assert.equal((await upgrade.query("SELECT to_regprocedure('public.claim_automation_job()') IS NOT NULL AS present")).rows[0].present,true);
    pass('transaction rollback restores pre-retirement objects without data loss');
    await upgrade.exec(retirement);
    assert.deepEqual(await counts(upgrade),before);
    assert.equal((await upgrade.query("SELECT total_trades FROM retired_features.manager_dna_profiles")).rows[0].total_trades,3);
    assert.equal((await upgrade.query("SELECT to_regclass('public.manager_dna_profiles') IS NULL AS absent")).rows[0].absent,true);
    assert.equal((await upgrade.query("SELECT count(*)::int AS n FROM pg_proc p JOIN pg_namespace n ON n.oid=p.pronamespace WHERE n.nspname='public' AND p.proname LIKE '%automation%'")).rows[0].n,0);
    pass('retirement preserves archived records and removes executable automation RPCs');
    await upgrade.exec(retirement);
    assert.equal((await upgrade.query("SELECT total_trades FROM retired_features.manager_dna_profiles")).rows[0].total_trades,3);
    for(const role of ['anon','authenticated','service_role']){
      await upgrade.exec('SET ROLE '+role);
      await assert.rejects(upgrade.query('SELECT * FROM retired_features.manager_dna_profiles'));
      await upgrade.exec('RESET ROLE');
    }
    pass('retirement is repeatable and archived feature data is inaccessible to API roles');
  } finally {await upgrade.close();}
  console.log('FACTUAL SCHEMA: '+groups+' groups passed / 0 failed');
})().catch(error=>{console.error(error.stack);process.exitCode=1;});
