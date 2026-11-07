// Isolated PostgreSQL checks; never reads hosting credentials or a live DB.
const assert=require('node:assert/strict');
const fs=require('node:fs');
const path=require('node:path');
const {PGlite}=require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules','@electric-sql/pglite'));
(async()=>{
 const db=new PGlite();let groups=0;const pass=s=>{groups++;console.log('PASS '+s)};
 try{
  await db.exec('CREATE ROLE anon;CREATE ROLE authenticated;CREATE ROLE service_role BYPASSRLS;');
  await db.exec(fs.readFileSync('scripts/schema.sql','utf8'));
  const migration=fs.readFileSync('scripts/migrations/20261107191054_lineup_snapshots.sql','utf8');
  await db.exec(migration);await db.exec(migration);
  pass('fresh schema and repeatable forward migration install lineup snapshots and page preference');
  assert.equal((await db.query("SELECT relrowsecurity enabled FROM pg_class WHERE oid='team_lineup_snapshots'::regclass")).rows[0].enabled,true);
  await db.exec('SET ROLE service_role');
  const insert=`INSERT INTO team_lineup_snapshots(account_id,championship_id,user_team_id,target_round,season,scoring_version,payload,observed_at,expires_at,complete)
   VALUES($1,'league','team','r6','season','score',$2,$3,$4,true)
   ON CONFLICT(account_id,championship_id,user_team_id,target_round,season,scoring_version) DO UPDATE SET payload=excluded.payload,observed_at=excluded.observed_at,expires_at=excluded.expires_at`;
  await db.query(insert,['a',{fixture:'new'},'2026-11-07T01:00:00Z','2026-11-07T01:00:30Z']);
  await db.query(insert,['a',{fixture:'old'},'2026-11-06T01:00:00Z','2026-11-06T01:00:30Z']);
  assert.equal((await db.query("SELECT payload->>'fixture' value FROM team_lineup_snapshots WHERE account_id='a'")).rows[0].value,'new');
  await db.query(insert,['b',{fixture:'other-account'},'2026-11-07T01:00:00Z','2026-11-07T01:00:30Z']);
  assert.equal((await db.query('SELECT count(*)::int n FROM team_lineup_snapshots')).rows[0].n,2);
  await db.exec("INSERT INTO team_lineup_snapshots SELECT account_id,championship_id,user_team_id,target_round,season,'other-score',payload,observed_at,expires_at,complete FROM team_lineup_snapshots WHERE account_id='a'");
  assert.equal((await db.query("SELECT count(*)::int n FROM team_lineup_snapshots WHERE account_id='a'")).rows[0].n,2);
  await db.exec("INSERT INTO user_page_preferences(user_id,page,visited_at)VALUES('fixture','lineup',now())");
  await assert.rejects(db.query(insert,['x',{big:'x'.repeat(524289)},'2026-11-07T01:00:00Z','2026-11-07T01:00:30Z']));
  await assert.rejects(db.exec("UPDATE team_lineup_snapshots SET complete=false"));
  pass('server role can cache isolated accounts, stale writes cannot replace newer data, and payload/completeness are bounded');
  for(const role of ['anon','authenticated']){
   await db.exec('RESET ROLE; SET ROLE '+role);
   await assert.rejects(db.query('SELECT * FROM team_lineup_snapshots'));
   await assert.rejects(db.query(insert,['x',{},'2026-11-07T01:00:00Z','2026-11-07T01:00:30Z']));
   await db.exec('RESET ROLE');
   assert.equal((await db.query("SELECT has_function_privilege($1,'public.keep_latest_team_lineup_snapshot()','execute') allowed",[role])).rows[0].allowed,false);
  }
  pass('browser roles cannot read, write or invoke privileged snapshot functions');
 }finally{await db.close()}
 console.log(`LINEUP SCHEMA: ${groups} groups passed / 0 failed`);
})().catch(e=>{console.error(e.stack);process.exitCode=1});
