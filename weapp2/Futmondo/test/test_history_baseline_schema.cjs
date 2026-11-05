// Offline PostgreSQL verification of complete-history snapshot storage.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const { PGlite } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-history-schema-tools/node_modules','@electric-sql/pglite'));
const migration = fs.readFileSync('scripts/migrations/20261105201845_complete_history_baselines.sql','utf8');
const roles = 'CREATE ROLE anon; CREATE ROLE authenticated; CREATE ROLE service_role BYPASSRLS;';
let groups=0;
const pass = label => {groups++;console.log('PASS '+label);};
const insert = `INSERT INTO public.complete_history_baselines(account_id,championship_id,season,scoring_version,resource,encoding,payload,payload_sha256,json_bytes,snapshot_hash,observed_at)
 VALUES('fixture-account','fixture-league','season','score','pressroom_fixture-league','json-gzip-base64-v1',$1,$2,$3,$2,$4)
 ON CONFLICT(account_id,championship_id,season,scoring_version,resource) DO UPDATE SET payload=EXCLUDED.payload,observed_at=EXCLUDED.observed_at`;
(async()=>{
  const db=new PGlite();
  try {
    await db.exec(roles);
    await db.exec(migration);await db.exec(migration);
    pass('forward migration is repeatable without existing factual tables');
    assert.equal((await db.query("SELECT relrowsecurity AS enabled FROM pg_class WHERE oid='public.complete_history_baselines'::regclass")).rows[0].enabled,true);
    await db.exec('SET ROLE service_role');
    await db.query(insert,['newer','a'.repeat(64),5,'2026-10-02T00:00:00Z']);
    await db.query(insert,['older','a'.repeat(64),5,'2026-10-01T00:00:00Z']);
    assert.equal((await db.query('SELECT payload FROM public.complete_history_baselines')).rows[0].payload,'newer');
    await db.query(insert,['newest','a'.repeat(64),6,'2026-10-03T00:00:00Z']);
    assert.equal((await db.query('SELECT payload FROM public.complete_history_baselines')).rows[0].payload,'newest');
    assert.equal((await db.query('SELECT count(*)::int AS n FROM public.complete_history_baselines')).rows[0].n,1);
    pass('atomic upserts replace one baseline and reject older concurrent snapshots');
    await assert.rejects(db.query(insert,['x','a'.repeat(64),16777217,'2026-10-04T00:00:00Z']));
    await assert.rejects(db.query(insert,['x'.repeat(921601),'a'.repeat(64),1,'2026-10-04T00:00:00Z']));
    await assert.rejects(db.exec("UPDATE public.complete_history_baselines SET resource='pressroom_other'"));
    pass('database enforces payload, decompressed-size and resource scope bounds');
    for(const role of ['anon','authenticated']) {
      await db.exec('RESET ROLE; SET ROLE '+role);
      await assert.rejects(db.query('SELECT * FROM public.complete_history_baselines'));
      await assert.rejects(db.query(insert,['x','a'.repeat(64),1,'2026-10-04T00:00:00Z']));
      await db.exec('RESET ROLE');
      assert.equal((await db.query("SELECT has_function_privilege($1,'public.keep_newest_complete_history_baseline()','execute') AS allowed",[role])).rows[0].allowed,false);
    }
    pass('browser roles cannot read, write or invoke the trigger function');
  } finally {await db.close();}
  const fresh=new PGlite();
  try {
    await fresh.exec(roles);
    await fresh.exec(fs.readFileSync('scripts/schema.sql','utf8'));
    await fresh.exec(migration);await fresh.exec(migration);
    assert.equal((await fresh.query("SELECT to_regclass('public.complete_history_baselines') IS NOT NULL AS present")).rows[0].present,true);
    pass('fresh application schema includes the private baseline table and tolerates its migration');
  } finally {await fresh.close();}
  console.log('HISTORY BASELINE SCHEMA: '+groups+' groups passed / 0 failed');
})().catch(error=>{console.error(error.stack);process.exitCode=1;});
