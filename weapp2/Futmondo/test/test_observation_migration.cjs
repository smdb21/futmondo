// Isolated PostgreSQL compatibility test. Never connects to Supabase.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const { PGlite } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules', '@electric-sql/pglite'));
(async () => {
  const db = new PGlite();
  await db.exec('CREATE ROLE anon; CREATE ROLE authenticated; CREATE ROLE service_role BYPASSRLS;');
  for (const file of ['scripts/schema.sql','scripts/migrations/20260905_reliable_insights.sql',
    'scripts/migrations/20260907_private_history.sql','scripts/migrations/20260907_observation_lifecycle.sql']) {
    await db.exec(fs.readFileSync(file,'utf8'));
  }
  console.log('PASS schema and all migrations execute in isolated PostgreSQL');
  const insert = `INSERT INTO auction_observations(auction_id,championship_id,player_id,winning_amount,observed_at,outcome)
    VALUES ('a','c','p',$1,$2,'sold') ON CONFLICT(championship_id,auction_id)
    DO UPDATE SET winning_amount=EXCLUDED.winning_amount,observed_at=EXCLUDED.observed_at`;
  await db.query(insert,[100,'2026-09-01T00:00:00Z']);
  await db.query(insert,[100,'2026-09-02T00:00:00Z']);
  let row=(await db.query('SELECT * FROM auction_observations')).rows[0];
  assert.equal(new Date(row.first_observed_at).toISOString(),'2026-09-01T00:00:00.000Z');
  assert.equal(new Date(row.observed_at).toISOString(),'2026-09-01T00:00:00.000Z');
  assert.equal(new Date(row.last_seen_at).toISOString(),'2026-09-02T00:00:00.000Z');
  console.log('PASS repeated captures retain first knowledge and update last seen');
  await db.query(insert,[150,'2026-09-03T00:00:00Z']);
  row=(await db.query(`SELECT * FROM auction_observation_history WHERE observed_at<'2026-09-02' ORDER BY observed_at DESC LIMIT 1`)).rows[0];
  assert.equal(Number(row.winning_amount),100);
  await db.query(insert,[90,'2026-08-01T00:00:00Z']);
  assert.equal(Number((await db.query('SELECT winning_amount FROM auction_observations')).rows[0].winning_amount),150);
  console.log('PASS revisions preserve historical replay and reject stale overwrites');
  await db.exec(fs.readFileSync('scripts/migrations/20260907_observation_lifecycle.sql','utf8'));
  assert.equal((await db.query('SELECT * FROM observation_revisions')).rows.length,1);
  console.log('PASS migration reapplication preserves data');
  await db.exec(`INSERT INTO auction_observations(auction_id,championship_id,player_id,observed_at,legacy_timing,outcome)
    VALUES ('legacy','c','p','2026-01-01',true,'unsold');
    UPDATE auction_observations SET observed_at='2026-09-08' WHERE auction_id='legacy';`);
  const legacy = (await db.query("SELECT * FROM auction_observation_history WHERE auction_id='legacy' ORDER BY observed_at")).rows;
  assert.equal(legacy.length,2);assert.equal(legacy[0].legacy_timing,true);assert.equal(legacy[1].legacy_timing,false);
  assert.equal(new Date(legacy[1].first_observed_at).toISOString(),'2026-09-08T00:00:00.000Z');
  console.log('PASS fresh observations retain uncertain legacy provenance without backdating knowledge');
  await db.exec('SET ROLE authenticated');
  await assert.rejects(db.query('SELECT * FROM league_preferences'));
  await assert.rejects(db.query('SELECT * FROM auction_observation_history'));
  await db.exec('RESET ROLE');
  console.log('PASS browser roles cannot read private preferences or revision history');
  await db.close();
})().catch(error => { console.error(error.message); process.exitCode=1; });
