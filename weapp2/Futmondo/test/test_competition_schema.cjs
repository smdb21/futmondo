// Synthetic, isolated database validation. No credentials or remote calls.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const { PGlite } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-history-schema-tools/node_modules', '@electric-sql/pglite'));
const schema = fs.readFileSync('scripts/schema.sql','utf8');
const migration = fs.readFileSync('scripts/migrations/20261110225848_general_competition_context.sql','utf8');
const roles = 'CREATE ROLE anon; CREATE ROLE authenticated; CREATE ROLE service_role BYPASSRLS;';
let checks = 0;
const pass = name => { checks++; console.log('PASS '+name); };
async function scalar(db, sql) { return Object.values((await db.query(sql)).rows[0])[0]; }
// Reconstruct only the original, documented table contracts needed by this
// migration. This deliberately excludes every new competition column/table.
function legacySchema() {
  const tables = ['championships','real_clubs','players','user_teams','user_team_history','player_history','market_transactions','round_dream_team','player_daily_snapshots','player_match_observations','league_rule_snapshots','player_round_history_checks','rival_finance_evidence','complete_history_baselines','team_lineup_snapshots'];
  let sql = '';
  for (const name of tables) {
    const escaped = name.replace(/[.*+?^${}()|[\]\\]/g,'\\$&');
    const re = new RegExp('CREATE TABLE IF NOT EXISTS (?:public\\.)?'+escaped+' \\([\\s\\S]*?\\n\\);');
    let block = schema.match(re)?.[0]; assert.ok(block, 'Missing original table '+name);
    block = block.replace(/^\s*competition_id (?:text|TEXT)[^\n]*\n/gm,'')
      .replace(/^\s*season TEXT NOT NULL DEFAULT 'unknown',\n/gm,'')
      .replace(/^\s*scoring_version TEXT NOT NULL DEFAULT 'unknown',\n/gm,'')
      .replace(/^\s*source_round_key TEXT[^\n]*\n/gm,'');
    block = block.replace(/competition_id,\s*/g,'').replace(/source_round_key,\s*/g,'');
    if(name==='league_rule_snapshots') block=block.replace('PRIMARY KEY(championship_id,season,scoring_version,observed_at)','PRIMARY KEY(championship_id,scoring_version,observed_at)');
    if(name==='round_dream_team') block=block.replace(/CONSTRAINT round_dream_team_context_round_key UNIQUE\([^\n]+\)/,'UNIQUE(championship_id, round_number, player_id)');
    if(name==='player_daily_snapshots') block=block.replace(/CONSTRAINT player_daily_snapshots_context_date_key UNIQUE\([^\n]+\)/,'UNIQUE(player_id, championship_id, snapshot_date)');
    sql += block+'\n';
  }
  sql += 'ALTER TABLE player_match_observations ADD COLUMN occurred_at timestamptz; ALTER TABLE player_match_observations ADD COLUMN round_start_at timestamptz;';
  sql += `CREATE UNIQUE INDEX player_history_source_time ON player_history(player_id,championship_id,observed_at);
CREATE UNIQUE INDEX user_team_history_source_time ON user_team_history(user_team_id,championship_id,observed_at);
CREATE VIEW player_round_points_latest WITH(security_invoker=true) AS SELECT DISTINCT ON(championship_id,player_id,season,scoring_version,round) championship_id,player_id,season,scoring_version,round,points,score_status,observed_at,occurred_at,round_start_at FROM player_match_observations ORDER BY championship_id,player_id,season,scoring_version,round,observed_at DESC;
CREATE VIEW rival_finance_evidence_latest WITH(security_invoker=true) AS SELECT DISTINCT ON(account_id,championship_id,season,evidence_type,source_id) account_id,championship_id,season,evidence_type,source_id,observed_at,payload FROM rival_finance_evidence ORDER BY account_id,championship_id,season,evidence_type,source_id,observed_at DESC;`;
  sql += 'GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO service_role; GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO service_role;';
  return sql;
}
(async()=>{
  const fresh = new PGlite();
  try {
    await fresh.exec(roles); await fresh.exec(schema);
    await fresh.exec("UPDATE application_access SET admin_only=FALSE; INSERT INTO user_page_preferences(user_id,page,visited_at) VALUES('fixture','lineup','2026-11-01');");
    await fresh.exec(schema);
    assert.equal(await scalar(fresh,'SELECT admin_only FROM application_access'),false);
    assert.equal(await scalar(fresh,"SELECT page FROM user_page_preferences WHERE user_id='fixture'"),'lineup');
    pass('fresh initialization repeats without resetting admission or navigation preferences');
    await fresh.exec("SET ROLE service_role; INSERT INTO championships(id,name) VALUES('game','Fixture'),('other','Other'); INSERT INTO players(id,name,slug) VALUES('same','Same fixture','same'),('new-id','New source fixture','new'); INSERT INTO user_teams(id,championship_id,name) VALUES('team','game','Team'),('foreign-team','other','Foreign team');");
    await fresh.exec(`INSERT INTO player_match_observations(player_id,championship_id,competition_id,season,scoring_version,round,source_round_key,round_id,match_id,points,score_status,observed_at) VALUES
('same','game','domestic','2026-2027','rules',1,'id:r1','r1','m1',0,'final','2026-10-01'),
('same','game','europe','2026-2027','rules',1,'id:r1','r1','m2',-2,'final','2026-10-01'),
('same','game','domestic','2025-2026','rules',1,'id:r1','r1','m-old',18,'final','2026-10-01'),
('same','game','domestic','2026-2027','new-rules',1,'id:r1','r1','m1',9,'final','2026-10-01'),
('same','game','domestic','2026-2027','rules',1,'id:knockout','knockout','m3',4,'final','2026-10-01'),
('same','game','domestic','2026-2027','rules',1,'id:r1','r1','m1',NULL,'unavailable','2026-10-02');`);
    const rows=(await fresh.query("SELECT source_round_key,points::int,round_id,match_id FROM player_round_dataset WHERE row_kind='point' AND championship_id='game' AND competition_id='domestic' AND season='2026-2027' AND scoring_version='rules' ORDER BY source_round_key")).rows;
    assert.deepEqual(rows,[{source_round_key:'id:knockout',points:4,round_id:'knockout',match_id:'m3'},{source_round_key:'id:r1',points:null,round_id:'r1',match_id:'m1'}]);
    assert.equal(await scalar(fresh,'SELECT count(*)::int FROM player_match_observations'),6);
    pass('round IDs, overlapping labels, revisions, unknown scores and competition/season/scoring isolation survive latest views');
    const membership=`INSERT INTO player_membership_observations(player_id,competition_id,season,real_club_id,observed_at,source) VALUES
('same','domestic','2026-2027','club-a','2026-09-01','catalog'),
('same','domestic','2026-2027','club-b','2026-10-01','catalog'),
('same','domestic','2026-2027','club-a','2026-11-01','catalog'),
('same','europe','2026-2027','club-a','2026-09-01','catalog'),
('new-id','domestic','2026-2027','club-b','2026-10-01','catalog'),
('same','domestic','2026-2027',NULL,'2026-11-02','catalog') ON CONFLICT(player_id,competition_id,season,observed_at,real_club_id) DO NOTHING;`;
    await fresh.exec(membership); await fresh.exec(membership);
    assert.equal(await scalar(fresh,'SELECT count(*)::int FROM player_membership_observations'),6);
    assert.equal(await scalar(fresh,'SELECT count(*)::int FROM player_membership_observations WHERE effective_at IS NOT NULL'),0);
    await assert.rejects(fresh.exec("UPDATE player_membership_observations SET real_club_id='changed'"));
    pass('same-ID/new-ID transfers, return transfers, concurrent competitions and NULL-safe membership replay preserve original records');
    const context=`INSERT INTO league_context_observations(account_id,championship_id,competition_id,season,scoring_version,observed_at,expires_at,complete,payload) VALUES('account','game','domestic','2026-2027','rules','2026-11-01','2026-11-01T00:05:00Z',TRUE,'{"competition_name":"Fixture","scoring":{"point_system":"fixture"}}') ON CONFLICT(account_id,championship_id,competition_id,season,scoring_version,observed_at) DO NOTHING;`;
    await fresh.exec(context); await fresh.exec(context);
    assert.equal(await scalar(fresh,'SELECT count(*)::int FROM league_context_observations'),1);
    await assert.rejects(fresh.exec("UPDATE league_context_observations SET complete=FALSE"));
    pass('context observations are durable, replay-idempotent and immutable to the server role');
    await assert.rejects(fresh.exec("INSERT INTO market_transactions(championship_id,buyer_team_id,price) VALUES('game','foreign-team',1)"));
    await assert.rejects(fresh.exec("INSERT INTO player_daily_snapshots(player_id,championship_id,owner_team_id,value) VALUES('same','game','foreign-team',1)"));
    await assert.rejects(fresh.exec("UPDATE user_teams SET championship_id='other' WHERE id='team'"));
    await fresh.exec("INSERT INTO market_transactions(championship_id,buyer_team_id,seller_team_id,price) VALUES('game','','team',1),('game','team',NULL,2)");
    assert.equal(await scalar(fresh,"SELECT count(*)::int FROM market_transactions WHERE buyer_team_id IS NULL OR seller_team_id IS NULL"),2);
    pass('team-linked writes reject foreign leagues and retain empty/null Market counterparties');
    for(const role of ['anon','authenticated']) {
      await fresh.exec('RESET ROLE; SET ROLE '+role);
      for(const table of ['league_context_observations','player_membership_observations','player_round_dataset','rival_finance_evidence_latest']) await assert.rejects(fresh.query('SELECT * FROM '+table));
    }
    await fresh.exec('RESET ROLE');
    pass('browser roles cannot read private context, membership, score or finance objects');
    const indexes=(await fresh.query("SELECT indexdef FROM pg_indexes WHERE indexname IN ('player_round_league_context_revision','player_round_checks_league_context')")).rows;
    assert.equal(indexes.length,2); assert.ok(indexes.every(x=>x.indexdef.includes('(championship_id, competition_id, season, scoring_version, player_id')));
    pass('bulk history indexes lead with the complete league context before player identity');
    await fresh.exec(`INSERT INTO player_match_observations(player_id,championship_id,competition_id,season,scoring_version,round,source_round_key,points,score_status,observed_at)
SELECT 'load-'||g,'game',CASE WHEN g%2=0 THEN 'domestic' ELSE 'europe' END,CASE WHEN g%3=0 THEN '2025-2026' ELSE '2026-2027' END,'rules',g%38+1,'number:'||(g%38+1),g%19,'final','2026-10-01'::timestamptz FROM generate_series(1,3000) AS g;
ANALYZE player_match_observations;`);
    const plan=(await fresh.query("EXPLAIN (ANALYZE, FORMAT JSON) SELECT player_id,round,points FROM player_round_points_latest WHERE championship_id='game' AND competition_id='domestic' AND season='2026-2027' AND scoring_version='rules' ORDER BY player_id,round LIMIT 1000")).rows[0]['QUERY PLAN'][0];
    const used=[]; const visit=node=>{if(node['Index Name'])used.push(node['Index Name']);for(const child of node.Plans||[])visit(child);};visit(plan.Plan);
    assert.ok(used.includes('player_round_league_context_revision'));
    console.log(`MEASURE scoped3000 synthetic rows: ${plan['Execution Time'].toFixed(3)} ms; indexed context query.`);
    pass('representative multi-context score query uses the scoped index before its row limit');
  } finally { await fresh.close(); }
  const upgrade=new PGlite();
  try {
    await upgrade.exec(roles); await upgrade.exec(legacySchema());
    await upgrade.exec("INSERT INTO championships(id,name) VALUES('game','Fixture'); INSERT INTO players(id,name,slug) VALUES('same','Fixture','same'); INSERT INTO user_teams(id,championship_id,name) VALUES('team','game','Fixture'); INSERT INTO player_match_observations(player_id,championship_id,season,scoring_version,round,points,score_status,observed_at) VALUES('same','game','unknown','legacy',1,-2,'unknown','2025-10-01'); INSERT INTO user_team_history(user_team_id,championship_id,points,budget,observed_at) VALUES('team','game',0,NULL,NULL); INSERT INTO player_history(player_id,championship_id,value,observed_at) VALUES('same','game',100,NULL);");
    const before=(await upgrade.query('SELECT to_jsonb(p) AS row FROM player_match_observations p')).rows;
    const originalHistory=(await upgrade.query('SELECT id,user_team_id,championship_id,points,budget,observed_at FROM user_team_history')).rows;
    await upgrade.exec(migration.replace(/COMMIT;\s*$/,'ROLLBACK;'));
    assert.equal(await scalar(upgrade,"SELECT to_regclass('public.league_context_observations') IS NULL"),true);
    assert.equal(await scalar(upgrade,"SELECT count(*)::int FROM information_schema.columns WHERE table_name='player_match_observations' AND column_name='competition_id'"),0);
    assert.deepEqual((await upgrade.query('SELECT to_jsonb(p) AS row FROM player_match_observations p')).rows,before);
    pass('pre-commit rollback restores original schema and exact historical rows');
    await upgrade.exec(migration); await upgrade.exec(migration);
    assert.deepEqual((await upgrade.query("SELECT to_jsonb(p) - ARRAY['competition_id','source_round_key','round_id','match_id','source_round_number'] AS row FROM player_match_observations p")).rows,before);
    assert.deepEqual((await upgrade.query('SELECT id,user_team_id,championship_id,points,budget,observed_at FROM user_team_history')).rows,originalHistory);
    assert.equal(await scalar(upgrade,"SELECT competition_id FROM player_match_observations"),'unknown');
    assert.equal(await scalar(upgrade,"SELECT season FROM player_match_observations"),'unknown');
    pass('upgrade repeats without inventing seasons, relabeling legacy facts or changing unknown source times');
    await upgrade.exec("SET ROLE service_role; INSERT INTO player_round_history_checks(championship_id,competition_id,player_id,season,scoring_version,checked_at,result_status,point_count) VALUES('game','domestic','same','2026-2027','rules','2026-11-01','empty',0),('game','europe','same','2026-2027','rules','2026-11-01','scores',1);");
    assert.equal(await scalar(upgrade,"SELECT count(*)::int FROM player_round_dataset WHERE row_kind='check'"),2);
    pass('upgraded acquisition markers remain distinct across real competitions');
  } finally { await upgrade.close(); }
  console.log(`COMPETITION SCHEMA: ${checks} groups passed / 0 failed`);
})().catch(e=>{console.error(e.stack);process.exitCode=1;});
