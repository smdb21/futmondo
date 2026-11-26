const assert = require('node:assert/strict');
const fs = require('node:fs');
const {createHash} = require('node:crypto');
const {PGlite} = require((process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules') + '/@electric-sql/pglite');

(async () => {
  const db = new PGlite();
  const q = async (sql, args = []) => (await db.query(sql, args)).rows;
  try {
    await db.exec('create role anon;create role authenticated;create role service_role bypassrls;');
    await db.exec(fs.readFileSync('scripts/schema.sql', 'utf8'));
    await db.exec(fs.readFileSync('scripts/migrations/20261126135250_split_round_job_cadence.sql', 'utf8'));
    await db.exec('set role service_role');
    await q("select worker_elect('leader')");
    const [a] = await q("select worker_authorize('fixture',true,'synthetic','UTC',0,0,'[]','scope') as account");
    const context = {account_id:'fixture',championship_id:'fantasy',competition_id:'real-league',season:'2026'};
    const source = 'calendar:v1:' + createHash('sha256').update(context.competition_id).digest('hex');
    const now = Date.now()/1000;
    const split = {slow_from:now-3600, resume_at:now+86400*60};
    const save = async (time, gaps, scope=context) => q('select finance_history_write($1)', [[{
      ...scope, evidence_type:'eligibility',source_id:source,observed_at:new Date(time*1000).toISOString(),
      payload:{kind:'league_round_calendar',completeness:'complete',scope:{...scope,league_id:scope.competition_id},split_gaps:gaps}
    }]]);
    const tasks = [
      ['get_classification_round_ranking',{round_id:'r23',round_number:23}],
      ['get_round_dreamteam',{reference:'r23'}],
      ['get_round_lineup_snapshot',{round_id:'r23',user_team_id:'mine'}],
      ['get_round_lineup_snapshot',{round_id:'r15',user_team_id:'mine'}],
      ['get_player_summary',{player_id:'p'}]
    ].map(([handler,args],i)=>({handler,args,context,resource_key:String(i+1).repeat(64),interval_seconds:300}));
    const batch = async intervals => (await q('select worker_enqueue_batch($1,$2,$3,$4) n',
      ['fixture','leader:test',a.account.generation,tasks.map((d,i)=>({...d,interval_seconds:intervals[i]}))]))[0].n;
    const get = async () => q("select native_interval_seconds interval,ctid::text id,due_at,owner,last_error from worker_jobs where descriptor->'context'->>'championship_id'='fantasy' order by resource_key");
    const fast = tasks.map(()=>300), daily = tasks.map(()=>86400);
    assert.equal(await batch(fast),5);
    await q("update worker_jobs set checked_at=now()-interval '1 hour'");
    await q('update worker_jobs set owner=$1,lease_until=now()+interval \'60 seconds\' where resource_key=$2',['leader:other',tasks[1].resource_key]);
    await q('update worker_jobs set last_error=$1 where resource_key=$2',['source_transient',tasks[2].resource_key]);
    await save(now-120,{r23:split},{...context,season:'old'});
    await batch(daily);
    assert.deepEqual((await get()).map(j=>j.interval),fast);
    await save(now-100,{r23:split});
    const before = await get();
    await batch(daily);
    const paused = await get();
    assert.deepEqual(paused.map(j=>j.interval),[86400,86400,86400,300,300]);
    assert.ok(paused[0].due_at>before[0].due_at);
    for (const i of [1,2]) assert.deepEqual(paused[i].due_at,before[i].due_at);
    assert.equal(paused[1].owner,'leader:other');assert.equal(paused[2].last_error,'source_transient');
    await batch(daily);await batch(fast);
    assert.deepEqual(await get(),paused);
    await save(now-30,{});
    await save(now-50,{r23:split});
    await batch(fast);
    assert.deepEqual((await get()).map(j=>j.interval),fast);
    await batch(daily);
    assert.deepEqual((await get()).map(j=>j.interval),fast);
    for(const role of ['anon','authenticated']){
      const [r]=await q("select has_function_privilege($1,'worker_enqueue_batch(text,text,bigint,jsonb)','execute') as allowed",[role]);
      assert.equal(r.allowed,false);
    }
    console.log('PASS split-only cadence, current scoped source revision, unchanged rediscovery, stale-order protection, reopening, owner/retry and grants');
  } finally {
    await db.close();
  }
})().catch(e=>{console.error(e);process.exit(1)});
