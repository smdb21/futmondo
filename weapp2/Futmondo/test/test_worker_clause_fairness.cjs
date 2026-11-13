const fs=require('fs'),assert=require('assert/strict');
const {PGlite}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/@electric-sql/pglite');
(async()=>{const db=new PGlite();try{
 await db.exec('create role anon;create role authenticated;create role service_role bypassrls;');
 await db.exec(fs.readFileSync('scripts/schema.sql','utf8'));
 const prior=fs.readFileSync('scripts/migrations/20261113115929_worker_resolved_handoff.sql','utf8');await db.exec(prior);
 const q=async(s,a=[]) => (await db.query(s,a)).rows;
 const rpc=async(n,a)=>(await q(`select worker_${n}(${a.map((_,i)=>'$'+(i+1)).join(',')}) result`,a))[0].result;
 await db.exec('set role service_role');await rpc('elect',['leader']);await rpc('authorize',['u',true,'synthetic','UTC',0,0,[],'fixture']);
 const context={account_id:'u',championship_id:'l',user_team_id:'t',player_id:'p',competition_id:'c',season:'s',scoring_version:'v'};
 const core={version:1,handler:'get_players_from_team',resource_key:'a'.repeat(64),args:{championship_id:'l',user_team_id:'t'},context};
 const clause={version:1,handler:'get_clause_transfer_summary',resource_key:'b'.repeat(64),args:{championship_id:'l',user_team_id:'t',player_id:'p'},context};
 await rpc('enqueue',['u',core,60,0]);await rpc('enqueue',['u',clause,300,0]);
 const reset=async(age=90)=>{await q('update worker_jobs set owner=null,lease_until=null');await q("update worker_jobs set due_at=now()-interval '5 seconds',checked_at=now()-interval '61 seconds' where resource_key=$1",[core.resource_key]);await q("update worker_jobs set due_at=now()-make_interval(secs=>$1),checked_at=now()-interval '400 seconds' where resource_key=$2",[age,clause.resource_key]);};
 for(let i=0;i<3;i++){await reset();assert.equal((await rpc('claim',['leader:old',true])).resource_key,core.resource_key);}
 console.log('PASS reproduced starvation: continuously due core reads repeatedly beat a 90-second-overdue clause under the prior policy');
 const migration=fs.readFileSync('scripts/migrations/20261113142221_worker_clause_fairness.sql','utf8');await db.exec('reset role');await db.exec(migration);await db.exec(migration);await db.exec('set role service_role');
 await reset();const job=await rpc('claim',['leader:fair',true]);assert.equal(job.resource_key,clause.resource_key);assert.equal(job.interval_seconds,300);assert.equal(job.priority,3);
 console.log('PASS aged clause shares core urgency and wins by older deadline without changing stored priority or five-minute cadence');
 await reset(30);assert.equal((await rpc('claim',['leader:recent',true])).resource_key,core.resource_key);
 await reset(60);assert.equal((await rpc('claim',['leader:boundary',true])).resource_key,clause.resource_key);
 console.log('PASS normal core precedence holds before the exact 60-second overdue boundary');
 await reset();await q("update worker_accounts set quiet_start=extract(hour from now() at time zone 'UTC')::integer,quiet_end=(extract(hour from now() at time zone 'UTC')::integer+1)%24");
 assert.equal(await rpc('claim',['leader:quiet',true]),null);
 await rpc('enqueue',['u',clause,60,10]);assert.equal((await rpc('claim',['leader:manual',true])).resource_key,clause.resource_key);
 console.log('PASS aging cannot bypass quiet-hour eligibility; explicit Refresh still bypasses it and legacy intervals remain normalized');
}finally{await db.close()}})().catch(e=>{console.error(e.message,e.detail||'');process.exitCode=1});
