const fs=require('node:fs'),assert=require('node:assert/strict');
const {PGlite}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/@electric-sql/pglite');
(async()=>{const db=new PGlite();try{
 await db.exec('create role anon;create role authenticated;create role service_role bypassrls;');
 await db.exec(fs.readFileSync('scripts/schema.sql','utf8'));
 const files=['20261114043816_worker_database_read_boundary.sql','20261114043932_worker_autonomous_cadence.sql'];
 for(let i=0;i<2;i++)for(const file of files)await db.exec(fs.readFileSync('scripts/migrations/'+file,'utf8'));
 const q=async(s,a=[]) => (await db.query(s,a)).rows;
 const rpc=async(n,a)=>(await q(`select worker_${n}(${a.map((_,i)=>'$'+(i+1)).join(',')}) result`,a))[0].result;
 await db.exec('set role service_role');await rpc('elect',['leader']);const account=await rpc('authorize',['u',true,'synthetic','UTC',0,0,[],'fixture']);
 const bootstrap=(await q('select resource_key,descriptor from worker_jobs'))[0];
 assert.equal(bootstrap.resource_key,'34e25315b6f448370899177aa835561ec8f58be3381da2eb634c087634a1a5bd');
 assert.equal(bootstrap.descriptor.handler,'get_active_championships');
 assert.equal((await q('select count(*)::int n from worker_jobs'))[0].n,1);
 console.log('PASS enrollment alone seeds the exact R/Python canonical discovery job, with no browser enqueue');
 const context={account_id:'u',championship_id:'l',user_team_id:'t',player_id:'p',competition_id:'c',season:'s',scoring_version:'v'};
 const descriptor=(handler,key,args={championship_id:'l',user_team_id:'t',player_id:'p'})=>({version:1,handler,resource_key:key.repeat(64),args,context});
 const stored=descriptor('get_player_recent_points','b');
 const cold={...stored,resource_key:'a'.repeat(64),context:{...context,competition_id:'unknown',season:'unknown',scoring_version:'unknown'}};
 await q('update worker_accounts set verified_leagues=$1 where account_id=$2',[{l:{competition_id:'c',season:'s',scoring_version:'v'}},'u']);
 const version='11111111-1111-4111-8111-111111111111';
 await q(`insert into worker_snapshots(account_id,resource_key,handler,version,context,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts,run_id,descriptor)
 values($1,$2,$3,$4,$5,now()-interval '10 minutes',now()-interval '10 minutes',now()-interval '5 minutes','complete','ok',1,$6,1,gen_random_uuid(),$7)`,['u',stored.resource_key,stored.handler,version,context,'c'.repeat(64),stored]);
 await q('insert into worker_chunks(account_id,resource_key,version,part,data) values($1,$2,$3,0,$4)',['u',stored.resource_key,version,'fixture']);
 const before=await q('select resource_key,descriptor,due_at,requested_at,priority from worker_jobs order by resource_key');
 await db.exec('begin read only');
 let result=await rpc('read',['u',{[cold.resource_key]:''},{[cold.resource_key]:cold}]);
 assert.deepEqual(result.redirects,[{requested_key:cold.resource_key,descriptor:stored}]);assert.equal(result.resources.length,1);
 assert.deepEqual(result.resources[0].manifest.context,context);assert.equal(result.jobs.length,0);
 assert.ok(new Date(result.resources[0].manifest.valid_until)<new Date());
 result=await rpc('read',['u',{[cold.resource_key]:'',[stored.resource_key]:version},{[cold.resource_key]:cold}]);assert.equal(result.resources.length,0);
 await db.exec('commit');assert.deepEqual(await q('select resource_key,descriptor,due_at,requested_at,priority from worker_jobs order by resource_key'),before);
 console.log('PASS cold unknown-context display resolves a persisted child with no requested or canonical job; read-only transaction succeeds and no schedule changes');
 assert.equal((await rpc('read',['u',{[cold.resource_key]:''},{[cold.resource_key]:{...cold,args:{...cold.args,player_id:'different'}}}])).resources.length,0);
 assert.equal((await rpc('read',['u',{[cold.resource_key]:''},{[cold.resource_key]:{...cold,context:{...cold.context,season:'old'}}}])).resources.length,0);
 await assert.rejects(rpc('read',['u',{[cold.resource_key]:''},{[cold.resource_key]:{...cold,context:{...cold.context,account_id:'other'}}}]),/Foreign read/);
 assert.equal((await rpc('read',['other',{[stored.resource_key]:''}])).resources.length,0);
 await assert.rejects(rpc('read',['u',Object.fromEntries(Array.from({length:129},(_,i)=>['k'+i,'']))]),/Invalid read/);
 console.log('PASS account, known-season, exact-argument and bounded-request checks reject foreign or oversized lookups');
 // Publication persists the exact child descriptor before retiring its old
 // independent query job; parent source timestamps remain untouched.
 const parent=descriptor('get_player_summary','d');
 await rpc('enqueue',['u',parent,3600,0]);await rpc('enqueue',['u',stored,300,0]);
 await q("update worker_jobs set owner='leader:publish',lease_until=now()+interval '60 seconds',started_at=now(),requested_at=now()-interval '1 second' where resource_key=$1",[parent.resource_key]);
 const childManifest={context,version,parts:1,json_bytes:1,sha256:'c'.repeat(64),observed_at:'2026-11-13T00:00:00Z',checked_at:'2026-11-13T00:00:00Z',valid_until:'2026-11-13T00:05:00Z',completeness:'complete',status:'ok'};
 // Use a newer source than the seeded fixture for an acknowledged replacement.
 childManifest.observed_at=new Date(Date.now()-1000).toISOString();childManifest.checked_at=childManifest.observed_at;childManifest.valid_until=new Date(Date.now()+299000).toISOString();
 assert.equal(await rpc('publish_child',['u',parent.resource_key,'leader:publish',account.generation,stored,childManifest]),true);
 assert.equal((await q('select count(*)::int n from worker_jobs where resource_key=$1',[stored.resource_key]))[0].n,0);
 assert.deepEqual((await q('select descriptor from worker_snapshots where resource_key=$1',[stored.resource_key]))[0].descriptor,stored);
 console.log('PASS successful summary publication stores the derived descriptor and retires its idle duplicate job only after durable acknowledgement');
 const base='2026-11-14T00:00:00Z';
 for(const [handler,seconds] of [['get_user_team_info',60],['get_players_from_team',60],['get_clause_transfer_summary',300],['get_player_summary',3600]]){
  const due=await rpc('next_refresh_due',[handler,seconds,'2026-11-13T00:00:00Z','2026-11-13T23:59:00Z',base]);
  assert.equal(new Date(due)-new Date(base),seconds*1000);
 }
 console.log('PASS expired source validity cannot accelerate a completed job; next run follows the full configured interval');
 // Saturate the queue with527 player jobs and continuously overdue core work.
 await q('delete from worker_jobs');
 const handlers=['get_players_from_team','get_market_players','get_championship_players','get_player_summary','get_player_matches','get_player_card_history','fm_worker_finances'];
 for(let i=0;i<handlers.length;i++)await rpc('enqueue',['u',descriptor(handlers[i],(i+1).toString(16)),handlers[i]=='get_player_summary'?3600:60,0]);
 await q(`insert into worker_jobs(account_id,resource_key,descriptor,generation,priority,base_priority,interval_seconds)
 select 'u',md5(i::text)||md5('fixture'||i::text),jsonb_build_object('version',1,'handler','get_player_summary',
 'args',jsonb_build_object('championship_id','l','user_team_id','t','player_id','bulk-'||i),'context',$2::jsonb),$1,1,1,3600 from generate_series(1,527) i`,[account.generation,context]);
 const seen=[];const started=performance.now();
 for(let i=0;i<handlers.length;i++){
  const job=await rpc('claim',['leader:bulk',false]);assert.ok(job);seen.push(job.descriptor.handler);
  await q("update worker_jobs set owner=null,lease_until=null,checked_at=now(),due_at=now()-interval '1 second' where resource_key=$1",[job.resource_key]);
 }
 assert.deepEqual([...new Set(seen)].sort(),[...handlers].sort());
 const elapsed=Math.round(performance.now()-started);
 const short=await rpc('claim',['leader:short',true]);assert.ok(['get_players_from_team','get_market_players'].includes(short.descriptor.handler));
 console.log(`PASS all seven query families progress amid527 waiting player jobs and recurring core work (${elapsed}ms for seven claims); short lane remains core-only`);
 await q("update worker_jobs set owner=null,lease_until=null,due_at=now()+interval '1 hour' where descriptor->>'handler' in ('get_players_from_team','get_market_players')");
 const fallback=await rpc('claim',['leader:fallback',true]);assert.ok(fallback);assert.notEqual(fallback.descriptor.handler,'fm_worker_finances');
 console.log('PASS the short lane helps the historical backlog when core is caught up instead of sitting idle');
 await db.exec("update worker_jobs set owner=null,lease_until=null,checked_at=now(),due_at=now()-interval '1 second';update worker_accounts set quiet_start=extract(hour from now() at time zone 'UTC')::integer,quiet_end=(extract(hour from now() at time zone 'UTC')::integer+1)%24");
 assert.equal(await rpc('claim',['leader:quiet',false]),null);
 console.log('PASS autonomous scheduling preserves the quiet-hour floor');
}finally{await db.close()}})().catch(e=>{console.error(e.stack,e.detail||'');process.exitCode=1});
