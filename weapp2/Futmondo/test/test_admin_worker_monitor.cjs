// Isolated Postgres fixtures only: no live worker or private account records.
const fs=require('node:fs'),assert=require('node:assert/strict');
const {PGlite}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/@electric-sql/pglite');
(async()=>{const db=new PGlite();let checks=0;const pass=name=>{checks++;console.log('PASS',name)};try{
 await db.exec('create role anon;create role authenticated;create role service_role bypassrls;');
 await db.exec(fs.readFileSync('scripts/schema.sql','utf8'));
 const q=async(sql,args=[]) => (await db.query(sql,args)).rows;
 const leader='11111111-1111-4111-8111-111111111111',standby='22222222-2222-4222-8222-222222222222',old='33333333-3333-4333-8333-333333333333';
 for(const account of ['own-fixture','other-fixture'])await q("insert into worker_accounts(account_id,enabled,encrypted_token,credential_id,verified_leagues) values($1,true,'SECRET-CIPHERTEXT','fixture',$2)",[account,{league:{competition_id:'competition',season:'2026',scoring_version:'score'}}]);
 await q("insert into worker_health(id,status,heartbeat,requests,jobs) values($1,'running',now()-interval '20 seconds',100,80),($2,'standby',now()-interval '30 seconds',0,0),($3,'running',now()-interval '4 minutes',5,3)",[leader,standby,old]);
 await q("insert into worker_leader(id,owner,lease_until) values(1,$1,now()+interval '180 seconds')",[leader]);
 let next=0;const add=async(account,owner,lease="now()+interval '60 seconds'")=>{
  const key=(++next).toString(16).padStart(64,'0');
  const d={handler:'get_players_from_team',args:{private_note:'DO-NOT-EXPOSE'},context:{account_id:account,championship_id:'league',season:'2026',competition_id:'competition',scoring_version:'score'}};
  const rows=await q(`insert into worker_jobs(account_id,resource_key,descriptor,generation,owner,started_at,lease_until,last_error,checkpoint) values($1,$2,$3,1,$4,now()-interval '5 seconds',${lease},'PRIVATE-ERROR', '{"private":"PRIVATE-CHECKPOINT"}') returning resource_key,run_id`,[account,key,d,owner]);return {...rows[0],account,owner};
 };
 const own=await add('own-fixture',leader+':0'),other=await add('other-fixture',leader+':1');
 await add('own-fixture',leader+':9',"now()-interval '1 second'");
 await add('other-fixture',standby+':0');
 await add('other-fixture',old+':0');
 const activity=(job,sequence=1,route='/1/userteam/roster',phase='source_request')=>({phase,route,method:route?'POST':null,started_at:new Date().toISOString(),sequence,run_id:job.run_id});
 const guard=async(job,body,owner=job.owner,generation=1)=> (await q('select worker_guard_activity($1,$2,$3,$4,NULL,$5) as ok',[job.account,job.resource_key,owner,generation,body]))[0].ok;
 const monitor=async(account='own-fixture')=>(await q('select worker_admin_monitor($1) as data',[account]))[0].data;
 await db.exec('set role service_role');
 assert.equal(await guard(own,activity(own)),true);assert.equal(await guard(other,activity(other,1,'/1/player/summary')),true);
 let m=await monitor();
 if(process.env.FUTMONDO_MONITOR_FIXTURE_PATH)fs.writeFileSync(process.env.FUTMONDO_MONITOR_FIXTURE_PATH,JSON.stringify(m));
 assert.deepEqual(m.summary,{live_workers:2,working_workers:1,active_jobs:2,retained_workers:1,expired_jobs:1});
 assert.equal(m.current.active_jobs,1);assert.equal(m.others.active_jobs,1);assert.equal(m.workers.length,2);
 assert.equal(m.current.jobs[0].activity.route,'/1/userteam/roster');assert.equal(m.others.jobs[0].activity.route,'/1/player/summary');
 assert.equal(m.current.jobs[0].activity.fresh,true);assert.equal(m.workers.find(x=>x.worker_id===standby).status,'standby');
 assert.deepEqual(Object.keys(m.current.jobs[0]).sort(),['account_ref','activity','handler','job_id','lane','lease_until','started_at','worker_id']);
 assert.ok(!/SECRET|PRIVATE|DO-NOT-EXPOSE|own-fixture|other-fixture|encrypted_token|checkpoint|descriptor/.test(JSON.stringify(m)));
 pass('fresh shared workers, exact account groups, expired leases and safe field projection');

 assert.equal(await guard(own,activity(own,5,'/1/market/players')),true);
 const newer=(await q('select activity_at from worker_jobs where resource_key=$1',[own.resource_key]))[0].activity_at;
 assert.equal(await guard(own,activity(own,4,'/1/player/summary')),true);
 m=await monitor();assert.equal(m.current.jobs[0].activity.route,'/1/market/players');
 assert.equal((await q('select activity_at from worker_jobs where resource_key=$1',[own.resource_key]))[0].activity_at.getTime(),newer.getTime());
 assert.equal(await guard(own,activity(own,6),leader+':1'),false);assert.equal(await guard(own,activity(own,6),own.owner,2),false);
 assert.equal(await guard(own,{...activity(own,6),run_id:'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa'}),false);
 await q("update worker_jobs set activity_at=now()-interval '31 seconds' where resource_key=$1",[own.resource_key]);
 assert.equal(await guard(own,activity(own,5,'/1/market/players')),true);
 assert.equal((await monitor()).current.jobs[0].activity.fresh,true);
 pass('late sequence cannot overwrite activity; equal sequence refreshes freshness; wrong owner/generation/run fail');

 for(const bad of [
  {...activity(own),password:'secret'}, {...activity(own),phase:'secret'},
  {...activity(own),route:'/1/userteam/roster?token=private'}, {...activity(own),route:'https://api.futmondo.com/1/userteam/roster'},
  {...activity(own),method:'CONNECT'}, {...activity(own),sequence:-1}, {...activity(own),sequence:1.5},
  {...activity(own),run_id:'private'}, {...activity(own),started_at:'private'},
  {...activity(own),route:null}, {...activity(own),method:null},
  {...activity(own),started_at:'2026-02-31T20:00:00Z'}, {...activity(own),phase:'x'.repeat(2000)}
 ])await assert.rejects(guard(own,bad));
 assert.equal(await guard(own,activity(own,6,null,'processing')),true);
 m=await monitor();assert.equal(m.current.jobs[0].activity.phase,'processing');assert.equal(m.current.jobs[0].activity.route,null);
 pass('payload schema rejects unknown keys, unsafe routes, methods, oversize values and malformed identities');
 await q("update worker_jobs set owner=$1,descriptor=jsonb_set(descriptor,'{handler}',to_jsonb('private/handler99'::text)) where resource_key=$2",[leader+':bad-lane',own.resource_key]);
 m=await monitor();assert.equal(m.current.jobs[0].lane,'?');assert.equal(m.current.jobs[0].handler,'unknown');assert.equal(m.current.jobs[0].activity,null);
 if(process.env.FUTMONDO_MONITOR_FIXTURE_PATH)fs.writeFileSync(process.env.FUTMONDO_MONITOR_FIXTURE_PATH+'.sentinels',JSON.stringify(m));
 await q("update worker_jobs set owner=$1,descriptor=jsonb_set(descriptor,'{handler}',to_jsonb('get_players_from_team'::text)) where resource_key=$2",[own.owner,own.resource_key]);
 pass('unsafe handler and lane metadata become explicit safe sentinels');

 await q("update worker_jobs set activity_at=now()-interval '31 seconds' where resource_key=$1",[own.resource_key]);
 m=await monitor();assert.equal(m.current.jobs[0].activity.fresh,false);
 await q("update worker_jobs set activity=jsonb_set(activity,'{owner}',to_jsonb('other-owner'::text)) where resource_key=$1",[own.resource_key]);
 assert.equal((await monitor()).current.jobs[0].activity,null);
 assert.equal(await guard(own,activity(own,1)),true); // Different owner metadata permits a fresh sequence.
 await q('update worker_jobs set run_id=gen_random_uuid() where resource_key=$1',[own.resource_key]);
 assert.equal((await monitor()).current.jobs[0].activity,null);
 const newRun=(await q('select run_id from worker_jobs where resource_key=$1',[own.resource_key]))[0].run_id;
 assert.equal(await guard(own,activity(own,999)),false);
 own.run_id=newRun;assert.equal(await guard(own,activity(own,0)),true);
 pass('stale samples and old-owner/run metadata never masquerade as current activity');

 await q("update worker_accounts set enabled=false where account_id='own-fixture'");
 assert.equal(await guard(own,activity(own,1)),false);
 await q("update worker_accounts set enabled=true,excluded_leagues='[\"league\"]' where account_id='own-fixture'");
 assert.equal(await guard(own,activity(own,1)),false);
 await q("update worker_accounts set excluded_leagues='[]' where account_id='own-fixture'");
 await q("update worker_jobs set descriptor=jsonb_set(descriptor,'{handler}',to_jsonb('fm_worker_finances'::text))||'{\"context\":{\"championship_id\":\"league\",\"competition_id\":\"wrong\",\"season\":\"2026\",\"scoring_version\":\"score\"}}'::jsonb where resource_key=$1",[own.resource_key]);
 assert.equal(await guard(own,activity(own,1)),false);
 await q("update worker_jobs set descriptor=jsonb_set(descriptor,'{handler}',to_jsonb('get_players_from_team'::text)) where resource_key=$1",[own.resource_key]);
 await q("insert into blocked_leagues(championship_id,sport) values('league','basket')");
 assert.equal(await guard(own,activity(own,1)),false);
 await db.exec('reset role');await q("delete from blocked_leagues where championship_id='league'");await db.exec('set role service_role');
 await q("update worker_leader set lease_until=now()-interval '1 second'");
 assert.equal(await guard(own,activity(own,1)),false);
 m=await monitor();assert.equal(m.summary.active_jobs,0);assert.equal(m.summary.working_workers,0);assert.deepEqual(m.current.jobs,[]);assert.deepEqual(m.others.jobs,[]);
 assert.equal(m.current.active_jobs,0);assert.equal(m.others.active_jobs,0);
 await q("update worker_leader set lease_until=now()+interval '180 seconds'");
 pass('account revocation, blocked league and expired leadership preserve acquisition guards');

 for(const role of ['anon','authenticated']){
  await db.exec('set role '+role);
  await assert.rejects(monitor());await assert.rejects(guard(own,activity(own)));
  await assert.rejects(q('select activity from worker_jobs limit 1'));
 }
 await db.exec('set role service_role');
 const permissions=await q("select p.proname,p.prosecdef from pg_proc p join pg_namespace n on n.oid=p.pronamespace where n.nspname='public' and p.proname in ('worker_activity_valid','worker_guard_activity','worker_admin_monitor')");
 assert.equal(permissions.length,3);assert.ok(permissions.every(x=>x.prosecdef===false));
 pass('browser roles cannot read telemetry or invoke monitor; functions remain security invoker');

 await q("update worker_health set heartbeat=now()-interval '4 minutes'");
 m=await monitor();assert.equal(m.summary.live_workers,0);assert.equal(m.summary.active_jobs,0);assert.equal(m.summary.retained_workers,3);assert.deepEqual(m.workers,[]);
 assert.equal(await monitor(''),null);
 pass('old heartbeat history is not counted as live workers and empty monitor identity fails closed');

 await q("update worker_health set heartbeat=now() where id=$1",[leader]);
 for(let i=0;i<130;i++)await add(i<3?'own-fixture':'other-fixture',leader+':'+(i%2));
 m=await monitor();assert.equal(m.limits.jobs_truncated,true);assert.equal(m.current.jobs.length+m.others.jobs.length,128);
 assert.equal(m.summary.active_jobs,132);assert.equal(m.current.active_jobs,4);assert.equal(m.current.jobs.length,4);
 for(let i=0;i<66;i++)await q("insert into worker_health(id,status,heartbeat) values($1,'standby',now())",['standby-'+i]);
 m=await monitor();assert.equal(m.workers.length,64);assert.equal(m.limits.workers_truncated,true);assert.equal(m.summary.live_workers,67);
 assert.ok(m.workers.every(x=>/^[a-fA-F0-9-]{8,64}$/.test(x.worker_id)));
 assert.ok(m.workers.some(x=>/^[a-f0-9]{16}$/.test(x.worker_id)));
 pass('bounded rows retain exact counts and prioritize the current account');
 await q("insert into worker_jobs(account_id,resource_key,descriptor,generation) select 'other-fixture',md5('idle-'||i)||md5('idle-2-'||i),'{\"handler\":\"get_players_from_team\"}',1 from generate_series(1,10000) i");
 await q('analyze worker_jobs');
 const plan=await q('explain (analyze,format json) select worker_admin_monitor($1)', ['own-fixture']);
 const timing=plan[0]['QUERY PLAN'][0];
 m=await monitor();assert.equal(m.summary.active_jobs,132);
 console.log('SQL PERF',JSON.stringify({idle_jobs:10000,active_jobs:132,live_workers:67,execution_ms:timing['Execution Time'],response_bytes:Buffer.byteLength(JSON.stringify(m))}));
 console.log(`ADMIN WORKER MONITOR SQL: ${checks} passed / 0 failed`);
}finally{await db.close();}})().catch(error=>{console.error(error);process.exit(1)});
