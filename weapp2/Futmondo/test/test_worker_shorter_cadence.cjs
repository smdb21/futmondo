const fs=require('node:fs'),assert=require('node:assert/strict');
const {PGlite}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/@electric-sql/pglite');
(async()=>{const db=new PGlite();try{
 await db.exec('create role anon;create role authenticated;create role service_role bypassrls;');
 await db.exec(fs.readFileSync('scripts/schema.sql','utf8'));
 const migration=fs.readFileSync('scripts/migrations/20261114144534_worker_shorter_cadence.sql','utf8');
 const previous=fs.readFileSync('scripts/migrations/20261113065753_worker_clause_cadence.sql','utf8');
 const start=previous.indexOf('CREATE OR REPLACE FUNCTION public.worker_enqueue(');
 await db.exec(previous.slice(start,previous.indexOf('END $$;',start)+7));
 const q=async(s,a=[]) => (await db.query(s,a)).rows;
 const rpc=async(n,a)=>(await q(`select worker_${n}(${a.map((_,i)=>'$'+(i+1)).join(',')}) result`,a))[0].result;
 await db.exec('set role service_role');await rpc('elect',['fixture']);
 const account=await rpc('authorize',['fixture',true,'synthetic','UTC',0,0,[],'fixture']);
 const context={account_id:'fixture',championship_id:'league',user_team_id:'mine',player_id:'p',competition_id:'competition',season:'season',scoring_version:'scoring'};
 const descriptor=n=>({version:1,handler:'get_player_summary',resource_key:n.toString(16).padStart(64,'0'),
  args:{championship_id:'league',user_team_id:'mine',player_id:'p'+n},context:{...context,player_id:'p'+n}});
 const d=descriptor(1);
 await rpc('enqueue',['fixture',d,3600,0]);
 await q("update worker_jobs set checked_at=now()-interval '10 minutes',due_at=now()+interval '50 minutes' where resource_key=$1",[d.resource_key]);
 const discover=d=>rpc('enqueue_batch',['fixture','fixture:lane',account.generation,[{...d,interval_seconds:300}]]);
 const job=d=>q('select to_jsonb(j) value,ctid::text row_version from worker_jobs j where resource_key=$1',[d.resource_key]).then(x=>x[0]);
 await discover(d);let old=(await job(d)).value;
 assert.equal(old.interval_seconds,300);assert.ok(new Date(old.due_at)-Date.now()>49*60000);
 console.log('PASS reproduces the old defect: hourly source promoted to300s still waits50minutes');

 await q(`insert into worker_snapshots(account_id,resource_key,handler,version,context,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts,run_id,descriptor)
 values('fixture',$1,'get_player_summary',gen_random_uuid(),$2,now()-interval '10 minutes',now()-interval '10 minutes',now()-interval '5 minutes','unknown','ok',1,repeat('a',64),1,gen_random_uuid(),$3)`,[d.resource_key,d.context,d]);
 await q("update worker_accounts set verified_leagues=$1 where account_id='fixture'",[{league:{competition_id:'competition',season:'season',scoring_version:'scoring'}}]);
 const market={...d,handler:'get_market_players',resource_key:'f'.repeat(64),context:{...context,player_id:''},args:{championship_id:'league',user_team_id:'mine'}};
 await rpc('enqueue',['fixture',market,60,0]);
 await q(`insert into worker_snapshots(account_id,resource_key,handler,version,context,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts,run_id,descriptor)
 values('fixture',$1,'get_market_players',gen_random_uuid(),$2,now(),now(),now()+interval '60 seconds','unknown','ok',1,repeat('a',64),1,gen_random_uuid(),$3)`,[market.resource_key,market.context,market]);
 const repairs=[];
 for(const [i,kind] of ['rival','unknown','backoff','running','old-generation'].entries()){
  const item=descriptor(100+i);
  if(kind==='rival')item.context.user_team_id=item.args.user_team_id='rival';
  if(kind==='unknown')item.context.scoring_version='unknown';
  await rpc('enqueue',['fixture',item,300,0]);
  await q("update worker_jobs set checked_at=now()-interval '10 minutes',due_at=now()+interval '50 minutes' where resource_key=$1",[item.resource_key]);
  if(kind==='backoff')await q("update worker_jobs set last_error='source_rejected' where resource_key=$1",[item.resource_key]);
  if(kind==='running')await q("update worker_jobs set owner='fixture:active',lease_until=now()+interval '60 seconds' where resource_key=$1",[item.resource_key]);
  if(kind==='old-generation')await q('update worker_jobs set generation=generation-1 where resource_key=$1',[item.resource_key]);
  repairs.push([item,await job(item)]);
 }
 const source=await q('select to_jsonb(s) value from worker_snapshots s order by resource_key');
 await db.exec('reset role');await db.exec(migration);await db.exec('set role service_role');
 const repaired=await job(d);assert.equal(new Date(repaired.value.due_at)-new Date(old.checked_at),300000);
 for(const [item,prior] of repairs)assert.deepEqual(await job(item),prior);
 await db.exec('reset role');await db.exec(migration);await db.exec('set role service_role');
 assert.deepEqual(await job(d),repaired);
 console.log('PASS migration repairs an already-promoted own summary once; rival/unknown/stale-generation, running and failed jobs remain untouched');
 await q("update worker_jobs set interval_seconds=3600,base_priority=1,due_at=checked_at+interval '1 hour' where resource_key=$1",[d.resource_key]);
 const before=(await job(d)).value;const started=performance.now();await discover(d);const elapsed=Math.round(performance.now()-started);
 let after=await job(d);
 assert.equal(after.value.interval_seconds,300);assert.equal(new Date(after.value.due_at)-new Date(before.checked_at),300000);
 assert.ok(new Date(after.value.due_at)<new Date());assert.equal(after.value.checked_at,before.checked_at);assert.equal(after.value.requested_at,before.requested_at);
 assert.deepEqual(await q('select to_jsonb(s) value from worker_snapshots s order by resource_key'),source);
 console.log(`PASS autonomous promotion follows last completion+300s immediately (${elapsed}ms), preserving source stamps, validity, successful history and request identity`);
 await discover(d);assert.deepEqual(await job(d),after);
 await rpc('enqueue_batch',['fixture','fixture:lane',account.generation,[{...d,interval_seconds:3600}]]);assert.deepEqual(await job(d),after);
 console.log('PASS equal or slower rediscovery leaves the entire job and row version unchanged');
 const cases=[['running',"owner='fixture:busy',lease_until=now()+interval '60 seconds'"],
  ['backoff',"last_error='source_rate_limited'"],['unchecked','checked_at=null']];
 for(const [i,[label,change]] of cases.entries()){
  const item=descriptor(i+2);await rpc('enqueue',['fixture',item,3600,0]);
  await q("update worker_jobs set checked_at=now()-interval '10 minutes',due_at=now()+interval '50 minutes' where resource_key=$1",[item.resource_key]);
  await q(`update worker_jobs set ${change} where resource_key=$1`,[item.resource_key]);
  const prior=(await job(item)).value;await discover(item);const next=(await job(item)).value;
  assert.equal(next.interval_seconds,300);for(const field of ['due_at','owner','lease_until','checked_at','last_error','requested_at'])assert.deepEqual(next[field],prior[field],label+' '+field);
 }
 console.log('PASS active leases, failed-source backoff and unchecked schedules are never accelerated');
 const recent=descriptor(6);await rpc('enqueue',['fixture',recent,3600,0]);
 await q("update worker_jobs set checked_at=now()-interval '100 seconds',due_at=now()+interval '3500 seconds' where resource_key=$1",[recent.resource_key]);
 await discover(recent);const next=(await job(recent)).value;
 assert.equal(new Date(next.due_at)-new Date(next.checked_at),300000);assert.ok(new Date(next.due_at)>new Date());
 const early=descriptor(7);await rpc('enqueue',['fixture',early,3600,0]);
 await q("update worker_jobs set checked_at=now()-interval '10 minutes',due_at=now()-interval '10 minutes' where resource_key=$1",[early.resource_key]);
 const earlyBefore=(await job(early)).value;await discover(early);assert.equal((await job(early)).value.due_at,earlyBefore.due_at);
 console.log('PASS a recent successful check waits the remaining cadence and an already-earlier job never moves later');
 const manual=descriptor(8);await rpc('enqueue',['fixture',manual,3600,0]);
 await q("update worker_jobs set due_at=now()+interval '1 hour',requested_at=now()-interval '1 hour' where resource_key=$1",[manual.resource_key]);
 await rpc('enqueue',['fixture',manual,3600,10]);const manualAfter=(await job(manual)).value;
 assert.equal(manualAfter.priority,10);assert.ok(new Date(manualAfter.due_at)<=new Date());assert.ok(new Date(manualAfter.requested_at)>Date.now()-5000);
 const privileges=await q("select has_function_privilege('anon','worker_enqueue(text,jsonb,integer,integer)','execute') anon,has_function_privilege('authenticated','worker_enqueue(text,jsonb,integer,integer)','execute') authenticated,has_function_privilege('service_role','worker_enqueue(text,jsonb,integer,integer)','execute') service");
 assert.deepEqual(privileges,[{anon:false,authenticated:false,service:true}]);
 console.log('PASS explicit manual priority and service-only function access remain unchanged');
}finally{await db.close()}})().catch(e=>{console.error(e.stack,e.detail||'');process.exitCode=1});
