// Isolated database lifecycle: no accounts, credentials or production I/O.
const fs=require('node:fs'),assert=require('node:assert/strict');
const {PGlite}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/@electric-sql/pglite');
(async()=>{const db=new PGlite();try{
 await db.exec('create role anon;create role authenticated;create role service_role bypassrls;');
 await db.exec(fs.readFileSync('scripts/schema.sql','utf8'));
 const migration=fs.readdirSync('scripts/migrations').find(x=>x.endsWith('_database_first_sync.sql'));
 await db.exec(fs.readFileSync('scripts/migrations/'+migration,'utf8'));
 const q=async(sql,args=[]) => (await db.query(sql,args)).rows;
 const rpc=async(name,args)=>(await q(`select worker_${name}(${args.map((_,i)=>'$'+(i+1)).join(',')}) result`,args))[0].result;
 await db.exec('set role service_role');
 let a=await rpc('connect',['fixture','synthetic','credential-1',0,'UTC']);
 assert.equal(a.enabled,true);assert.equal(a.quiet_end,0);assert.ok(!('encrypted_token' in a));
 const seed=(await q('select * from worker_jobs'))[0];assert.equal(seed.descriptor.handler,'get_active_championships');
 await rpc('elect',['leader']);
 await q("update worker_jobs set owner='leader:0',lease_until=now()+interval '60 seconds',started_at=now(),checkpoint='{\"step\":2}',attempts=3");
 const before=await q('select * from worker_jobs');
 for(const credential of ['credential-2','credential-3']){
  const updated=await rpc('connect',['fixture','synthetic',credential,a.generation,'UTC']);
  assert.equal(updated.generation,a.generation);assert.deepEqual(await q('select * from worker_jobs'),before);
 }
 await rpc('renew',['fixture','synthetic','credential-4']);
 assert.deepEqual(await q('select * from worker_jobs'),before);
 console.log('PASS automatic and repeated login preserve generation, lease, checkpoint, attempts, run identity and due time');
 const version='11111111-1111-4111-8111-111111111111';
 await q(`insert into worker_snapshots(account_id,resource_key,context,handler,version,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts,run_id,descriptor)
 values('fixture',$1,$2,'get_active_championships',$3,now()-interval '1 day',now()-interval '1 day',now()-interval '23 hours','complete','ok',1,$4,1,gen_random_uuid(),$5)`,[seed.resource_key,seed.descriptor.context,version,'a'.repeat(64),seed.descriptor]);
 await q('insert into worker_chunks(account_id,resource_key,version,part,data) values($1,$2,$3,0,$4)',['fixture',seed.resource_key,version,'synthetic']);
 await db.exec('begin read only');const bootstrap=await rpc('bootstrap',['fixture']);await db.exec('commit');
 assert.equal(bootstrap.resources.length,1);assert.equal(bootstrap.resources[0].manifest.version,version);
 assert.equal(bootstrap.preferred_league,null);assert.ok(!('encrypted_token' in bootstrap.account));
 assert.deepEqual(await q('select * from worker_jobs'),before);
 console.log('PASS bootstrap reads expired persisted data in a read-only transaction without acquisition or timestamp changes');
 let prepared=await rpc('prepare_source',['fixture',seed.resource_key,'leader:0',a.generation,'request']);
 assert.equal(prepared.account.credential_id,'credential-4');assert.equal(prepared.checkpoint,null);
 const response={manifest:{parts:1,json_bytes:1,sha256:'a'.repeat(64)},chunks:['fixture']};
 await rpc('checkpoint',['fixture',seed.resource_key,'leader:0',a.generation,'request',response]);
 prepared=await rpc('prepare_source',['fixture',seed.resource_key,'leader:0',a.generation,'request']);
 assert.deepEqual(prepared.checkpoint,response);
 console.log('PASS one guarded source-preparation RPC supplies current access and durable restart checkpoints');
 const revoked=await rpc('disconnect',['fixture',a.generation]);
 assert.equal(revoked.enabled,false);assert.equal(revoked.generation,a.generation+1);
 assert.equal(await rpc('connect',['fixture','synthetic','late',a.generation,'UTC']),null);
 assert.equal(await rpc('renew',['fixture','synthetic','late']),null);
 assert.equal(await rpc('prepare_source',['fixture',seed.resource_key,'leader:0',a.generation,'request']),null);
 assert.equal((await q('select count(*)::int n from worker_jobs'))[0].n,0);
 assert.equal((await rpc('bootstrap',['fixture'])).resources.length,1);
 assert.equal((await q("select encrypted_token from worker_accounts where account_id='fixture'"))[0].encrypted_token,null);
 console.log('PASS revocation clears access/jobs, defeats late renewal, and preserves stored observations');
 a=await rpc('connect',['fixture','synthetic','reconnect',revoked.generation,'UTC']);
 assert.equal(a.enabled,true);assert.equal((await q('select count(*)::int n from worker_jobs'))[0].n,1);
 const scope={account_id:'fixture',championship_id:'l',user_team_id:'t',player_id:'',competition_id:'c',season:'s',scoring_version:'v'};
 await q('update worker_accounts set verified_leagues=$1',[{l:{competition_id:'c',season:'s',scoring_version:'v'}}]);
 const descriptor=(handler,id,args={championship_id:'l',user_team_id:'t'})=>({version:1,handler,resource_key:id.repeat(64),args,context:scope});
 await rpc('enqueue',['fixture',descriptor('get_user_team_info','a'),60,0]);
 await q("update worker_jobs set owner='leader:0',lease_until=now()+interval '60 seconds' where resource_key=$1",['a'.repeat(64)]);
 await rpc('preferences',['fixture',a.generation,['l']]);
 assert.equal(await rpc('guard',['fixture','a'.repeat(64),'leader:0',a.generation]),false);
 assert.equal((await q("select count(*)::int n from worker_jobs where descriptor->'context'->>'championship_id'='l'"))[0].n,0);
 await rpc('preferences',['fixture',a.generation,[]]);
 console.log('PASS excluded leagues invalidate collection without resetting unrelated account discovery');
 await q('delete from worker_jobs');
 const families=['get_user_team_info','get_market_players','get_championship_pressroom','get_player_summary','get_player_full_profile','fm_worker_finances'];
 for(let i=0;i<families.length;i++)await rpc('enqueue',['fixture',descriptor(families[i],(i+1).toString(16)),i<3?60:3600,0]);
 await q(`insert into worker_jobs(account_id,resource_key,descriptor,generation,interval_seconds)
 select 'fixture',md5(i::text)||md5('bulk'||i::text),jsonb_build_object('version',1,'handler','get_player_summary','args',jsonb_build_object('player_id',i::text),'context',$1::jsonb),$2,3600 from generate_series(1,527) i`,[scope,a.generation]);
 const start=performance.now(),seen=[];
 for(let i=0;i<families.length;i++){
  const job=await rpc('claim',['leader:0',false]);assert.ok(job);seen.push(job.descriptor.handler);
  await q('update worker_jobs set owner=null,lease_until=null where resource_key=$1',[job.resource_key]);
 }
 assert.deepEqual([...new Set(seen)].sort(),families.sort());
 console.log(`PASS all six families progress with 527 bulk jobs (${Math.round(performance.now()-start)} ms for six claims)`);
 const privileges=await q(`select p.proname,has_function_privilege('anon',p.oid,'execute') a,has_function_privilege('authenticated',p.oid,'execute') b
 from pg_proc p join pg_namespace n on n.oid=p.pronamespace where n.nspname='public' and p.proname in ('worker_connect','worker_bootstrap','worker_disconnect','worker_preferences','worker_prepare_source')`);
 assert.equal(privileges.length,5);assert.ok(privileges.every(x=>!x.a&&!x.b));
 console.log('PASS all new RPCs deny browser roles; no new public credential surface');
}finally{await db.close()}})().catch(e=>{console.error(e.stack);process.exitCode=1});
