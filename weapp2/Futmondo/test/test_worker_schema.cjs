const fs = require('fs');
const assert = require('assert/strict');
const {PGlite} = require(process.env.FUTMONDO_TEST_NODE_MODULES ? process.env.FUTMONDO_TEST_NODE_MODULES + '/@electric-sql/pglite' : '/tmp/futmondo-review-tools/node_modules/@electric-sql/pglite');
const migration = fs.readdirSync('scripts/migrations').find(x => x.endsWith('_background_acquisition.sql'));
(async()=>{
 const db=new PGlite();let count=0;
 const pass=x=>{count++;console.log('PASS '+x)};
 const q=async(sql,args=[]) => (await db.query(sql,args)).rows;
 const rpc=async(name,args)=> {if(name==='authorize'&&args.length===7)args=[...args,'synthetic-generation'];if(name==='renew'&&args.length===2)args=[...args,'synthetic-renewal'];return (await q(`select worker_${name}(${args.map((_,i)=>'$'+(i+1)).join(',')}) result`,args))[0].result;};
 try{
  await db.exec('create role anon;create role authenticated;create role service_role bypassrls;');
  await db.exec(fs.readFileSync('scripts/schema.sql','utf8'));
  await db.exec(fs.readFileSync('scripts/migrations/'+migration,'utf8'));
  await db.exec(fs.readFileSync('scripts/migrations/20261112021500_worker_catalog_upserts.sql','utf8'));
  await db.exec('set role service_role');
  assert.equal(await rpc('elect',['leader']),true);
  assert.equal(await rpc('elect',['other']),false);
  const account=await rpc('authorize',['u',true,'encrypted-fixture','America/Los_Angeles',0,7,'[]']);
  assert.equal(account.enabled,true);assert.ok(!('encrypted_token' in account));
  await assert.rejects(rpc('authorize',['bad',true,'x','Invalid/Timezone',0,7,'[]']));
  pass('repeatable schema and validated account policy; responses never return credential ciphertext');
  const descriptor={version:1,handler:'get_teams',args:{championship_id:'l'},context:{account_id:'u',championship_id:'l'}};
  descriptor.resource_key='a'.repeat(64);
  assert.equal(await rpc('enqueue',['u',descriptor,60,10]),true);
  assert.equal(await rpc('enqueue',['u',descriptor,60,10]),true);
  assert.equal((await q('select count(*)::int n from worker_jobs'))[0].n,1);
  const job=await rpc('claim',['leader:one',false]);
  assert.equal(job.account_id,'u');assert.equal(await rpc('claim',['leader:two',false]),null);
  assert.equal(await rpc('guard',['u',descriptor.resource_key,'leader:two',job.generation,null]),false);
  pass('durable request coalescing, exclusive leases and owner guards');
  const manifest={version:'11111111-1111-4111-8111-111111111111',context:descriptor.context,parts:1,json_bytes:10,sha256:'b'.repeat(64),
   observed_at:'2026-11-10T12:00:00Z',checked_at:'2026-11-10T12:00:00Z',valid_until:'2026-11-10T12:01:00Z',status:'ok',completeness:'unknown'};
  await assert.rejects(rpc('publish',['u',descriptor.resource_key,'leader:one',job.generation,manifest]));
  await q('insert into worker_chunks(account_id,resource_key,version,part,data) values($1,$2,$3,0,$4)',['u',descriptor.resource_key,manifest.version,'Zml4dHVyZQ==']);
  assert.equal(await rpc('publish',['u',descriptor.resource_key,'leader:one',job.generation,manifest]),true);
  const read=await rpc('read',['u',{[descriptor.resource_key]:''}]);
  assert.equal(read.resources.length,1);assert.equal(read.resources[0].manifest.observed_at,'2026-11-10T12:00:00+00:00');
  assert.equal((await rpc('read',['u',{[descriptor.resource_key]:manifest.version}])).resources.length,0);
  assert.equal((await rpc('read',['other',{[descriptor.resource_key]:''}])).resources.length,0);
  pass('manifest publication requires chunks, preserves source time, filters versions and isolates accounts');
  const cold={...descriptor,resource_key:'c'.repeat(64)};
  await rpc('enqueue',['u',cold,21600,0]);
  await q("update worker_jobs set due_at=now()-interval '1 second',checked_at=now()-interval '2 hours' where resource_key=$1",[descriptor.resource_key]);
  const priority=await rpc('claim',['leader:priority',false]);assert.equal(priority.resource_key,descriptor.resource_key);
  assert.equal((await q('select base_priority from worker_jobs where resource_key=$1',[descriptor.resource_key]))[0].base_priority,4);
  await q('update worker_jobs set owner=null,lease_until=null where resource_key=$1',[descriptor.resource_key]);
  await rpc('enqueue',['u',descriptor,60,10]);
  const factJob=await rpc('claim',['leader:facts',false]);assert.ok(factJob);
  const write=(payload,when)=>rpc('write_fact',['u',descriptor.resource_key,'leader:facts',factJob.generation,'players',payload,'id',false,when]);
  await write({id:'worker-player',name:'New name',slug:'worker-player'},'2026-11-10T12:00:00Z');
  await write({id:'worker-player',name:'Old name',slug:'worker-player'},'2026-11-09T12:00:00Z');
  assert.equal((await q("select name from players where id='worker-player'"))[0].name,'New name');
  await assert.rejects(rpc('write_fact',['u',descriptor.resource_key,'leader:facts',factJob.generation,'worker_accounts',{},'account_id',false,'2026-11-10T12:00:00Z']));
  pass('guarded fact writes reject stale mutable metadata and non-factual tables');
  // A missing conflict target means PostgREST's default primary-key merge,
  // not INSERT. These catalogs are already seeded before most worker reads.
  await q("insert into championships(id,name) values('l','League')");
  for (const [table,first,second] of [
   ['user_teams',{id:'catalog-team',championship_id:'l',name:'Before'},{id:'catalog-team',championship_id:'l',name:'After'}],
   ['championships',{id:'catalog-league',name:'Before'},{id:'catalog-league',name:'After'}],
   ['real_clubs',{id:'catalog-club',name:'Before'},{id:'catalog-club',name:'After'}],
   ['players',{id:'catalog-player',name:'Before',slug:'catalog-player'},{id:'catalog-player',name:'After',slug:'catalog-player'}]
  ]) {
   const save=(payload,when)=>rpc('write_fact',['u',descriptor.resource_key,'leader:facts',factJob.generation,table,payload,null,false,when]);
   assert.equal(await save(first,'2026-11-09T12:00:00Z'),true);
   assert.equal(await save(second,'2026-11-10T12:00:00Z'),true);
   assert.equal(await save(first,'2026-11-09T12:00:00Z'),true);
   const rows=await q(`select name from ${table} where id=$1`,[first.id]);
   assert.deepEqual(rows,[{name:'After'}]);
  }
  pass('default catalog writes merge by ID, remain unique and reject older observations');

  const checkpoint={fixture:true};
  await rpc('checkpoint',['u',descriptor.resource_key,'leader:facts',factJob.generation,'request-1',checkpoint]);
  assert.deepEqual(await rpc('checkpoint',['u',descriptor.resource_key,'leader:facts',factJob.generation,'request-1',null]),checkpoint);
  await assert.rejects(rpc('checkpoint',['u',descriptor.resource_key,'other',factJob.generation,'request-1',null]));
  pass('durable source checkpoints survive consumers and enforce the active lease');
  await rpc('enqueue',['u',descriptor,60,10]);
  assert.equal(await rpc('publish',['u',descriptor.resource_key,'leader:facts',factJob.generation,manifest]),true);
  assert.equal((await q("select priority from worker_jobs where account_id='u'"))[0].priority,10);
  pass('refresh requested during a run remains queued after old work publishes');
  await q("update worker_jobs set due_at=now()-interval '1 hour',checked_at=now()-interval '1 hour'");
  const reclaimed=await rpc('claim',['leader:two',false]);assert.ok(reclaimed);
  await rpc('authorize',['u',false,null,'UTC',0,7,'[]']);
  assert.equal(await rpc('guard',['u',descriptor.resource_key,'leader:two',reclaimed.generation,null]),false);
  assert.equal(await rpc('publish',['u',descriptor.resource_key,'leader:two',reclaimed.generation,manifest]),false);
  pass('revocation rejects already leased jobs and late snapshots');
  await rpc('authorize',['u',true,'new','UTC',0,7,'["l"]']);
  assert.equal(await rpc('enqueue',['u',descriptor,60,10]),false);
  await db.exec('reset role;set role anon');await assert.rejects(q('select * from worker_accounts'));
  await assert.rejects(rpc('read',['u',{}]));
  await db.exec('reset role;set role authenticated');await assert.rejects(q('select * from worker_snapshots'));
  pass('excluded leagues and browser roles cannot acquire private data');
  await db.exec('reset role;set role service_role');
  await q("update worker_chunks set created_at=now()-interval '2 days'");
  await rpc('cleanup',[]);
  assert.equal((await q('select count(*)::int n from worker_chunks'))[0].n,1);
  pass('cleanup preserves every chunk referenced by a current manifest');
  console.log('WORKER SCHEMA: '+count+' groups passed');
 }finally{await db.close()}
})().catch(e=>{console.error(e);process.exitCode=1});
