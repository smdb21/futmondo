const fs = require('fs');
const assert = require('assert/strict');
const {PGlite} = require((process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules') + '/@electric-sql/pglite');
(async () => {
 const db = new PGlite(); let groups = 0;
 const q = async (sql, args=[]) => (await db.query(sql,args)).rows;
 const rpc = async (name,args) => (await q(`select worker_${name}(${args.map((_,i)=>'$'+(i+1)).join(',')}) result`,args))[0].result;
 const pass = label => {groups++; console.log('PASS '+label)};
 try {
  await db.exec('create role anon;create role authenticated;create role service_role bypassrls;');
  await db.exec(fs.readFileSync('scripts/schema.sql','utf8'));
  const migration = fs.readFileSync('scripts/migrations/20261113043655_worker_discovery_batches.sql','utf8');
  await db.exec(migration); await db.exec(migration);
  await db.exec('set role service_role');
  await rpc('elect',['leader']);
  const account = await rpc('authorize',['u',true,'fixture','UTC',0,0,[],'fixture']);
  const d = i => ({version:1,resource_key:i.toString(16).padStart(64,'0'),handler:'get_player_summary',args:{player_id:'p'+i},
   context:{account_id:'u',championship_id:'league',player_id:'p'+i},interval_seconds:3600});
  const batch = values => rpc('enqueue_batch',['u','leader:one',account.generation,values]);
  const first = Array.from({length:128},(_,i)=>d(i+1));
  assert.equal(await batch(first),128);
  let rows = await q('select resource_key,ctid::text,descriptor,requested_at,priority,interval_seconds from worker_jobs order by resource_key');
  assert.equal(rows.length,128);assert.equal(rows[0].interval_seconds,3600);
  assert.ok(rows.every(r=>!('interval_seconds' in r.descriptor)));
  assert.equal(await batch(first),128);
  assert.deepEqual(await q('select resource_key,ctid::text,descriptor,requested_at,priority,interval_seconds from worker_jobs order by resource_key'),rows);
  pass('128 durable jobs per call, exact descriptor identity and no row writes for unchanged rediscovery');
  await rpc('enqueue',['u',rows[0].descriptor,3600,10]);
  const foreground = await q('select ctid::text,priority,requested_at,due_at from worker_jobs where resource_key=$1',[d(1).resource_key]);
  await batch(first);
  assert.deepEqual(await q('select ctid::text,priority,requested_at,due_at from worker_jobs where resource_key=$1',[d(1).resource_key]),foreground);
  assert.equal((await rpc('claim',['leader:foreground',true])).resource_key,d(1).resource_key);
  const faster = {...d(2),interval_seconds:60};assert.equal(await batch([faster]),1);
  assert.equal((await q('select interval_seconds,base_priority from worker_jobs where resource_key=$1',[d(2).resource_key]))[0].interval_seconds,60);
  pass('rediscovery preserves foreground requests and leases; tighter intervals still update');
  await assert.rejects(batch(Array.from({length:129},(_,i)=>d(i+1))));
  await assert.rejects(batch({}));
  await assert.rejects(batch([{...d(999),args:{padding:'x'.repeat(1048576)}}]));
  const foreign = d(1000);foreign.context.account_id='foreign';
  await assert.rejects(batch([d(999),foreign]));
  assert.equal((await q('select count(*)::int n from worker_jobs where resource_key=$1',[d(999).resource_key]))[0].n,0);
  assert.equal(await rpc('enqueue_batch',['u','other:one',account.generation,[d(999)]]),null);
  assert.equal(await rpc('enqueue_batch',['u','leader:one',account.generation-1,[d(999)]]),null);
  pass('count/byte/shape bounds, atomic foreign-context rejection and leader/generation guards');
  const old = await q('select count(*)::int n from worker_jobs');
  await q("update worker_accounts set excluded_leagues='[\"league\"]' where account_id='u'");
  assert.equal(await batch([d(999)]),0);
  assert.deepEqual(await q('select count(*)::int n from worker_jobs'),old);
  await q("update worker_accounts set enabled=false,encrypted_token=null where account_id='u'");
  assert.equal(await batch([d(999)]),null);
  await db.exec('reset role');
  for (const role of ['anon','authenticated']) {
   assert.equal((await q("select has_function_privilege($1,'public.worker_enqueue_batch(text,text,bigint,jsonb)','execute') allowed",[role]))[0].allowed,false);
  }
  pass('excluded/disabled accounts cannot seed jobs and browser roles cannot invoke batching');
  // Independent 527-player, four-resources-per-player cold bootstrap measurement.
  await q("update worker_accounts set enabled=true,encrypted_token='fixture',excluded_leagues='[]' where account_id='u'");
  const many=Array.from({length:2108},(_,i)=>d(2000+i));let calls=0;const start=performance.now();
  for(let i=0;i<many.length;i+=128){assert.equal(await batch(many.slice(i,i+128)),Math.min(128,many.length-i));calls++;}
  assert.equal(calls,17);
  assert.equal((await q('select count(*)::int n from worker_jobs'))[0].n,2236);
  console.log(`MEASURE 2108 discoveries: ${calls} RPCs, ${(performance.now()-start).toFixed(1)} ms local SQL`);
  pass('full catalog bootstrap stays durable with 17 calls instead of 2108');
  console.log(`WORKER DISCOVERY BATCHES: ${groups} groups passed`);
 } finally {await db.close()}
})().catch(error=>{console.error(error);process.exitCode=1});
