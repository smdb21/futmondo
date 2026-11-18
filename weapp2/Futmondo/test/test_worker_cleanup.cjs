const fs=require('node:fs'),assert=require('node:assert/strict');
const {PGlite}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/@electric-sql/pglite');
(async()=>{const db=new PGlite();try{
 await db.exec('create role anon;create role authenticated;create role service_role bypassrls;');
 await db.exec(fs.readFileSync('scripts/schema.sql','utf8'));
 const q=async(s,a=[]) => (await db.query(s,a)).rows;
 await q("insert into worker_accounts(account_id,enabled,encrypted_token,credential_id) values('fixture',true,'synthetic','fixture')");
 const d={handler:'get_notifications',resource_key:'a'.repeat(64),args:[],context:{account_id:'fixture'}};
 await q('select worker_enqueue($1,$2,300,0)',['fixture',d]);
 const job=(await q('select * from worker_jobs limit 1'))[0];
 const version='11111111-1111-1111-1111-111111111111';
 await q(`insert into worker_snapshots(account_id,resource_key,handler,version,context,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts,run_id,descriptor)
 values('fixture',$1,'get_notifications',$2,$3,now(),now(),now(),'complete','ok',1,repeat('a',64),1,$4,$5)`,[job.resource_key,version,d.context,job.run_id,d]);
 await q(`insert into worker_chunks(account_id,resource_key,version,part,data,created_at)
 values('fixture',$1,$2,0,'current',now()-interval '3 days')`,[job.resource_key,version]);
 await q(`insert into worker_chunks(account_id,resource_key,version,part,data,created_at)
 select 'fixture',$1,gen_random_uuid(),0,'obsolete',now()-interval '2 hours' from generate_series(1,5)`,[job.resource_key]);
 await q(`insert into worker_chunks(account_id,resource_key,version,part,data) values('fixture',$1,gen_random_uuid(),0,'upload')`,[job.resource_key]);
 for(const current of [true,false])await q(`insert into worker_checkpoints(account_id,resource_key,run_id,request_key,response,created_at) values('fixture',$1,$2,$3,'{}',now()-interval '3 days')`,[job.resource_key,current?job.run_id:'22222222-2222-2222-2222-222222222222',current?'active':'old']);
 await db.exec('set role service_role');
 let r=(await q('select worker_cleanup_batch(2) as result'))[0].result;
 assert.equal(r.chunks,2);assert.equal(r.checkpoints,1);
 r=(await q('select worker_cleanup_batch(2) as result'))[0].result;assert.equal(r.chunks,2);
 r=(await q('select worker_cleanup_batch(2) as result'))[0].result;assert.equal(r.chunks,1);
 assert.deepEqual((await q('select data from worker_chunks order by data')).map(x=>x.data),['current','upload']);
 assert.equal((await q('select request_key from worker_checkpoints'))[0].request_key,'active');
 assert.equal(Number((await q('select chunks_deleted from worker_cleanup_status'))[0].chunks_deleted),5);
 assert.equal((await q('select worker_cleanup_batch(2) as result'))[0].result.chunks,0);
 await assert.rejects(q('select worker_cleanup_batch(0)'));
 await db.exec('set role anon');await assert.rejects(q('select worker_cleanup_batch(2)'));
 console.log('PASS bounded cleanup, live snapshot/upload protection, active checkpoint retention, restart-safe draining, status and permissions');
}finally{await db.close();}})().catch(e=>{console.error(e);process.exit(1)});
