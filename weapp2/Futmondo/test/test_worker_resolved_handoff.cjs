const fs=require('fs'),assert=require('assert/strict');
const {PGlite}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/@electric-sql/pglite');
(async()=>{const db=new PGlite();try{
 await db.exec('create role anon;create role authenticated;create role service_role bypassrls;');
 await db.exec(fs.readFileSync('scripts/schema.sql','utf8'));
 const migration=fs.readFileSync('scripts/migrations/20261113115929_worker_resolved_handoff.sql','utf8');await db.exec(migration);await db.exec(migration);
 const q=async(s,a=[]) => (await db.query(s,a)).rows;
 const rpc=async(n,a)=>(await q(`select worker_${n}(${a.map((_,i)=>'$'+(i+1)).join(',')}) result`,a))[0].result;
 await db.exec('set role service_role');await rpc('elect',['leader']);const a=await rpc('authorize',['u',true,'synthetic','UTC',0,0,[],'fixture']);
 const context={account_id:'u',championship_id:'l',user_team_id:'t',player_id:'',competition_id:'unknown',season:'unknown',scoring_version:'unknown'};
 const unknown={version:1,handler:'get_championship_pressroom',resource_key:'a'.repeat(64),args:{championship_id:'l'},context};
 const resolved={...unknown,resource_key:'b'.repeat(64),context:{...context,competition_id:'c',season:'s',scoring_version:'v'}};
 await rpc('enqueue',['u',unknown,60,10]);await rpc('enqueue',['u',resolved,60,10]);
 assert.equal(await rpc('resolved_peer',['u',unknown]),null);
 await q('update worker_accounts set verified_leagues=$1 where account_id=$2',[{l:{competition_id:'c',season:'s',scoring_version:'v'}},'u']);
 assert.deepEqual(await rpc('resolved_peer',['u',unknown]),resolved);
 for(const d of [{...unknown,context:{...context,season:'other'}},{...unknown,args:{championship_id:'other'}},{...unknown,context:{...context,user_team_id:'other'}},{...unknown,handler:'get_player_recent_points'}])assert.equal(await rpc('resolved_peer',['u',d]),null);
 assert.equal(await rpc('resolved_peer',['other',unknown]),null);
 console.log('PASS redirects require authoritative membership and exact account/arguments/context; known season and historical data never migrate');
 const version='11111111-1111-4111-8111-111111111111';
 await q(`insert into worker_snapshots(account_id,resource_key,handler,version,context,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts,run_id)
 values($1,$2,$3,$4,$5,now()-interval '10 seconds',now()-interval '10 seconds',now()+interval '50 seconds','complete','ok',1,$6,1,gen_random_uuid())`,['u',resolved.resource_key,resolved.handler,version,resolved.context,'c'.repeat(64)]);
 await q('insert into worker_chunks(account_id,resource_key,version,part,data) values($1,$2,$3,0,$4)',['u',resolved.resource_key,version,'fixture']);
 let result=await rpc('read',['u',{[unknown.resource_key]:'',[resolved.resource_key]:''}]);
 assert.deepEqual(result.redirects,[{requested_key:unknown.resource_key,descriptor:resolved}]);
 assert.equal(result.resources.length,1);assert.equal(result.resources[0].key,resolved.resource_key);assert.deepEqual(result.resources[0].manifest.context,resolved.context);
 assert.equal(result.jobs.length,1);assert.equal(result.jobs[0].key,resolved.resource_key);assert.ok(!('encrypted_token' in result.account));
 result=await rpc('read',['u',{[unknown.resource_key]:'',[resolved.resource_key]:version}]);assert.equal(result.resources.length,0);
 assert.equal((await q('select count(*)::int n from worker_jobs'))[0].n,2);
 console.log('PASS unknown requests receive canonical chunks once, preserve context/source time and deduplicate job status without deleting old jobs');
 const clause={...resolved,handler:'get_clause_transfer_summary',resource_key:'d'.repeat(64),args:{championship_id:'l',user_team_id:'t',player_id:'p'},context:{...resolved.context,player_id:'p'}};
 await rpc('enqueue',['u',clause,300,0]);await q("update worker_jobs set due_at=now()+interval '1 hour' where resource_key<>$1",[clause.resource_key]);
 // Model the reported 527-player load and a long composite occupying lane zero.
 const finance={...resolved,handler:'fm_worker_finances',resource_key:'f'.repeat(64)};
 await rpc('enqueue',['u',finance,60,10]);assert.equal((await rpc('claim',['leader:bulk',false])).resource_key,finance.resource_key);
 await q(`insert into worker_jobs(account_id,resource_key,descriptor,generation,priority,base_priority,interval_seconds)
 select 'u',md5(i::text)||md5('fixture'||i::text),jsonb_build_object('version',1,'handler','get_player_recent_points',
 'args',jsonb_build_object('championship_id','l','user_team_id','t','player_id','bulk-'||i),
 'context',jsonb_build_object('account_id','u','championship_id','l','user_team_id','t','player_id','bulk-'||i,'competition_id','c','season','s','scoring_version','v')),
 $1,6,3,300 from generate_series(1,527) i`,[a.generation]);
 const job=await rpc('claim',['leader:short',true]);assert.equal(job.resource_key,clause.resource_key);assert.equal(job.interval_seconds,300);assert.equal(job.base_priority,3);
 console.log('PASS autonomous clause work progresses ahead of 527 bulk demands while a finance job occupies the other lane');
 await q("update worker_accounts set verified_leagues='{}'");assert.equal(await rpc('has_resolved_peer',['u',unknown]),false);
 console.log('PASS unresolved bootstrap stays eligible when authoritative membership is unavailable');
}finally{await db.close()}})().catch(e=>{console.error(e.message,e.detail||'');process.exitCode=1});
