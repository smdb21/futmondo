const fs=require('node:fs'),assert=require('node:assert/strict');
const {PGlite}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/@electric-sql/pglite');
(async()=>{const db=new PGlite();try{
 await db.exec('create role anon;create role authenticated;create role service_role bypassrls;');
 await db.exec(fs.readFileSync('scripts/schema.sql','utf8'));
 for(const file of ['20261114043816_worker_database_read_boundary.sql','20261114043932_worker_autonomous_cadence.sql'])await db.exec(fs.readFileSync('scripts/migrations/'+file,'utf8'));
 const q=async(s,a=[]) => (await db.query(s,a)).rows;
 const rpc=async(n,a)=>(await q(`select worker_${n}(${a.map((_,i)=>'$'+(i+1)).join(',')}) result`,a))[0].result;
 await db.exec('set role service_role');
 await rpc('authorize',['fixture',true,'synthetic','UTC',0,0,[],'fixture']);
 await q("update worker_accounts set verified_leagues=$1 where account_id='fixture'",[{league:{competition_id:'competition',season:'season',scoring_version:'scoring'}}]);
 await db.exec(`insert into worker_jobs(account_id,resource_key,descriptor,generation,interval_seconds)
 select account_id,key,jsonb_build_object('version',1,'resource_key',key,'handler','get_player_summary',
 'args',jsonb_build_object('championship_id','league','user_team_id',team,'player_id',player),
 'context',jsonb_build_object('account_id',account_id,'championship_id','league','user_team_id',team,'player_id',player,
 'competition_id',case when unknown then 'unknown' else 'competition' end,
 'season',case when unknown then 'unknown' else 'season' end,
 'scoring_version',case when unknown then 'unknown' else 'scoring' end)),generation,3600
 from worker_accounts a cross join lateral (
 select md5(kind||i)||md5(i||kind) key,i::text player,team,unknown from (values
 ('own','own-team',false,527),('blank','',false,527),('unknown-blank','',true,527),
 ('unknown-own','own-team',true,292),('rival','rival-team',false,20)) kinds(kind,team,unknown,n)
 cross join lateral generate_series(1,n) i) jobs where a.account_id='fixture';
 insert into worker_snapshots(account_id,resource_key,handler,version,context,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts,run_id,descriptor)
 select account_id,resource_key,'get_player_summary',gen_random_uuid(),descriptor->'context',now()-interval '1 day',now()-interval '1 day',now()-interval '23 hours','unknown','ok',1,repeat('a',64),1,gen_random_uuid(),descriptor
 from worker_jobs where descriptor->>'handler'='get_player_summary';
 insert into worker_chunks(account_id,resource_key,version,part,data)
 select account_id,resource_key,version,0,'fixture' from worker_snapshots;`);
 assert.equal((await q("select count(*)::integer n from worker_jobs where descriptor->>'handler'='get_player_summary'"))[0].n,1893);
 const migration=fs.readFileSync('scripts/migrations/20261114135558_worker_summary_job_retirement.sql','utf8');
 const started=performance.now();await db.exec(migration);const elapsed=Math.round(performance.now()-started);
 assert.equal((await q("select count(*)::integer n from worker_jobs where descriptor->>'handler'='get_player_summary'"))[0].n,567);
 assert.equal((await q("select count(*)::integer n from worker_jobs where descriptor->'context'->>'user_team_id'=''"))[0].n,21); //20 ambiguous summaries +account bootstrap
 assert.equal((await q("select count(*)::integer n from worker_jobs where descriptor->'context'->>'user_team_id'='rival-team'"))[0].n,20);
 assert.equal((await q('select count(*)::integer n from worker_snapshots'))[0].n,1893);
 assert.equal((await q('select count(*)::integer n from worker_chunks'))[0].n,1893);
 await db.exec(migration);assert.equal((await q("select count(*)::integer n from worker_jobs where descriptor->>'handler'='get_player_summary'"))[0].n,567);
 console.log(`PASS retirement removes819 superseded unknown jobs and507 unambiguous blank-team duplicates (${elapsed}ms); all facts,20 ambiguous blank teams and20 rival scopes survive; repeat is inert`);
 // Separate synthetic accounts prevent an unrelated scope from providing proof.
 const context={account_id:'',championship_id:'league',user_team_id:'own-team',player_id:'p',competition_id:'competition',season:'season',scoring_version:'scoring'};
 const cases=['running','wrong-generation','unknown-membership','known-conflict','different-args','partial-peer','failed-peer','missing-peer','disabled','excluded','blocked'];
 const oldKeys=[];
 for(const [i,kind] of cases.entries()){
  const account='case-'+kind;await rpc('authorize',[account,true,'synthetic','UTC',0,0,[],'fixture']);
  const c={...context,account_id:account,championship_id:kind==='blocked'?'blocked-league':'league'};
  await q('update worker_accounts set verified_leagues=$1 where account_id=$2',[{[c.championship_id]:{competition_id:'competition',season:'season',scoring_version:'scoring'}},account]);
  const args={championship_id:c.championship_id,user_team_id:c.user_team_id,player_id:c.player_id};
  const peer={version:1,resource_key:(20+i).toString(16).padStart(64,'0'),handler:'get_player_summary',context:c,args};
  const old={...peer,resource_key:(40+i).toString(16).padStart(64,'0'),context:{...c,competition_id:'unknown',season:kind==='known-conflict'?'different-season':'unknown',scoring_version:'unknown'},args:kind==='different-args'?{...args,extra:true}:args};
  await rpc('enqueue',[account,peer,3600,0]);await rpc('enqueue',[account,old,3600,0]);oldKeys.push(old.resource_key);
  if(kind!=='missing-peer')await q(`insert into worker_snapshots(account_id,resource_key,handler,version,context,observed_at,checked_at,valid_until,completeness,status,json_bytes,sha256,parts,run_id,descriptor)
   values($1,$2,'get_player_summary',gen_random_uuid(),$3,now(),now(),now(),$6,$4,1,repeat('a',64),1,gen_random_uuid(),$5)`,[account,peer.resource_key,c,kind==='failed-peer'?'error':'ok',peer,kind==='partial-peer'?'partial':'unknown']);
  if(kind==='running')await q("update worker_jobs set owner='fixture:active',lease_until=now()+interval '1 minute' where resource_key=$1",[old.resource_key]);
  if(kind==='wrong-generation')await q('update worker_jobs set generation=generation-1 where resource_key=$1',[old.resource_key]);
  if(kind==='unknown-membership')await q("update worker_accounts set verified_leagues='{}' where account_id=$1",[account]);
  if(kind==='disabled')await q('update worker_accounts set enabled=false where account_id=$1',[account]);
  if(kind==='excluded')await q('update worker_accounts set excluded_leagues=$1 where account_id=$2',[[c.championship_id],account]);
  if(kind==='blocked')await q("insert into blocked_leagues(championship_id,sport) values($1,'basketball')",[c.championship_id]);
 }
 await db.exec(migration);
 assert.equal((await q('select count(*)::integer n from worker_jobs where resource_key=any($1::text[])',[oldKeys]))[0].n,cases.length);
 console.log('PASS active leases, stale generations, unverified/foreign identities, conflicting known season, argument mismatch, missing/partial proof, disabled/excluded/blocked scopes all remain untouched');
}finally{await db.close()}})().catch(e=>{console.error(e.stack,e.detail||'');process.exitCode=1});
