const fs=require('node:fs'),assert=require('node:assert/strict');
const {PGlite}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/@electric-sql/pglite');
(async()=>{const db=new PGlite();try{
const q=async(sql,args=[]) => (await db.query(sql,args)).rows;
await db.exec('create role anon;create role authenticated;create role service_role bypassrls;');
await db.exec(fs.readFileSync('scripts/schema.sql','utf8').split('-- Normalize repeated historical facts')[0]);
await db.exec("insert into championships(id,name) values('c','fixture');insert into players(id,name,slug) values('p','fixture','fixture')");
const base={player_id:'p',championship_id:'c',competition_id:'league',season:'season'};
const specs=[['price','player_history',{...base,value:100,points:-2,recorded_at:'2026-01-01T01:02:03.123456Z'},['value','change','points','avg_points','avg_last_five','matches']],['match','player_match_observations',{...base,scoring_version:'score',round:1,source_round_key:'r',points:-3,score_status:'final'},['points','score_status','is_home','minutes_raw','start_raw','participation','round_id','match_id','source_round_number','occurred_at','round_start_at']],['finance','rival_finance_evidence',{account_id:'a',championship_id:'c',competition_id:'league',season:'season',evidence_type:'movements',source_id:'x',payload:{amount:-10,missing:null,empty:[],flag:false}},['payload']]];
for(const [pre,table,row,facts] of specs){
 const insert=async(r)=>{const keys=Object.keys(r);return q(`insert into ${table}(${keys.join(',')}) select ${keys.join(',')} from jsonb_populate_record(null::${table},$1)`,[r]);};
 for(let i=1;i<=6;i++)await insert({...row,...(i===4?(pre==='finance'?{payload:{amount:0}}:{points:0}):{}),observed_at:`2026-01-0${i}T00:00:00.123456Z`});
 await insert({...row,competition_id:'another',observed_at:'2026-01-01T00:00:00Z'});
 if(pre==='finance')for(let i=0;i<40;i++)for(let j=0;j<3;j++)await insert({...row,competition_id:'large-payload',payload:{body:'x'.repeat(65536),revision:i},observed_at:new Date(Date.UTC(2026,1,1,0,i*3+j)).toISOString()});
 if(pre==='price')await db.exec("insert into player_history(value) values(0),(0)");
 await db.exec(fs.readFileSync(`scripts/history_batches/prepare_${pre}.sql`,'utf8'));
 const cut=fs.readFileSync(`scripts/history_batches/cutover_${pre}.sql`,'utf8');
 await assert.rejects(db.exec(cut));await db.exec('rollback');
 assert.equal((await q(`select relkind from pg_class where oid='${table}'::regclass`))[0].relkind,'r');
 await q(`select ${pre}_migration_seed(2)`);
 assert.equal((await q(`select seeded from ${pre}_migration_status`))[0].seeded,false);
 // An identity sorting before the discovery cursor is covered by the change tracker.
 await insert({...row,competition_id:'000-late',observed_at:'2025-01-01T00:00:00Z'});
 for(let i=0;i<100;i++){if((await q(`select ${pre}_migration_seed(2) n`))[0].n===0)break;}
 assert.equal((await q(`select seeded from ${pre}_migration_status`))[0].seeded,true);
 assert.equal((await q(`select ${pre}_migration_seed() n`))[0].n,0);
 const n=(await q(`select count(*) n from ${pre}_migration_queue`))[0].n;
 await db.exec('begin');await q(`select ${pre}_migration_batch(1)`);await db.exec('rollback');
 assert.equal((await q(`select count(*) n from ${pre}_migration_queue`))[0].n,n);
 const first=(await q(`select ${pre}_migration_batch(1) result`))[0].result;
 assert.equal(first.processed,1);assert.equal(first.pending,Number(n)-1);
 const drain=async()=>{for(let i=0;i<10;i++){const r=(await q(`select ${pre}_migration_batch(2) result`))[0].result;if(!r.pending)return;}throw Error('did not drain');};
 await drain();
 assert.equal((await q(`select ${pre}_migration_batch(2) result`))[0].result.processed,0);
 // Changes after a successful copy invalidate verification and survive restart/resume.
 await insert({...row,observed_at:'2026-01-07T00:00:00.123456Z'});
 await q(`update ${table} set competition_id='moved' where competition_id='another'`);
 await q(`delete from ${table} where observed_at='2026-01-01T00:00:00.123456Z'`);
 await assert.rejects(db.exec(cut));await db.exec('rollback');
 await assert.rejects(db.exec(`truncate ${table}`));
 await drain();
 const originals=(await q(`select to_jsonb(t) value from ${table} t`)).map(x=>x.value);
 // Independent JavaScript known-answer transformation: keep both ends of each factual run.
 const metadata=new Set([...facts,'id','observed_at','recorded_at']);
 const groups=new Map();
 for(const r of originals){const key=JSON.stringify(Object.entries(r).filter(([k])=>!metadata.has(k)));if(!groups.has(key))groups.set(key,[]);groups.get(key).push(r);}
 const expected=[];
 for(const rows of groups.values()){
 rows.sort((a,b)=>String(a.observed_at??'z').localeCompare(String(b.observed_at??'z'))||(a.id??0)-(b.id??0));
 const equal=(a,b)=>b&&JSON.stringify(facts.map(k=>a[k]))===JSON.stringify(facts.map(k=>b[k]));
 rows.forEach((r,i)=>{if(r.observed_at==null||(pre==='price'&&(r.player_id==null||r.championship_id==null))||!equal(r,rows[i-1])||!equal(r,rows[i+1]))expected.push(r);});
 }
 await db.exec(cut);
 const actual=(await q(`select to_jsonb(t) value from ${table} t`)).map(x=>x.value);
 const sort=a=>a.map(x=>JSON.stringify(x)).sort();assert.deepEqual(sort(actual),sort(expected));
 assert.equal((await q(`select relkind from pg_class where oid='${table}'::regclass`))[0].relkind,'v');
 assert.ok((await q(`select completed_at from ${pre}_migration_status`))[0].completed_at);
 await db.exec('set role anon');await assert.rejects(q(`select * from ${table}`));await assert.rejects(q(`select * from ${pre}_migration_verified`));await db.exec('reset role');
 console.log(`PASS ${pre}: batch bounds, rollback/resume, live insert/key update/delete tracking, blocked premature switch, exact retained rows, nullable identities and private grants`);
}
}finally{await db.close();}})().catch(e=>{console.error(e);process.exit(1)});
