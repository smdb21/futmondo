const fs=require('node:fs'),assert=require('node:assert/strict');
const {PGlite}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/@electric-sql/pglite');
(async()=>{const db=new PGlite();try{
 await db.exec('create role anon;create role authenticated;create role service_role bypassrls;');
 await db.exec(fs.readFileSync('scripts/schema.sql','utf8').split('-- Normalize repeated historical facts')[0]);
 const q=async(s,a=[]) => (await db.query(s,a)).rows;
 await db.exec("insert into championships(id,name) values('c','fixture');insert into players(id,name,slug) values('p','fixture','fixture')");
 const base={player_id:'p',championship_id:'c',competition_id:'league',season:'season'};
 const specs=[['player_history','price',{...base,value:100,points:-2,recorded_at:'2026-01-01T01:02:03.123456Z'},'player_id,championship_id,competition_id,season,observed_at'],['player_match_observations','match',{...base,scoring_version:'score',round:1,source_round_key:'r',points:-3,score_status:'final'},'player_id,championship_id,competition_id,season,scoring_version,round,source_round_key,observed_at'],['rival_finance_evidence','finance',{account_id:'a',championship_id:'c',competition_id:'league',season:'season',evidence_type:'movements',source_id:'x',payload:{amount:-10,missing:null,empty:[],flag:false}},'account_id,championship_id,competition_id,season,evidence_type,source_id,observed_at']];
 const before={};
 for(const [table,prefix,row] of specs){
  for(let i=1;i<=3;i++){
   const r={...row,observed_at:`2026-01-0${i}T00:00:00.123456Z`};
   const keys=Object.keys(r);await q(`insert into ${table}(${keys.join(',')}) select ${keys.join(',')} from jsonb_populate_record(null::${table},$1)`,[r]);
  }
  before[table]=await q(`select to_jsonb(t) as value from ${table} t order by observed_at`);
 }
 // Nullable source identity/time in legacy price records must not collapse.
 await db.exec("insert into player_history(player_id,championship_id,value) values(null,null,0),(null,null,0)");
 before.player_history=await q('select to_jsonb(t) as value from player_history t order by id');
 await db.exec(fs.readFileSync('scripts/migrations/20261118200000_normalize_fact_histories.sql','utf8'));
 for(const [table,prefix,row,conflict] of specs){
  assert.deepEqual(await q(`select to_jsonb(t) as value from ${table} t order by ${prefix==='price'?'id':'observed_at'}`),before[table].filter(x=>!String(x.value.observed_at).startsWith('2026-01-02')));
  assert.equal(Number((await q(`select count(*) n from ${prefix}_history_states`))[0].n),prefix==='price'?2:1);
  await db.exec('set role service_role');
  const persisted=(await q(`select to_jsonb(t) value from ${table} t where observed_at='2026-01-03T00:00:00.123456Z'`))[0].value;
  delete persisted.id;
  const write=async(day,change={})=>q(`select ${prefix}_history_write($1,$2,false)`,[{...persisted,...change,observed_at:`2026-01-${String(day).padStart(2,'0')}T00:00:00.123456Z`},conflict]);
  await write(4);await write(5);await write(5);
  const delta=prefix==='finance'?{payload:{amount:42}}:{points:42};
  await write(6,delta);await write(7,delta);await write(8);await write(9);await write(10);
  assert.deepEqual((await q(`select extract(day from observed_at)::int d from ${table} where observed_at is not null order by observed_at`)).map(x=>x.d),[1,5,6,7,8,10]);
  // Historical cutoff uses an actual earlier endpoint, never the future confirmation.
  assert.equal((await q(`select extract(day from observed_at)::int d from ${table} where observed_at<'2026-01-04' order by observed_at desc limit 1`))[0].d,1);
  // An out-of-order change must remain between the surrounding confirmations.
  await write(9,delta);
  assert.deepEqual((await q(`select extract(day from observed_at)::int d from ${table} where observed_at>='2026-01-08' order by observed_at`)).map(x=>x.d),[8,9,10]);

  const incoming={...row,observed_at:'2026-01-01T00:00:00.123456Z'};
  await q(`select ${prefix}_history_write($1,$2,true)`,[incoming,conflict]);
  const changed={...incoming,...(prefix==='finance'?{payload:{amount:0}}:{points:0})};
  await q(`select ${prefix}_history_write($1,$2,false)`,[changed,conflict]);
  const result=(await q(`select to_jsonb(t) value from ${table} t where observed_at='2026-01-01T00:00:00.123456Z'`))[0].value;
  assert.equal(prefix==='finance'?result.payload.amount:result.points,0);
  await q(`select ${prefix}_history_write($1,$2,false)`,[{...row,observed_at:'2025-12-01T00:00:00Z'},conflict]);
  await assert.rejects(q(`select ${prefix}_history_write($1,$2,false)`,[{...row,token:'bad'},conflict]));
  const countBefore=Number((await q(`select count(*) n from ${prefix}_history_times`))[0].n);
  const statesBefore=Number((await q(`select count(*) n from ${prefix}_history_states`))[0].n);
  const confirmations=Array.from({length:200},(_,i)=>({...persisted,observed_at:new Date(Date.UTC(2026,1,1,0,i)).toISOString()}));
  const started=Date.now();
  await q(`select ${prefix}_history_write($1,$2,false)`,[JSON.stringify(confirmations),conflict]);
  assert.equal(Number((await q(`select count(*) n from ${prefix}_history_times`))[0].n),countBefore+1);
  assert.equal(Number((await q(`select count(*) n from ${prefix}_history_states`))[0].n),statesBefore);
  console.log(`${prefix}: 200 identical confirmations, one additional endpoint, ${Date.now()-started}ms isolated fixture`);
  // A different competition has independent endpoints despite identical facts.
  await q(`select ${prefix}_history_write($1,$2,false)`,[{...persisted,competition_id:'another',observed_at:'2026-02-01T00:00:00Z'},conflict]);
  assert.equal(Number((await q(`select count(*) n from ${table} where competition_id='another'`))[0].n),1);
  await db.exec('reset role');
 }
 assert.equal((await q('select count(*) n from player_round_points_latest'))[0].n,2);
 assert.equal((await q('select payload from rival_finance_evidence_latest'))[0].payload.amount,-10);
 await db.exec('set role anon');for(const [table] of specs)await assert.rejects(q(`select * from ${table}`));
 console.log('PASS first/last endpoints; A/B/A changes; late changes; as-of cutoff; endpoint rows/timestamps/IDs roundtrip; nullable uniqueness; negative/zero values; JSON payload fidelity; duplicate/merge/late writes; latest views and access');
}finally{await db.close();}})().catch(e=>{console.error(e);process.exit(1)});
