const assert = require('node:assert/strict');
const fs = require('node:fs');
const {PGlite} = require('@electric-sql/pglite');

(async () => {
  const db = new PGlite();
  try {
    await db.exec('create role anon;create role authenticated;create role service_role bypassrls');
    await db.exec(fs.readFileSync('scripts/schema.sql','utf8'));
    const q = async (sql,args=[]) => (await db.query(sql,args)).rows[0];
    const comp='504e4f584d8bec9a67000079';
    const scope={account_id:'acct',championship_id:'league',competition_id:comp,season:'2026',scoring_version:'stats'};
    const d={handler:'get_club_lineups',resource_key:'a'.repeat(64),context:scope,args:{championship_id:'league',manual_only:true}};
    await q("insert into worker_accounts(account_id,enabled,encrypted_token,credential_id,verified_leagues) values('acct',true,'fixture','fixture',$1)",[{league:{competition_id:comp,season:'2026',scoring_version:'stats'}}]);
    await q("insert into worker_leader(id,owner,lease_until) values(1,'test',now()+interval '10 minutes')");
    await q("select worker_enqueue('acct',$1,300,10)",[d]);
    const parked=async()=> (await q('select due_at=\'infinity\'::timestamptz as parked from worker_jobs where resource_key=$1',[d.resource_key])).parked;
    assert.equal(await parked(),true);
    await q("update worker_jobs set due_at=now() where resource_key=$1",[d.resource_key]);
    assert.equal(await parked(),true);
    const kickoff=new Date(Date.now()+20*60000).toISOString();
    const early=new Date(Date.now()+60*60000).toISOString();
    const request=async(match='match',target=d,when=kickoff)=>(await q(
      "select worker_request_club_lineup('acct',$1,$2,'round',$3) as status",[target,match,when])).status;
    // An account may manage several competitions. Only its actual, verified source ID is admitted.
    const coverage=JSON.parse(fs.readFileSync('www/club-lineup-leagues.json','utf8'));
    const leagues={};
    for (const id of [...Object.keys(coverage),'unmapped']) leagues[id]={competition_id:id,season:'2026',scoring_version:'stats'};
    await q("update worker_accounts set verified_leagues=verified_leagues || $1::jsonb where account_id='acct'",[leagues]);
    for(const [i,id] of Object.keys(leagues).entries()){
      const scoped={...d,resource_key:String(i+1).repeat(64),args:{...d.args,championship_id:id},
        context:{...scope,championship_id:id,competition_id:id}};
      assert.equal(await request('shared-fixture',scoped), coverage[id]?'requested':'unavailable');
      if(id!==comp) assert.equal(await request('spoof', {...scoped,context:{...scoped.context,competition_id:comp}}),'unavailable');
    }
    assert.equal(await request(),'requested');
    assert.equal(await request(),'busy');
    assert.equal(await parked(),false);
    await q("update worker_jobs set owner='test:1',lease_until=now()+interval '1 minute',started_at=now() where resource_key=$1",[d.resource_key]);
    const take=async(owner)=>(await q("select worker_take_club_lineup('acct',$1,$2,(select generation from worker_accounts where account_id='acct')) as req",[d.resource_key,owner])).req;
    assert.equal(await take('wrong'),null);
    assert.equal((await take('test:1')).match_id,'match');
    assert.equal(await take('test:1'),null);
    await q("update worker_jobs set owner=null,due_at=now()+interval '3 minutes' where resource_key=$1",[d.resource_key]);
    assert.equal(await parked(),true);
    assert.equal(await request('another'),'requested');
    assert.equal(await parked(),false);
    await q("update worker_club_lineup_requests set requested_at=now()-interval '4 minutes'");
    await q("update worker_jobs set due_at=now() where resource_key=$1",[d.resource_key]);
    assert.equal(await parked(),true);
    // A different fixture and an alternate resource cannot reset the original game's budget.
    const other={...d,resource_key:'b'.repeat(64)};
    assert.match(await request('match',other),/^retry_at:/);
    await q("update worker_club_lineup_limits set last_check=now()-interval '16 minutes' where match_id='match'");
    assert.equal(await request(),'requested');
    assert.equal(await request('match',other),'busy');
    await q("update worker_jobs set owner='test:1',lease_until=now()+interval '1 minute' where resource_key=$1",[d.resource_key]);
    assert.equal((await take('test:1')).checks,2);
    assert.equal(await request('match',other),'limit_reached');
    assert.equal(await request(),'limit_reached');
    // An expired, unconsumed request never uses a check.
    assert.equal((await q("select checks from worker_club_lineup_limits where match_id='another'")).checks,0);
    assert.equal(await request('early',d,early),'requested');
    assert.equal((await take('test:1')).checks,1);
    await q("update worker_club_lineup_limits set last_check=now()-interval '16 minutes' where match_id='early'");
    const allowed=Number((await request('early',other,early)).split(':')[1]);
    assert.equal(allowed,Math.ceil(Date.parse(early)/1000)-1800);
    assert.equal((await q("select has_table_privilege('authenticated','worker_club_lineup_limits','SELECT') as can_read")).can_read,false);
    assert.equal((await q("select has_function_privilege('authenticated','worker_request_club_lineup(text,jsonb,text,text,text)','EXECUTE') as browser")).browser,false);
    // Model 498 previously spent credits. Only a two-credit endpoint can still be dispatched.
    await q("insert into striker_credit_reservations(endpoint,credits) values('prior_usage',498)");
    const reserve=async(endpoint)=>(await q('select worker_reserve_striker_call($1) as id',[endpoint])).id;
    assert.equal(await reserve('get_lineups'),null);
    const lastTwo=await reserve('list_games');
    assert.ok(lastTwo);
    assert.equal(await reserve('get_squad'),null);
    await q('select worker_settle_striker_call($1,0)',[lastTwo]);
    assert.ok(await reserve('get_squad'));
    await q("update striker_credit_reservations set reserved_at=now()-interval '32 days' where endpoint='prior_usage'");
    assert.ok(await reserve('get_lineups'));
    assert.equal((await q("select has_function_privilege('authenticated','worker_reserve_striker_call(text)','execute') as allowed")).allowed,false);
    const claim=async(league,endpoint,lookup,fetch=true)=>(await q(
      'select worker_claim_striker_response($1,$2,$3,$4) as reply',[league,endpoint,lookup,fetch])).reply;
    const save=async(permit,partial,data)=>(await q(
      'select worker_save_striker_response($1,extract(epoch from now())::float8,$2,$3) as ok',[permit,partial,data])).ok;
    const schedule=await claim('laliga','list_games','2026-11-26');
    assert.equal(schedule.status,'fetch');
    assert.equal((await claim('laliga','list_games','2026-11-26')).status,'busy');
    assert.equal(await save(schedule.reservation,false,{games:[{game_id:'42'}]}),true);
    assert.equal(await save(schedule.reservation,false,{games:[]}),true);
    const before=(await q('select sum(credits) as used from striker_credit_reservations')).used;
    const stored=await claim('laliga','list_games','2026-11-26',false);
    assert.equal(stored.data.games[0].game_id,'42');
    assert.equal(Number(stored.expires)-Number(stored.observed),86400);
    assert.equal((await q('select sum(credits) as used from striker_credit_reservations')).used,before);
    assert.equal((await claim('soccer','list_games','2026-11-26')).status,'fetch');
    const empty=await claim('laliga','get_lineups','42');
    assert.equal(await save(empty.reservation,true,{lineups:[]}),true);
    const pending=await claim('laliga','get_lineups','42');
    assert.equal(Number(pending.expires)-Number(pending.observed),120);
    await q("update striker_public_responses set expires_at=now()-interval '1 second' where endpoint='get_lineups'");
    const fresh=await claim('laliga','get_lineups','42');
    assert.equal(fresh.status,'fetch');
    assert.equal(await save(empty.reservation,true,{lineups:[]}),false);
    assert.equal((await q("select has_table_privilege('authenticated','striker_public_responses','SELECT') as access")).access,false);
    console.log('PASS shared daily schedule, pending expiry, no duplicate reservations and stale-writer protection');
    console.log('PASS two fixture checks, late retry, cross-resource cooldown and global rolling allowance');
    console.log('PASS existing jobs parked, refresh inert, explicit wake, generation and lease guard, single consumption, duplicate click, expiry and restricted role');
  }finally{await db.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
