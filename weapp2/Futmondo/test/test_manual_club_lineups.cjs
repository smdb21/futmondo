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
    const request=async(match='match')=>(await q("select worker_request_club_lineup('acct',$1,$2,'round',(now()+interval '20 minutes')::text) as status",[d,match])).status;
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
    assert.equal((await q("select has_function_privilege('authenticated','worker_request_club_lineup(text,jsonb,text,text,text)','EXECUTE') as browser")).browser,false);
    console.log('PASS existing jobs parked, refresh inert, explicit wake, generation and lease guard, single consumption, duplicate click, expiry and restricted role');
  }finally{await db.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
