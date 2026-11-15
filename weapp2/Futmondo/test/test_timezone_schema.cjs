const assert=require('node:assert/strict');
const fs=require('node:fs');
const {PGlite}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/@electric-sql/pglite');
const migration=fs.readFileSync('scripts/migrations/20261115061026_user_timezones.sql','utf8');
(async()=>{
 const db=new PGlite();
 try {
  await db.exec('CREATE ROLE anon; CREATE ROLE authenticated; CREATE ROLE service_role BYPASSRLS;');
  await db.exec(fs.readFileSync('scripts/schema.sql','utf8'));
  await db.exec(migration);
  await db.exec("SET ROLE service_role; INSERT INTO user_page_preferences(user_id,page,visited_at) VALUES('known-user','today',now())");
  let row=(await db.query("SELECT * FROM initialize_user_timezone('a','America/Los_Angeles','Account A')")).rows[0];
  assert.equal(row.timezone,'America/Los_Angeles');
  row=(await db.query("SELECT * FROM initialize_user_timezone('a','Asia/Tokyo','Account A')")).rows[0];
  assert.equal(row.timezone,'America/Los_Angeles');
  await db.query("SELECT * FROM set_user_timezone('a','Europe/Madrid','admin')");
  row=(await db.query("SELECT * FROM initialize_user_timezone('a','Asia/Tokyo','Account A')")).rows[0];
  assert.equal(row.timezone,'Europe/Madrid');assert.equal(row.source,'admin');assert.equal(row.display_name,'Account A');
  await assert.rejects(db.query("SELECT * FROM set_user_timezone('a','Invalid/Timezone','admin')"));
  await assert.rejects(db.query("SELECT * FROM set_user_timezone('a','UTC','browser')"));
  await assert.rejects(db.query("SELECT * FROM set_user_timezone('bad id','UTC','admin')"));
  let users=(await db.query('SELECT * FROM list_user_timezones()')).rows;
  assert.deepEqual(users.map(x=>x.user_id),['a','known-user']);assert.equal(users[1].timezone,null);
  await db.query("SELECT * FROM set_user_timezone('known-user','Asia/Kolkata','admin')");
  assert.equal((await db.query("SELECT * FROM initialize_user_timezone('known-user','UTC','Known')")).rows[0].timezone,'Asia/Kolkata');
  assert.equal((await db.query("SELECT * FROM list_user_timezones('','account a')")).rows.length,1);
  assert.equal((await db.query("SELECT * FROM list_user_timezones('a','')")).rows.length,1);
  await db.exec("INSERT INTO user_page_preferences(user_id,page,visited_at) SELECT 'user-'||lpad(i::text,3,'0'),'today',now() FROM generate_series(1,220)i");
  users=(await db.query('SELECT * FROM list_user_timezones()')).rows;assert.equal(users.length,201);
  assert.equal((await db.query('SELECT * FROM list_user_timezones($1)',[users[199].user_id])).rows.length,22);
  for(const role of ['anon','authenticated']){
   await db.exec('RESET ROLE; SET ROLE '+role);
   await assert.rejects(db.query('SELECT * FROM user_timezone_preferences'));
   await assert.rejects(db.query("SELECT * FROM initialize_user_timezone('x','UTC','')"));
   await assert.rejects(db.query("SELECT * FROM set_user_timezone('a','UTC','admin')"));
   await assert.rejects(db.query('SELECT * FROM list_user_timezones()'));
  }
  console.log('PASS fresh schema + repeatable upgrade, durable initialize/override/reuse, known-account discovery, search/pagination, zone validation and server-only grants');
 } finally {await db.close();}
})().catch(e=>{console.error(e);process.exitCode=1});
