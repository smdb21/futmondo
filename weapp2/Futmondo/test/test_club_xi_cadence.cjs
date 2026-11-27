const assert = require('node:assert/strict');
const fs = require('node:fs');
const { PGlite } = require('@electric-sql/pglite');

(async () => {
  const db = new PGlite();
  try {
    await db.exec('CREATE ROLE anon; CREATE ROLE authenticated; CREATE ROLE service_role BYPASSRLS;');
    await db.exec(fs.readFileSync('scripts/schema.sql', 'utf8'));
    await db.exec(fs.readFileSync('scripts/migrations/20261127044336_club_lineup_cadence.sql', 'utf8'));
    await db.exec('UPDATE worker_schedule_policy SET base_interval=2700 WHERE id=1');
    async function delay(handler, seconds) {
      const {rows} = await db.query(`SELECT extract(epoch FROM
        worker_next_refresh_due($1, 300, now()+make_interval(secs=>$2), now(), now())-now()) AS delay`,
        [handler, seconds]);
      return Number(rows[0].delay);
    }
    assert.equal(await delay('get_club_lineups', 180), 180);
    assert.equal(await delay('get_club_lineups', -20), 60);
    assert.equal(await delay('get_club_lineups', 86400), 2700);
    assert.equal(await delay('get_lineup_match_details', 180), 2700);
    const {rows} = await db.query(`SELECT has_function_privilege('authenticated',
      'worker_next_refresh_due(text,integer,timestamptz,timestamptz,timestamptz)','EXECUTE') AS browser,
      has_function_privilege('service_role',
      'worker_next_refresh_due(text,integer,timestamptz,timestamptz,timestamptz)','EXECUTE') AS worker`);
    assert.deepEqual(rows[0], {browser:false,worker:true});
    console.log('PASS fixture-specific deadlines, one-minute floor, quiet cadence, ordinary sync and restricted privileges');
  } finally {
    await db.close();
  }
})().catch(e=>{console.error(e);process.exitCode=1;});
