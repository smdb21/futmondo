const fs = require('node:fs');
const assert = require('node:assert/strict');
const { PGlite } = require((process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules') + '/@electric-sql/pglite');

(async () => {
  const db = new PGlite();
  const query = async (sql, values = []) => (await db.query(sql, values)).rows;
  const rpc = async (name, ...args) =>
    (await query(`select lineup_schedule_${name}(${args.map((_, i) => '$' + (i + 1)).join(',')}) as result`, args))[0].result;
  const stamp = offset => new Date(Date.now() + offset * 1000).toISOString();
  const context = { account_id: 'a', championship_id: 'l', user_team_id: 't', competition_id: 'c', season: 's', scoring_version: 'v' };
  const lineup = { strategy: '4-4-2', starters: Array.from({ length: 11 }, (_, i) => 'p' + i),
    bench: ['', '', '', ''], captain: '' };
  const stages = [{ player_id: '', due_at: null, lineup },
    { player_id: 'incoming', due_at: stamp(10), lineup }];
  const plan = 'a'.repeat(32), second = 'b'.repeat(32), third = 'c'.repeat(32);
  const admit = (id, lines = stages) => rpc('admit', id, context, 'r', stamp(60), lineup, lines, false, 'synthetic', 'fixture');
  try {
    await db.exec('create role anon; create role authenticated; create role service_role bypassrls;');
    await db.exec(fs.readFileSync('scripts/schema.sql', 'utf8'));
    await db.exec(fs.readFileSync('scripts/migrations/20261123120000_scheduled_lineups.sql', 'utf8'));
    const permissions = await query("select has_function_privilege('anon','lineup_schedule_claim(text)','execute') as anon, has_table_privilege('authenticated','lineup_schedule_plans','select') as browser");
    assert.deepEqual(permissions[0], { anon: false, browser: false });
    await db.exec('set role service_role');
    assert.equal(await admit(plan, [stages[0], { ...stages[1], due_at: stamp(300) }]), false);
    assert.equal(await admit(plan), true);
    assert.equal(await admit(plan), true);
    assert.equal(await admit(second), false);
    assert.equal((await rpc('read', context)).encrypted_token, undefined);
    assert.equal(await rpc('read', { ...context, account_id: 'other' }), null);
    assert.equal(await rpc('claim', 'worker'), null);
    await query("select worker_elect('worker')");
    assert.equal((await rpc('claim', 'worker')).stage_index, 0);
    assert.equal(await rpc('claim', 'worker'), null);
    assert.equal(await rpc('dispatch', plan, 0, 'worker', stamp(-1)), false);
    assert.equal(await rpc('manual_begin', context, 'manual', null), 'confirm:' + plan);
    assert.equal(await rpc('dispatch', plan, 0, 'worker', stamp(15)), true);
    assert.equal(await rpc('dispatch', plan, 0, 'worker', stamp(15)), false);
    assert.equal(await rpc('cancel', context, plan), false);
    assert.equal(await rpc('manual_begin', context, 'manual', plan), 'busy');
    await query("update lineup_schedule_plans set lease_until=now()-interval '1 second' where id=$1", [plan]);
    assert.equal((await rpc('claim', 'worker')).state, 'uncertain');
    assert.equal(await rpc('dispatch', plan, 0, 'worker', stamp(15)), false);
    assert.equal(await rpc('settle', plan, 0, 'worker', 'saved', null), true);
    const acknowledged = await rpc('read', context);
    assert.equal(acknowledged.stage_index, 1);
    assert.ok(acknowledged.outcomes[0].dispatched_at);
    assert.equal(await rpc('claim', 'worker'), null);
    assert.equal(await rpc('manual_begin', context, 'manual', plan), 'ready');
    assert.equal((await rpc('read', context)).state, 'cancelled');
    assert.equal(await admit(second), false);
    await rpc('manual_end', context, 'manual');
    assert.equal(await admit(second), true);
    assert.equal((await rpc('claim', 'worker')).id, second);
    assert.equal(await rpc('settle', second, 0, 'worker', 'unchanged', null), true);
    await query("update lineup_schedule_plans set due_at=now()-interval '1 hour',kickoff=now()-interval '1 second' where id=$1", [second]);
    assert.equal(await rpc('claim', 'worker'), null);
    assert.equal((await rpc('read', context)).state, 'expired');
    assert.equal(await admit(third), true);
    assert.equal((await rpc('claim', 'worker')).id, third);
    assert.equal(await rpc('settle', third, 0, 'worker', 'lineup_changed', null), true);
    assert.equal((await rpc('read', context)).code, 'lineup_changed');
    assert.equal((await query('select count(*)::integer n from lineup_schedule_plans where encrypted_token is not null'))[0].n, 0);
    await db.exec('reset role');
    await db.exec(fs.readFileSync('scripts/migrations/20261123170000_replace_lineup_schedules.sql', 'utf8'));
    await db.exec('set role service_role');
    const future = { ...stages[1], due_at: stamp(35) };
    const replacement = (id, lines, confirmed = null, baseline = lineup) =>
      rpc('admit_plan', id, context, 'r', stamp(60), baseline, lines, false, 'synthetic', 'fixture', confirmed);
    const oldId = 'd'.repeat(32), newId = 'e'.repeat(32);
    assert.equal(await replacement(oldId, stages), 'ready');
    assert.equal(await replacement(newId, [future]), 'confirm:' + oldId);
    assert.equal(await replacement(newId, [future], 'f'.repeat(32)), 'confirm:' + oldId);
    assert.equal(await replacement(newId, [{ ...future, due_at: stamp(120) }], oldId), 'invalid');
    assert.equal((await rpc('read', context)).id, oldId);
    assert.equal((await rpc('claim', 'worker')).id, oldId);
    assert.equal(await rpc('dispatch', oldId, 0, 'worker', stamp(15)), true);
    assert.equal(await replacement(newId, [future], oldId), 'busy');
    assert.equal(await rpc('settle', oldId, 0, 'worker', 'saved', null), true);
    assert.equal(await replacement(newId, [future], oldId, { ...lineup, captain: 'p1' }), 'lineup_changed');
    assert.equal(await replacement(newId, [future], oldId), 'ready');
    assert.equal(await replacement(newId, [future], oldId), 'ready');
    assert.equal((await rpc('read', context)).id, newId);
    assert.equal(await rpc('claim', 'worker'), null);
    const retired = (await query('select state,code,encrypted_token from lineup_schedule_plans where id=$1', [oldId]))[0];
    assert.deepEqual(retired, { state: 'cancelled', code: 'replaced', encrypted_token: null });
    console.log('PASS exact confirmation, dispatch conflict, stale baseline protection and deferred first stage');
    const planLookup = await query("explain (analyze,format json) select id from lineup_schedule_plans where state in ('queued','preparing','dispatched','uncertain') and due_at<=now() order by due_at limit 1");
    console.log('PASS scoped admission, consent, restart recovery, one dispatch, cancellation, expiry and secure grants');
    console.log('Due lookup fixture execution ms:', planLookup[0]['QUERY PLAN'][0]['Execution Time']);
  } finally {
    await db.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
