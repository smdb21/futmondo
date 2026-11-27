const assert = require('node:assert/strict');
const { chromium } = require((process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules') + '/playwright');

(async () => {
  const browser = await chromium.launch({ headless: true, args: ['--no-sandbox'] });
  try {
    for (const width of [390, 1440]) {
      const page = await browser.newPage({ viewport: { width, height: 950 } });
      const errors = [];
      page.on('pageerror', error => errors.push(error.message));
      await page.route('**/*', route => new URL(route.request().url()).hostname === '127.0.0.1'
        ? route.continue() : route.fulfill({ status: 200, contentType: 'image/svg+xml',
          body: '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16"><circle cx="8" cy="8" r="8" fill="red"/></svg>' }));
      const url = `http://127.0.0.1:${process.env.FUTMONDO_EDITOR_TEST_PORT || '8542'}`;
      await page.context().addCookies([{name:'fixture_browser',value:String(width),url}]);
      const initialHTML = await (await page.request.get(url)).text();
      assert.match(initialHTML, /<button(?=[^>]*id="lineup-save")(?=[^>]*disabled)[^>]*>/,
        'Save is present before Shiny delivers its first output');
      await page.goto(url);
      await page.locator('a[data-value="Lineup"]').click();
      await page.locator('.fm-lineup-pitch .fm-lineup-player').nth(10).waitFor();
      const originalSave = await page.locator('#lineup-save').elementHandle();
      const readCounts = async () => (await page.locator('#fixture_counts').innerText()).trim().split('/').map(Number);
      const counts = await readCounts();
      await page.locator('.fm-lineup-options summary').click();
      assert.ok(await page.getByLabel('Include incoming players').isChecked());
      await page.getByText(/Incoming Keeper.*Eligible before kickoff/).waitFor();
      await page.getByText('Due at or after kickoff', {exact: false}).waitFor();
      await page.getByLabel('Include incoming players').uncheck();
      await page.locator('#lineup-starter_11').click();
      await page.locator('.modal .fm-lineup-choice[data-player-id="p25"]').waitFor();
      assert.equal(await page.locator('.modal .fm-lineup-choice[data-player-id="p26"]').count(), 0);
      await page.getByRole('button', {name:'Cancel', exact:true}).click();
      await page.getByLabel('Include incoming players').check();
      const start = performance.now();
      await page.getByRole('button', {name: 'Optimize'}).click();
      if (process.env.FUTMONDO_EDITOR_OWNED_FIXTURE === 'true') {
        const save = page.getByRole('button', {name: 'Save to LigaRadar'});
        await save.click();
        await page.getByText('Saved to LigaRadar and verified.').waitFor();
        assert.equal(await page.locator('.fm-lineup-player.fm-lineup-incoming').count(), 0);
        assert.equal(await page.locator('#lineup-schedule_status .fm-lineup-submission').count(), 0);
        assert.equal((await readCounts())[1], counts[1] + 1);
        console.log(`PASS ${width}px: all-owned optimized lineup saves immediately`);
        await page.close();
        continue;
      }
      const signing = page.locator('.fm-lineup-pitch .fm-lineup-incoming[data-player-id="p25"]');
      await signing.waitFor();
      const rendered = Math.round(performance.now() - start);
      assert.match(await signing.innerText(), /Incoming ·/);
      await page.getByText(/Incoming Keeper.*Starting XI/).waitFor();
      await page.waitForFunction(() => {
        const incoming = document.querySelector('.fm-lineup-pitch .fm-lineup-incoming img');
        const owned = document.querySelector('.fm-lineup-pitch .fm-lineup-player:not(.fm-lineup-incoming) img');
        return incoming && owned && getComputedStyle(incoming).filter === 'grayscale(1)' &&
          getComputedStyle(owned).filter === 'none';
      });
      const save = page.getByRole('button', {name: 'Save to LigaRadar'});
      assert.ok(await originalSave.evaluate(node => node === document.querySelector('#lineup-save')),
        'Optimizing preserves the visible Save button');
      const schedule = page.locator('#lineup-schedule_status .fm-lineup-submission');
      const coverage = process.env.FUTMONDO_EDITOR_COVERAGE_FIXTURE === 'true';
      const latest = coverage ? page.locator('.fm-lineup-player[data-player-id="p27"]') : signing;
      const openConfirmation = async () => {
        await save.click();
        const due = (await latest.locator('.fm-lineup-incoming-label').innerText()).replace('Incoming · ', '');
        await page.getByText(`Your lineup will be scheduled for ${due}.`, {exact: true}).waitFor();
      };
      const confirm = page.getByRole('button', {name: 'Schedule lineup', exact: true});
      assert.equal(await page.getByLabel('Approve this lineup').count(), 0);
      assert.ok(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth));
      if (process.env.FUTMONDO_EDITOR_DISABLE_SCHEDULE === 'true') {
        await page.getByText('Automatic saves are unavailable on this deployment.').waitFor();
        assert.ok(await save.isDisabled());
        assert.deepEqual(await readCounts(), counts);
        console.log(`PASS ${width}px: pitch with scheduling disabled (${rendered} ms)`);
        await page.close();
        continue;
      }
      const editBench = async () => {
        const bench = page.locator('#lineup-bench_1');
        const player = await bench.getAttribute('data-player-id');
        const expected = await page.locator('#lineup-expected_points').innerText();
        await bench.click();
        await page.getByRole('button', {name: 'Remove player'}).click();
        await page.waitForFunction(() => document.querySelector('#lineup-bench_1').dataset.playerId === '');
        assert.equal(await page.locator('#lineup-expected_points').innerText(), expected);
        await bench.click();
        await page.locator('.modal .fm-lineup-choice[data-player-id="' + player + '"]').click();
        await page.waitForFunction(id => document.querySelector('#lineup-bench_1').dataset.playerId === id, player);
      };
      await editBench();
      await openConfirmation();
      await page.getByRole('button', {name: 'Cancel', exact: true}).click();
      assert.equal(await schedule.count(), 0);
      await signing.waitFor();
      await openConfirmation();
      await confirm.click();
      await page.getByText('Scheduled lineup · queued').waitFor();
      const due = (await latest.locator('.fm-lineup-incoming-label').innerText()).replace('Incoming · ', '');
      const queuedMessage = `Full lineup scheduled for ${due}.`;
      assert.equal(await schedule.count(), 1);
      assert.equal(await schedule.innerText(), queuedMessage);
      await editBench();
      if (coverage) {
        await page.getByLabel('Require full outfield bench coverage').check();
        await page.getByRole('button', {name: 'Optimize'}).click();
        await openConfirmation();
        await confirm.click();
        await page.getByRole('button', {name: 'Keep schedule'}).click();
        await openConfirmation();
        await confirm.click();
        await page.getByRole('button', {name: 'Cancel schedule and replace'}).click();
        await page.getByText('Lineup scheduled.', {exact: true}).waitFor();
        assert.equal(await schedule.innerText(), queuedMessage);
        assert.deepEqual(await readCounts(), counts);
        assert.deepEqual(errors, []);
        console.log(`PASS ${width}px: dated confirmation and schedule replacement`);
        await page.close();
        continue;
      }
      await page.reload();
      await page.locator('a[data-value="Lineup"]').click();
      assert.equal(await page.getByRole('button', {name: 'Edit scheduled lineup'}).count(), 0);
      await signing.waitFor();
      await page.locator('.fm-lineup-options summary').click();
      await page.getByLabel('Include incoming players').uncheck();
      const total = page.locator('#lineup-expected_points');
      const incomingTotal = Number((await total.innerText()).split(' · ')[1]);
      await signing.click();
      await page.locator('.modal .fm-lineup-choice[data-player-id="p2"]').click();
      await page.locator('.fm-lineup-pitch [data-player-id="p2"]').waitFor();
      const editedTotal = Number((await total.innerText()).split(' · ')[1]);
      assert.ok(Math.abs(incomingTotal - editedTotal - (250 - 100/3)) < 0.02, `Totals before and after the keeper swap: ${incomingTotal}, ${editedTotal}`);
      const captain = page.getByLabel('Captain', {exact:true});
      if (!(await page.locator('#lineup-captain').inputValue())) {
        await captain.click();
        await page.locator('.selectize-dropdown .option[data-value="p2"]').click();
      }
      await page.waitForFunction(() => !document.querySelector('.fm-lineup-player.fm-lineup-incoming') && !document.querySelector('#lineup-save').disabled);
      await save.click();
      await page.getByRole('button', {name: 'Keep schedule'}).click();
      assert.equal((await readCounts())[1], counts[1]);
      await save.click();
      await page.getByRole('button', {name: 'Cancel schedule and save'}).click();
      await page.getByText('Saved to LigaRadar and verified.').waitFor();
      await page.getByText('Scheduled lineup · cancelled').waitFor();
      assert.equal((await readCounts())[1], counts[1] + 1);
      assert.deepEqual(errors, []);
      console.log(`PASS ${width}px: preview, grayscale, dated approval and confirmed manual cancellation (${rendered} ms to preview)`);
      await page.close();
    }
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
