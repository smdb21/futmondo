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
      await page.goto(`http://127.0.0.1:${process.env.FUTMONDO_EDITOR_TEST_PORT || '8542'}`);
      await page.locator('.fm-lineup-pitch .fm-lineup-player').nth(10).waitFor();
      const readCounts = async () => (await page.locator('#fixture_counts').innerText()).trim().split('/').map(Number);
      const counts = await readCounts();
      assert.ok(await page.getByLabel('Include incoming players').isChecked());
      await page.getByText(/Incoming Keeper.*Eligible before kickoff/).waitFor();
      await page.getByText('Due at or after kickoff', {exact: false}).waitFor();
      const start = performance.now();
      await page.getByRole('button', {name: 'Optimize'}).click();
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
      if (process.env.FUTMONDO_EDITOR_COVERAGE_FIXTURE === 'true') {
        const stageList = page.locator('#lineup-schedule_preview details');
        const schedule = page.locator('#lineup-schedule_status li');
        const approveStages = async () => {
          for (const stage of await stageList.all()) {
            await stage.locator('summary').click();
            await stage.getByLabel('Approve this lineup').check();
          }
        };
        const scheduleButton = page.getByRole('button', { name: 'Save and schedule approved lineups' });
        assert.equal(await stageList.count(), 3);
        await approveStages();
        await scheduleButton.click();
        await page.getByText('Scheduled lineup preview').waitFor();
        await schedule.nth(2).waitFor();
        await page.getByLabel('Require full outfield bench coverage').check();
        await page.getByRole('button', { name: 'Optimize' }).click();
        await page.getByText(/Full bench coverage first becomes possible after Incoming Forward/).waitFor();
        await signing.waitFor();
        assert.equal(await stageList.count(), 1);
        assert.equal(await schedule.count(), 3);
        await approveStages();
        await scheduleButton.click();
        const keep = page.getByRole('button', { name: 'Keep schedule' });
        await keep.click();
        assert.equal(await schedule.count(), 3);
        await scheduleButton.click();
        await page.getByRole('button', { name: 'Cancel schedule and replace' }).click();
        await page.getByText('Scheduled lineup preview').waitFor();
        await page.waitForFunction(() => document.querySelectorAll('#lineup-schedule_status li').length === 1);
        assert.match(await schedule.first().innerText(), /Incoming Forward/);
        assert.deepEqual(await readCounts(), counts);
        assert.deepEqual(errors, []);
        console.log(`PASS ${width}px: coverage reoptimization defers the first save and replacement requires confirmation`);
        await page.close();
        continue;
      }
      await page.selectOption('#lineup-preview_stage', '1');
      await signing.waitFor({ state: 'detached' });
      const review = page.locator('#lineup-schedule_preview details');
      assert.equal(await review.count(), 2);
      assert.ok(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth));
      if (process.env.FUTMONDO_EDITOR_DISABLE_SCHEDULE === 'true') {
        await page.getByText('Automatic saves are unavailable on this deployment.').waitFor();
        assert.equal(await page.getByRole('button', {name: 'Save and schedule approved lineups'}).count(), 0);
        assert.deepEqual(await readCounts(), counts);
        console.log(`PASS ${width}px: default preview with scheduling disabled (${rendered} ms)`);
        await page.close();
        continue;
      }
      await page.getByRole('button', {name: 'Save and schedule approved lineups'}).click();
      await page.getByText('Approve each lineup before scheduling.').waitFor();
      assert.deepEqual(await readCounts(), counts);
      for (const detail of await review.all()) {
        await detail.locator('summary').click();
        await detail.getByLabel('Approve this lineup').check();
      }
      await page.getByRole('button', {name: 'Save and schedule approved lineups'}).click();
      await page.getByText('Scheduled lineup · queued').waitFor();
      await page.getByLabel('Include incoming players').uncheck();
      await page.getByRole('button', {name: 'Optimize'}).click();
      const save = page.getByRole('button', {name: 'Save to LigaRadar'});
      await page.waitForFunction(() => !document.querySelector('#lineup-save').disabled);
      await save.click();
      await page.getByRole('button', {name: 'Keep schedule'}).click();
      assert.equal((await readCounts())[1], counts[1]);
      await save.click();
      await page.getByRole('button', {name: 'Cancel schedule and save'}).click();
      await page.getByText('Saved to LigaRadar and verified.').waitFor();
      await page.getByText('Scheduled lineup · cancelled').waitFor();
      assert.equal((await readCounts())[1], counts[1] + 1);
      assert.deepEqual(errors, []);
      console.log(`PASS ${width}px: preview, grayscale, stage approvals and confirmed manual cancellation (${rendered} ms to preview)`);
      await page.close();
    }
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
