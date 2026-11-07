// Start Rscript test/test_lineup_browser.R from the application directory first.
const assert = require('node:assert/strict');
const path = require('node:path');
const { chromium } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules', 'playwright'));

(async () => {
  const browser = await chromium.launch({ headless: true, args: ['--no-sandbox'] });
  let previousReads = 0;
  try {
    for (const width of [390, 1440]) {
      const page = await browser.newPage({ viewport: { width, height: 1000 } });
      const errors = [];
      page.on('pageerror', error => errors.push(error.message));
      await page.route('**/*', route => new URL(route.request().url()).hostname === '127.0.0.1' ? route.continue() : route.abort());
      await page.goto(`http://127.0.0.1:${process.env.FUTMONDO_EDITOR_TEST_PORT || '8542'}`);
      const pitch = page.locator('.fm-lineup-pitch');
      await pitch.locator('.fm-lineup-player').nth(10).waitFor();
      assert.equal(await pitch.locator('.fm-lineup-empty').count(), 0);
      assert.equal(await page.locator('.fm-lineup-bench-row .fm-lineup-player').count(), 4);
      await page.waitForFunction(n => Number(document.querySelector('#fixture_counts').textContent.split('/')[0]) > n, previousReads);
      const initialCounts = (await page.locator('#fixture_counts').innerText()).split('/').map(Number);
      assert.equal(await page.locator('#lineup-formation option[value="5-2-3"]').evaluate(node => node.disabled), true);
      const settings = async () => page.evaluate(() => ({
        formation: document.querySelector('#lineup-formation').value,
        n: document.querySelector('#lineup-recent_n').value,
        unavailable: document.querySelector('#lineup-include_unavailable').checked,
      }));
      const before = await settings();
      await page.locator('#lineup-clear').click();
      await page.waitForFunction(() => document.querySelectorAll('.fm-lineup-pitch .fm-lineup-empty').length === 11);
      assert.equal(await page.locator('.fm-lineup-bench-row .fm-lineup-empty').count(), 4);
      assert.equal(await page.locator('#lineup-captain').inputValue(), '');
      assert.equal(await page.locator('#lineup-save').isDisabled(), true);
      assert.deepEqual(await settings(), before);
      await page.waitForFunction(count => Number(document.querySelector('#fixture_counts').textContent.split('/')[0]) === count, initialCounts[0]);
      assert.equal(Number((await page.locator('#fixture_counts').innerText()).split('/')[1]), initialCounts[1]);

      const clearCounts = (await page.locator('#fixture_counts').innerText()).split('/').map(Number);
      await page.locator('#lineup-best').click();
      await page.waitForFunction(() => document.querySelectorAll('.fm-lineup-pitch .fm-lineup-empty').length === 0);
      assert.equal(await page.locator('.fm-lineup-bench-row .fm-lineup-empty').count(), 0);
      const bench = await page.locator('.fm-lineup-bench-row .fm-lineup-points').allTextContents();
      assert.deepEqual(bench.map(Number), bench.map(Number).sort((a, b) => b - a));
      assert.equal(await page.locator('.fm-lineup-bench-row').innerText().then(text => text.includes('GK')), false);
      for (const role of ['DEF', 'MID', 'FWD']) await page.locator('.fm-lineup-coverage').getByText(`${role} ✓`, { exact: true }).waitFor();
      assert.equal(await page.locator('#lineup-save').isDisabled(), false);

      // Slot selection opens locally and filters out players in other roles.
      const starter = pitch.locator('.fm-lineup-player').first();
      const pickerStarted = performance.now();
      await starter.click();
      const modal = page.locator('.modal');
      await modal.getByRole('button', { name: 'Select player', exact: true }).waitFor();
      const pickerMs = performance.now() - pickerStarted;
      const available = await modal.locator('#lineup-player_choice option').allTextContents();
      assert.ok(available.every(text => text === 'Empty position' || text.includes('FWD')));
      await modal.getByRole('button', { name: 'Cancel', exact: true }).click();
      await modal.waitFor({ state: 'detached' });
      assert.deepEqual((await page.locator('#fixture_counts').innerText()).split('/').map(Number), clearCounts);

      await page.locator('#lineup-save').click();
      await page.getByText('Saved to Futmondo and verified.', { exact: true }).waitFor();
      await page.waitForFunction(n => Number(document.querySelector('#fixture_counts').textContent.split('/')[1]) === n + 1, clearCounts[1]);
      assert.equal(await page.locator('#lineup-save').isDisabled(), true);
      await page.locator('#lineup-clear').click();
      await page.waitForFunction(() => document.querySelectorAll('.fm-lineup-pitch .fm-lineup-empty').length === 11);
      await page.locator('#lineup-reset').click();
      await page.waitForFunction(() => document.querySelectorAll('.fm-lineup-pitch .fm-lineup-empty').length === 0);
      const box = await pitch.boundingBox();
      assert.ok(box.x >= 0 && box.x + box.width <= width + 1, 'Pitch must fit the viewport');
      assert.equal(await page.locator('.shiny-output-error:visible').count(), 0);
      assert.deepEqual(errors, []);
      await page.screenshot({ path: `/tmp/futmondo-lineup-editor-${width}.png`, fullPage: true });
      previousReads = Number((await page.locator('#fixture_counts').innerText()).split('/')[0]);
      console.log(`PASS ${width}px: clear, defaults, disabled save, exact XI/bench, coverage, local picker (${pickerMs.toFixed(0)} ms), verified one-shot save, reset, viewport`);
      await page.close();
    }
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error.stack); process.exitCode = 1; });
