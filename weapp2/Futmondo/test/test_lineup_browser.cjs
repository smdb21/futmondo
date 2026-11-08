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
      await page.route('https://static02.mondocore.com/**/fixture-club.png', route => route.fulfill({
        contentType: 'image/svg+xml', body: '<svg xmlns="http://www.w3.org/2000/svg" width="22" height="22"><circle cx="11" cy="11" r="10" fill="orange"/></svg>'
      }));
      await page.route('**/*', route => new URL(route.request().url()).hostname === '127.0.0.1' ? route.continue() : route.request().url().endsWith('/fixture-club.png') ? route.fallback() : route.abort());
      await page.goto(`http://127.0.0.1:${process.env.FUTMONDO_EDITOR_TEST_PORT || '8542'}`);
      const pitch = page.locator('.fm-lineup-pitch');
      await pitch.locator('.fm-lineup-player').nth(10).waitFor();
      assert.equal(await pitch.locator('.fm-lineup-empty').count(), 0);
      assert.equal(await page.locator('.fm-lineup-bench-row .fm-lineup-player').count(), 4);
      assert.equal(await page.locator('.fm-lineup-crest').count(), 15);
      assert.equal(await page.locator('.fm-lineup-name > .fm-lineup-crest').count(), 15);
      assert.equal(await page.locator('.fm-lineup-portrait > .fm-lineup-crest').count(), 0);
      assert.ok((await page.locator('.fm-lineup-crest').evaluateAll(nodes => nodes.map(n=>({src:n.src,alt:n.alt}))))
        .every(n=>n.src.endsWith('/teams/64/fixture-club.png') && n.alt==='Fixture Club'));
      assert.equal(await page.locator('.fm-lineup-role').count(), 0);
      assert.equal(await pitch.locator('.fm-lineup-name').first().getAttribute('data-position'), 'FWD');
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
      assert.equal(await page.locator('.fm-lineup-crest').count(), 0);
      assert.equal(await page.locator('#lineup-captain').inputValue(), '');
      assert.equal(await page.locator('#lineup-save').isDisabled(), true);
      assert.deepEqual(await settings(), before);
      await page.waitForFunction(count => Number(document.querySelector('#fixture_counts').textContent.split('/')[0]) === count, initialCounts[0]);
      assert.equal(Number((await page.locator('#fixture_counts').innerText()).split('/')[1]), initialCounts[1]);

      const clearCounts = (await page.locator('#fixture_counts').innerText()).split('/').map(Number);
      await page.evaluate(() => {
        window.lineupProgress = [];
        new MutationObserver(() => {
          const status = document.querySelector('#lineup-status [role="status"]');
          if (status) window.lineupProgress.push({ busy: status.getAttribute('aria-busy'), text: status.textContent });
        }).observe(document.querySelector('#lineup-status'), { childList: true, subtree: true, attributes: true });
      });
      await page.locator('#lineup-best').click();
      await page.waitForFunction(() => document.querySelectorAll('.fm-lineup-pitch .fm-lineup-empty').length === 0);
      assert.ok(await page.evaluate(() => window.lineupProgress.some(s => s.busy === 'true' && s.text.includes('Calculating Best XI'))));
      assert.equal(await page.locator('#lineup-best').innerText(), 'Best XI + bench');
      assert.equal(await page.locator('.fm-lineup-bench-row .fm-lineup-empty').count(), 0);
      const bench = await page.locator('.fm-lineup-bench-row .fm-lineup-points').allTextContents();
      assert.deepEqual(bench.map(Number), bench.map(Number).sort((a, b) => b - a));
      assert.equal(await page.locator('.fm-lineup-bench-row').innerText().then(text => text.includes('GK')), false);
      for (const role of ['DEF', 'MID', 'FWD']) await page.locator('.fm-lineup-coverage').getByText(`${role} ✓`, { exact: true }).waitFor();
      assert.equal(await page.locator('#lineup-save').isDisabled(), false);

      const selections = async () => page.locator('.fm-lineup-player').evaluateAll(nodes => nodes.map(n => n.title));
      const fiveRoundSelections = await selections();
      assert.equal(fiveRoundSelections[10], 'Change Player 1');
      assert.equal(await pitch.locator('.fm-lineup-points').nth(10).innerText(), '60');
      await page.locator('#lineup-recent_n').fill('2');
      await page.locator('#lineup-recent_n').blur();
      await page.locator('.fm-lineup-best-warning').getByText(/current settings \(last 2/).waitFor();
      assert.deepEqual(await selections(), fiveRoundSelections);
      assert.equal(await pitch.locator('.fm-lineup-points').nth(10).innerText(), '0');
      await page.locator('#lineup-recent_n').fill('5');
      await page.locator('#lineup-recent_n').blur();
      await page.locator('.fm-lineup-best-warning').waitFor({ state: 'detached' });
      assert.deepEqual(await selections(), fiveRoundSelections);
      await page.locator('#lineup-recent_n').fill('2');
      await page.locator('#lineup-recent_n').blur();
      await page.locator('.fm-lineup-best-warning').waitFor();
      await page.locator('#lineup-best').click();
      await page.locator('#lineup-status').getByText(/Best XI \+ bench.*rounds 4, 5/).waitFor();
      assert.equal((await selections())[10], 'Change Player 2');
      assert.equal(await page.locator('.fm-lineup-best-warning').count(), 0);
      assert.deepEqual((await page.locator('#fixture_counts').innerText()).split('/').map(Number), clearCounts);

      // The picker shows live metrics and applies one row click immediately.
      const starter = pitch.locator('.fm-lineup-player').first();
      const pickerStarted = performance.now();
      await starter.click();
      const modal = page.locator('.modal');
      await modal.locator('.fm-lineup-choice').first().waitFor();
      const pickerMs = performance.now() - pickerStarted;
      const choices = modal.locator('.fm-lineup-choice');
      assert.equal(await choices.count(), 8);
      assert.ok((await choices.evaluateAll(nodes => nodes.map(n => n.dataset.position))).every(role => role === 'FWD'));
      const player17 = modal.locator('.fm-lineup-choice[data-player-id="p17"]');
      assert.equal(await player17.locator('.fm-lineup-choice-name').innerText(), 'P. 17\nFWD');
      assert.equal(await player17.getAttribute('title'), 'Player 17');
      assert.equal(await player17.locator('.fm-lineup-choice-total').innerText(), '117');
      assert.equal(await player17.locator('.fm-lineup-choice-average').innerText(), '17');
      assert.ok((await player17.locator('img').getAttribute('src')).endsWith('/fixture-club.png'));
      for (const row of await choices.all()) {
        const box = await row.boundingBox();
        assert.ok(box.x >= 0 && box.x + box.width <= width, 'Picker rows fit the viewport');
      }
      await page.screenshot({ path: `/tmp/futmondo-lineup-picker-${width}.png`, fullPage: true });
      await player17.click();
      await modal.waitFor({ state: 'detached' });
      await starter.getByText('P. 17', { exact: true }).waitFor();
      // Removing a starter leaves just that position empty until another click.
      await starter.click();
      await modal.locator('.fm-lineup-remove').click();
      await modal.waitFor({ state: 'detached' });
      await starter.getByText('Empty position', { exact: true }).waitFor();
      assert.equal(await pitch.locator('.fm-lineup-empty').count(), 1);
      assert.equal(await page.locator('#lineup-save').isDisabled(), true);
      await starter.click();
      await modal.locator('.fm-lineup-choice[data-player-id="p17"]').waitFor();
      assert.equal(await modal.locator('.fm-lineup-remove').count(), 0);
      await modal.locator('.fm-lineup-choice[data-player-id="p17"]').click();
      await modal.waitFor({ state: 'detached' });
      await starter.getByText('P. 17', { exact: true }).waitFor();
      const benchSlot = page.locator('.fm-lineup-bench-row .fm-lineup-player').first();
      await benchSlot.click();
      await modal.locator('.fm-lineup-choice[aria-pressed="true"]').waitFor();
      const benchId = await modal.locator('.fm-lineup-choice[aria-pressed="true"]').getAttribute('data-player-id');
      assert.equal(await modal.locator('.fm-lineup-choice').count(), 24);
      await modal.locator('.fm-lineup-remove').click();
      await modal.waitFor({ state: 'detached' });
      await benchSlot.getByText('Empty position', { exact: true }).waitFor();
      await benchSlot.click();
      await modal.locator(`.fm-lineup-choice[data-player-id="${benchId}"]`).click();
      await modal.waitFor({ state: 'detached' });
      assert.deepEqual((await page.locator('#fixture_counts').innerText()).split('/').map(Number), clearCounts);

      await page.locator('#lineup-save').click();
      await page.getByText('Saved to Futmondo and verified.', { exact: true }).waitFor();
      await page.waitForFunction(n => Number(document.querySelector('#fixture_counts').textContent.split('/')[1]) === n + 1, clearCounts[1]);
      assert.equal(await page.locator('#lineup-save').isDisabled(), true);
      await page.locator('#lineup-clear').click();
      await page.waitForFunction(() => document.querySelectorAll('.fm-lineup-pitch .fm-lineup-empty').length === 11);
      assert.equal(await page.locator('#lineup-reset').innerText(), 'Undo changes');
      await page.locator('#lineup-reset').click();
      await page.waitForFunction(() => document.querySelectorAll('.fm-lineup-pitch .fm-lineup-empty').length === 0);
      assert.equal(await page.locator('.fm-lineup-crest').count(), 15);
      for (const crest of await page.locator('.fm-lineup-crest').all()) {
        const size=await crest.boundingBox();
        assert.ok(size && size.width===14 && size.height===14, 'Crests remain visible and compact');
        const label=await crest.locator('..').locator('.fm-lineup-label').boundingBox();
        assert.ok(size.x+size.width<=label.x+1, 'Crest sits beside the player name');
      }
      const box = await pitch.boundingBox();
      assert.ok(box.x >= 0 && box.x + box.width <= width + 1, 'Pitch must fit the viewport');
      assert.equal(await page.locator('.shiny-output-error:visible').count(), 0);
      assert.deepEqual(errors, []);
      await page.screenshot({ path: `/tmp/futmondo-lineup-editor-${width}.png`, fullPage: true });
      previousReads = Number((await page.locator('#fixture_counts').innerText()).split('/')[0]);
      console.log(`PASS ${width}px: clear, defaults, disabled save, exact XI/bench, changed N, coverage, one-click picker (${pickerMs.toFixed(0)} ms), starter/bench removal and refill, verified one-shot save, undo latest saved, viewport`);
      await page.close();
    }
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error.stack); process.exitCode = 1; });
