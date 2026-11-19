// Start Rscript test/test_lineup_browser.R from the application directory first.
const assert = require('node:assert/strict');
const path = require('node:path');
const { assertLineupLayout } = require('./lineup_layout_helpers.cjs');
const { chromium } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules', 'playwright'));

(async () => {
  const browser = await chromium.launch({ headless: true, args: ['--no-sandbox'] });
  let previousReads = 0;
  try {
    for (const width of [320, 390, 1440]) {
      const page = await browser.newPage({ viewport: { width, height: 1000 } });
      const errors = [];
      page.on('pageerror', error => errors.push(error.message));
      await page.route('https://static02.mondocore.com/**/fixture-*.png', route => route.fulfill({
        contentType: 'image/svg+xml', body: '<svg xmlns="http://www.w3.org/2000/svg" width="22" height="22"><circle cx="11" cy="11" r="10" fill="orange"/></svg>'
      }));
      await page.route('**/*', route => new URL(route.request().url()).hostname === '127.0.0.1' ? route.continue() : /\/fixture-(club|home|away)\.png$/.test(route.request().url()) ? route.fallback() : route.abort());
      await page.goto(`http://127.0.0.1:${process.env.FUTMONDO_EDITOR_TEST_PORT || '8542'}`);
      const pitch = page.locator('.fm-lineup-pitch');
      const modal = page.locator('.modal');
      const assertControlsLayout = async () => {
        const boxes = await page.evaluate(() => {
          const rect = selector => { const r = document.querySelector(selector).getBoundingClientRect(); return {top:r.top,bottom:r.bottom,left:r.left,right:r.right}; };
          return {formation:rect('.fm-lineup-formation'), pitch:rect('.fm-lineup-pitch'),
            save:rect('#lineup-save'), fixtures:rect('.fm-lineup-fixtures'),
            optimize:rect('#lineup-best'), rounds:rect('#lineup-recent_n')};
        });
        assert.ok(boxes.formation.bottom <= boxes.pitch.top && boxes.pitch.top - boxes.formation.bottom < 30, 'Formation sits directly above pitch');
        assert.ok(boxes.save.bottom <= boxes.formation.top, 'Save sits above the formation and pitch');
        assert.ok(boxes.fixtures.top >= boxes.pitch.bottom, 'Real matches sit below the pitch');
        assert.ok(Math.abs(boxes.optimize.bottom - boxes.rounds.bottom) < 3, 'Rounds and Optimize share one row');
        assert.ok(boxes.optimize.right <= boxes.rounds.left, 'Rounds sit next to Optimize');
        assert.ok(boxes.rounds.right <= width && boxes.optimize.left >= 0, 'Controls fit viewport');
      };
      const assertMembership = async () => {
        // Reopening a renderUI modal may briefly contain its previous payload.
        await page.waitForFunction(() => {
          const expected = Object.fromEntries(Array.from(document.querySelectorAll('.fm-lineup-player'))
            .filter(node => node.dataset.playerId).map(node => [node.dataset.playerId, node.id.includes('-starter_') ? 'starter' : 'bench']));
          const rows = Array.from(document.querySelectorAll('.modal .fm-lineup-choice'));
          return rows.length > 0 && rows.every(node => node.dataset.lineupMembership === (expected[node.dataset.playerId] || 'none'));
        });
        const expected = await page.locator('.fm-lineup-player').evaluateAll(nodes => Object.fromEntries(nodes
          .filter(node => node.dataset.playerId).map(node => [node.dataset.playerId,
            node.id.includes('-starter_') ? 'starter' : 'bench'])));
        const rows = await modal.locator('.fm-lineup-choice').evaluateAll(nodes => nodes.map(node => ({
          id: node.dataset.playerId, membership: node.dataset.lineupMembership,
          label: node.querySelector('.fm-lineup-choice-membership')?.getAttribute('aria-label'),
          current: node.getAttribute('aria-pressed') === 'true',
          icon: node.querySelector('.fm-lineup-choice-membership i')?.className,
          visibleText: node.innerText,
          average: Number(node.querySelector('.fm-lineup-choice-average').textContent),
        })));
        const priorities = { bench: 0, none: 1, starter: 2 };
        for (let i = 1; i < rows.length; i++) {
          const previous = rows[i - 1], current = rows[i];
          assert.ok(priorities[previous.membership] <= priorities[current.membership], 'Bench precedes unused squad, then starters');
          if (previous.membership === current.membership && Number.isFinite(previous.average) && Number.isFinite(current.average)) {
            assert.ok(previous.average >= current.average, 'Average points descend within each membership group');
          }
        }
        for (const row of rows) {
          assert.equal(row.membership, expected[row.id] || 'none', `Current draft membership for ${row.id}`);
          if (row.membership === 'none') assert.equal(row.label, undefined);
          else {
            assert.match(row.label, row.membership === 'starter' ? /Starting XI$/ : /Bench [1-4]$/);
            assert.ok(row.icon.includes(row.current ? 'fa-crosshairs' : row.membership === 'starter' ? 'fa-circle-check' : 'fa-chair'));
          }
          assert.doesNotMatch(row.visibleText, /Current slot|Starting XI|Bench [1-4]|Not selected/);
        }
        assert.match(await modal.locator('.fm-lineup-picker-membership-note').innerText(), /including unsaved changes/);
      };
      const closePicker = async () => {
        await modal.locator('button[data-dismiss="modal"]').click();
        await modal.waitFor({ state: 'detached' });
      };
      await pitch.locator('.fm-lineup-player').nth(10).waitFor();
      await page.locator('.fm-lineup-fixture').nth(9).waitFor();
      assert.equal(await page.locator('#lineup-save').count(), 1);
      for (const id of ['p7', 'p17']) {
        const status = page.locator(`.fm-lineup-player[data-player-id="${id}"] .fm-lineup-portrait > .fm-lineup-status`);
        assert.equal(await status.getAttribute('title'), 'Doubtful');
        assert.equal(await status.getAttribute('aria-label'), 'Doubtful');
        const box = await status.boundingBox();
        const portrait = await status.locator('..').boundingBox();
        assert.ok(box.width >= 14 && box.x >= portrait.x && box.x + box.width <= portrait.x + portrait.width + 1, 'Status stays beside the portrait within the slot');
      }
      const coverageOption = page.locator('#lineup-require_bench_coverage');
      assert.equal(await coverageOption.isChecked(), false);
      assert.equal(await coverageOption.isEnabled(), true);
      const coverageLabel = page.locator('label').filter({ has: coverageOption });
      assert.match(await coverageLabel.locator('span[title]').getAttribute('title'), /defender, midfielder and forward.*Goalkeeper cover is not required/);
      const coverageBox = await coverageLabel.boundingBox();
      assert.ok(coverageBox.x >= 0 && coverageBox.x + coverageBox.width <= width, 'Coverage checkbox fits the viewport');
      assert.match(await page.locator('.fm-lineup-fixtures h3').innerText(), /Round 6/);
      assert.match(await page.locator('.fm-lineup-fixtures').innerText(), /Kickoff to be confirmed/);
      assert.match(await page.locator('.fm-lineup-fixtures').innerText(), /schedule may be incomplete/);
      assert.equal(await page.locator('.fm-lineup-fixture').first().locator('[aria-label^="Home:"]').innerText(), 'Home Club 1');
      assert.equal(await page.locator('.fm-lineup-fixture').first().locator('[aria-label^="Away:"]').innerText(), 'A very long visiting football club name');
      assert.ok(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), 'Fixture cards do not overflow');
      const fixtureCrests = page.locator('.fm-lineup-fixture-crest');
      assert.equal(await fixtureCrests.count(), 20);
      for (const crest of await fixtureCrests.all()) {
        await crest.scrollIntoViewIfNeeded();
        await crest.evaluate(el => el.decode());
        const size = await crest.boundingBox();
        assert.equal(size.width, 24); assert.equal(size.height, 24);
        assert.equal(await crest.getAttribute('loading'), 'lazy');
        assert.equal(await crest.getAttribute('decoding'), 'async');
        assert.equal(await crest.getAttribute('aria-hidden'), 'true');
        assert.equal(await crest.getAttribute('alt'), '');
      }
      const crestSources = await fixtureCrests.evaluateAll(nodes => nodes.map(n => n.src));
      assert.ok(crestSources.every((src, i) => src.endsWith(i % 2 ? '/fixture-away.png' : '/fixture-home.png')));
      assert.ok(await page.locator('.fm-lineup-fixture-team').evaluateAll(nodes => nodes.every(n => {
        const crest=n.querySelector('img').getBoundingClientRect(), name=n.querySelector('.fm-lineup-fixture-team-name').getBoundingClientRect();
        return crest.right <= name.left + 1 && name.right <= innerWidth + 1;
      })), 'Crests sit beside club names without overflow');
      await page.screenshot({ path: `/tmp/futmondo-lineup-fixtures-${width}.png`, fullPage: true });
      await assertControlsLayout();
      await assertLineupLayout(page, page.locator('.fm-lineup-layout'));
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
        coverage: document.querySelector('#lineup-require_bench_coverage').checked,
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
      await page.locator('#lineup-recent_n').fill('5');
      await page.locator('#lineup-recent_n').blur();
      await page.evaluate(() => {
        window.lineupProgress = [];
        new MutationObserver(() => {
          const status = document.querySelector('#lineup-status [role="status"]');
          if (status) window.lineupProgress.push({ busy: status.getAttribute('aria-busy'), text: status.textContent });
        }).observe(document.querySelector('#lineup-status'), { childList: true, subtree: true, attributes: true });
        // Click as soon as the optimized player enters the DOM, before dynamic
        // Shiny input bindings can attach. The old actionButton lost this click.
        const watcher = new MutationObserver(() => {
          const button = document.querySelector('#lineup-starter_1:not(.fm-lineup-empty)');
          if (button) {
            window.lineupClickedBeforeBinding = !button.classList.contains('shiny-bound-input');
            button.click();
            watcher.disconnect();
          }
        });
        watcher.observe(document.querySelector('#lineup-pitch'), { childList: true, subtree: true });
      });
      await page.locator('#lineup-best').click();
      await page.waitForFunction(() => document.querySelectorAll('.fm-lineup-pitch .fm-lineup-empty').length === 0);
      assert.ok(await page.evaluate(() => window.lineupProgress.some(s => s.busy === 'true' && s.text.includes('Calculating Best XI'))));
      assert.equal(await page.locator('#lineup-best').innerText(), 'Optimize');
      assert.equal(await page.locator('.fm-lineup-bench-row .fm-lineup-empty').count(), 0);
      const bench = await page.locator('.fm-lineup-bench-row .fm-lineup-points').allTextContents();
      assert.deepEqual(bench.map(Number), bench.map(Number).sort((a, b) => b - a));
      assert.equal(await page.locator('.fm-lineup-bench-row').innerText().then(text => text.includes('GK')), false);
      for (const role of ['DEF', 'MID', 'FWD']) await page.locator('.fm-lineup-coverage').getByText(`${role} ✓`, { exact: true }).waitFor();
      assert.equal(await page.locator('#lineup-save').isDisabled(), false);
      await modal.locator('.fm-lineup-choice').first().waitFor();
      assert.equal(await page.evaluate(() => window.lineupClickedBeforeBinding), true);
      await assertMembership();
      await closePicker();

      const selections = async () => page.locator('.fm-lineup-player').evaluateAll(nodes => nodes.map(n => n.title));
      const fiveRoundSelections = await selections();
      assert.equal(fiveRoundSelections[10], 'Change Player 1');
      assert.equal(await pitch.locator('.fm-lineup-points').nth(10).innerText(), '60');
      await page.locator('#lineup-recent_n').fill('2');
      await page.locator('#lineup-recent_n').blur();
      await page.locator('.fm-lineup-best-warning').waitFor();
      assert.match(await page.locator('.fm-lineup-best-warning').textContent(), /Optimization inputs changed: last N rounds/);
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

      const beforeCoverage = await selections();
      await coverageOption.check();
      await page.locator('.fm-lineup-best-warning').waitFor();
      assert.match(await page.locator('.fm-lineup-best-warning').textContent(), /bench coverage setting/);
      assert.deepEqual(await selections(), beforeCoverage);
      await page.locator('#lineup-best').click();
      await page.locator('.fm-lineup-best-warning').waitFor({ state: 'detached' });
      for (const role of ['DEF', 'MID', 'FWD']) await page.locator('.fm-lineup-coverage').getByText(`${role} ✓`, { exact: true }).waitFor();
      assert.deepEqual((await page.locator('#fixture_counts').innerText()).split('/').map(Number), clearCounts);

      // The picker shows live metrics and applies one row click immediately.
      const starter = pitch.locator('.fm-lineup-player').first();
      const pickerStarted = performance.now();
      await starter.click();
      await modal.locator('.fm-lineup-choice').first().waitFor();
      const pickerMs = performance.now() - pickerStarted;
      const choices = modal.locator('.fm-lineup-choice');
      await assertMembership();
      assert.equal(await choices.count(), 8);
      assert.ok((await choices.evaluateAll(nodes => nodes.map(n => n.dataset.position))).every(role => role === 'FWD'));
      const player17 = modal.locator('.fm-lineup-choice[data-player-id="p17"]');
      assert.match(await player17.locator('.fm-lineup-choice-name').innerText(), /^P\. 17\nFWD$/);
      assert.equal(await player17.getAttribute('title'), 'Player 17');
      assert.equal(await player17.locator('.fm-lineup-status').getAttribute('title'), 'Doubtful');
      const injuredStatus = modal.locator('.fm-lineup-choice[data-player-id="p24"] .fm-lineup-status');
      assert.equal(await injuredStatus.getAttribute('title'), 'Injured');
      assert.equal(await injuredStatus.getAttribute('aria-label'), 'Injured');
      assert.equal(await player17.locator('.fm-lineup-choice-total').innerText(), '117');
      assert.equal(await player17.locator('.fm-lineup-choice-average').innerText(), '17');
      assert.ok((await player17.locator('img').getAttribute('src')).endsWith('/fixture-club.png'));
      for (const box of await choices.evaluateAll(nodes => nodes.map(node => { const r = node.getBoundingClientRect(); return { x: r.x, width: r.width }; }))) {
        assert.ok(box.width > 0 && box.x >= 0 && box.x + box.width <= width, 'Picker rows fit the viewport');
      }
      await page.screenshot({ path: `/tmp/futmondo-lineup-picker-${width}.png`, fullPage: true });
      await player17.click();
      await modal.waitFor({ state: 'detached' });
      await starter.getByText('P. 17', { exact: true }).waitFor();
      // Removing a starter leaves just that position empty until another click.
      await starter.click();
      await modal.locator('.fm-lineup-choice').first().waitFor();
      await assertMembership();
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
      await assertMembership();
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
      await page.getByText('Saved to LigaRadar and verified.', { exact: true }).waitFor();
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
      await benchSlot.click();
      await modal.locator('.fm-lineup-choice').first().waitFor();
      await assertMembership();
      await closePicker();
      // Formation changes redraw the tiles; the same bench slot still opens.
      const savedCounts = (await page.locator('#fixture_counts').innerText()).split('/').map(Number);
      await page.locator('#lineup-formation').selectOption('4-4-2');
      await page.locator('.fm-lineup-pitch[aria-label="Pitch, formation 4-4-2"]').waitFor();
      await benchSlot.click();
      await modal.locator('.fm-lineup-choice').first().waitFor();
      await assertMembership();
      await closePicker();
      await page.locator('#lineup-reset').click();
      await page.waitForFunction(() => document.querySelectorAll('.fm-lineup-pitch .fm-lineup-empty').length === 0);
      await benchSlot.click();
      await modal.locator('.fm-lineup-choice').first().waitFor();
      await assertMembership();
      await closePicker();
      assert.deepEqual((await page.locator('#fixture_counts').innerText()).split('/').map(Number), savedCounts);
      const box = await pitch.boundingBox();
      assert.ok(box.x >= 0 && box.x + box.width <= width + 1, 'Pitch must fit the viewport');
      assert.equal(await page.locator('.shiny-output-error:visible').count(), 0);
      assert.deepEqual(errors, []);
      await page.screenshot({ path: `/tmp/futmondo-lineup-editor-${width}.png`, fullPage: true });
      // Dense midfield columns and formations without forwards must retain all
      // slots through layout-only resizing, without new reads or submissions.
      for (const formation of ['3-6-1', '4-6-0']) {
        await page.locator('#lineup-formation').selectOption(formation);
        await page.locator(`.fm-lineup-pitch[aria-label="Pitch, formation ${formation}"]`).waitFor();
        await assertLineupLayout(page, page.locator('.fm-lineup-layout'));
        await page.screenshot({ path: `/tmp/futmondo-lineup-editor-${formation}-${width}.png`, fullPage: true });
        if (width >= 992 && formation === '3-6-1') {
          const resizeCounts = await page.locator('#fixture_counts').innerText();
          for (const resizeWidth of [991, 992, 390, width]) {
            await page.setViewportSize({ width: resizeWidth, height: 1000 });
            await assertLineupLayout(page, page.locator('.fm-lineup-layout'));
          }
          await page.waitForTimeout(350); // Allow a fixture counter publication after resize.
          assert.equal(await page.locator('#fixture_counts').innerText(), resizeCounts, 'Resizing never reads or saves lineup data');
        }
      }
      assert.deepEqual((await page.locator('#fixture_counts').innerText()).split('/').map(Number), savedCounts);
      previousReads = Number((await page.locator('#fixture_counts').innerText()).split('/')[0]);
      console.log(`PASS ${width}px: save above pitch, ten matches with source crests below and no overflow, clear, defaults, disabled save, exact XI/bench, changed N, coverage, immediate post-optimize click, current draft XI/bench labels, repeated/redrawn one-click picker (${pickerMs.toFixed(0)} ms), starter/bench removal and refill, verified one-shot save, undo latest saved, viewport, dense/no-forward formations and responsive lineup geometry`);
      await page.close();
    }
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error.stack); process.exitCode = 1; });
