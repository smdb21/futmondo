const assert = require('node:assert/strict');
const path = require('node:path');
const {chromium} = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES ||
  '/tmp/futmondo-review-tools/node_modules', 'playwright'));
const tableId = 'rivals-league_finances_table';
(async () => {
  const browser = await chromium.launch({headless: true, executablePath: process.env.FUTMONDO_TEST_CHROMIUM || undefined, args: ['--no-sandbox']});
  try {
    for (const width of [1600, 1440, 1200, 390, 360]) {
      const page = await browser.newPage({viewport: {width, height: 1100}});
      const errors = [];
      page.on('pageerror', e => errors.push(e.message));
      await page.route('**/*', route => {
        const url = new URL(route.request().url());
        return ['127.0.0.1', 'localhost'].includes(url.hostname) ? route.continue() : route.abort();
      });
      page.setDefaultTimeout(15000);
      await page.goto(process.env.FUTMONDO_TEST_URL || 'http://127.0.0.1:8497');
      const table = page.locator('#' + tableId);
      await page.waitForFunction(() => window.Shiny?.shinyapp?.$socket?.readyState === 1);
      await page.locator('a[data-value="rivals"]').evaluate(link => link.click());
      const loading = page.locator('#rivals-league_finances_status [role=status]');
      await loading.waitFor();
      assert.equal(await loading.innerText(), 'Loading rivals…');
      assert.equal(await table.locator('.rt-tbody').count(), 0);
      await page.evaluate(() => Shiny.setInputValue('rivals-fixture_ready', true));
      const alpha = table.getByRole('row').filter({hasText:'Alpha'});
      const beta = table.getByRole('row').filter({hasText:'Beta'});
      const selected = table.locator('.rt-tr-selected');
      await alpha.waitFor();
      await loading.waitFor({state:'detached'});
      assert.equal(await table.getByRole('radio').count(), 0);
      assert.equal(await table.locator('.fm-own-team-row,.rt-tr-selected').count(), 0);
      async function rowColor(row) {
        const colors = await row.getByRole('cell').evaluateAll(cells =>
          cells.map(cell => getComputedStyle(cell).backgroundColor));
        assert.ok(colors.length > 1 && new Set(colors).size === 1, 'Whole row has one background');
        return colors[0];
      }
      for (const theme of ['dark','light']) {
        await page.evaluate(mode => document.documentElement.dataset.fmTheme = mode, theme);
        await page.mouse.move(0, 0);
        const neutral = await rowColor(beta);
        await beta.getByRole('cell').first().hover();
        const hover = await rowColor(beta);
        assert.notEqual(hover, neutral, 'Hover tints every cell');
        await beta.getByRole('cell').nth(1).click();
        await beta.getByRole('button', {pressed: true}).waitFor();
        await alpha.getByRole('cell').first().hover();
        const chosenColor = await rowColor(beta);
        assert.notEqual(chosenColor, neutral);
        assert.notEqual(chosenColor, hover, 'Persistent selection is distinct from hover');
        assert.equal(await rowColor(alpha), hover);
        assert.equal(await selected.count(), 1);
        await alpha.getByRole('button').press('Enter');
        await alpha.getByRole('button', {pressed: true}).waitFor();
        await page.mouse.move(0, 0);
        assert.equal(await rowColor(beta), neutral, 'Changing selection clears the previous team');
      }
      const grid = page.locator('.rivals-summary-grid');
      await grid.waitFor();
      const cards = grid.locator(':scope > div');
      assert.equal(await cards.count(), 6, 'Six summary cards');
      const boxes = await cards.evaluateAll(nodes => nodes.map(n => {
        const r = n.getBoundingClientRect(); return {x: r.x, y: r.y, width: r.width};
      }));
      if (width >= 1200) {
        assert.equal(new Set(boxes.map(b => Math.round(b.y))).size, 1, 'Six cards occupy one row');
        assert.equal(new Set(boxes.map(b => Math.round(b.x))).size, 6, 'Six desktop columns');
      } else {
        assert.equal(new Set(boxes.map(b => Math.round(b.x))).size, 1, 'Mobile cards stack');
      }
      const wealth = cards.filter({hasText: 'Overall wealth gained'});
      const profit = cards.filter({hasText: 'Trading profit'});
      await page.waitForFunction(() => document.querySelector('#rivals-selected_rival_name')?.textContent === 'Alpha');
      await wealth.getByText(/25\.000\.000/).waitFor();
      assert.ok((await wealth.innerText()).includes('25.000.000'));
      assert.ok((await profit.innerText()).includes('10.000.000'));
      const information = page.locator('.fm-info-panel');
      await information.locator(':scope > summary').click();
      const wealthBreakdown = information.locator('.rivals-calculation-breakdown').filter({hasText:'Cash balance'});
      await wealthBreakdown.getByText(/25\.000\.000/).first().waitFor();
      const wealthText = await wealthBreakdown.innerText();
      const profitText = await information.locator('.rivals-calculation-breakdown').filter({hasText:'Matched purchase'}).innerText();
      for (const amount of ['300.000.000', '25.000.000']) assert.ok(wealthText.includes(amount), wealthText);
      for (const amount of ['40.000.000', '30.000.000', '10.000.000']) assert.ok(profitText.includes(amount), profitText);
      assert.equal(/[a-f0-9]{24}/.test(await information.innerText()), false, 'No raw evidence IDs in summary details');
      assert.ok(/eligib|award|historical/i.test(await information.innerText()), 'Incomplete award evidence stays visible');
      const headers = await table.getByRole('columnheader').allTextContents();
      assert.ok(headers.includes('Overall wealth gained'));
      assert.ok(headers.includes('Trading profit'));
      // Sort the real numeric columns, retaining negative, zero and missing values.
      for (const name of ['Overall wealth gained', 'Trading profit']) {
        await table.getByRole('columnheader', {name: 'Sort ' + name, exact: true}).click();
        await page.waitForFunction(({tableId, name}) => {
          const sorted = Reactable.getState(tableId).sorted;
          return sorted.some(x => x.id === name && x.desc !== true);
        }, {tableId, name});
        const renderedTeams = await table.locator('.rt-tbody .rt-tr-group').evaluateAll(nodes =>
          nodes.map(n => n.textContent).filter(Boolean));
        assert.ok(renderedTeams[0].includes('Beta'), 'Negative value sorts first');
        assert.ok(renderedTeams[1].includes('Gamma'), 'Zero sorts before positive');
        assert.ok(renderedTeams[2].includes('Alpha'), 'Positive sorts after zero');
        assert.ok(renderedTeams[3].includes('Delta'), 'Unavailable sorts last');
        assert.equal(await selected.count(),1);
        assert.match(await selected.innerText(),/Alpha/,'Sorting preserves the clicked team highlight');
      }
      assert.ok((await grid.innerText()).includes('25.000.000'), 'Sorting retains selected Alpha summary');
      assert.equal(await page.locator('.shiny-output-error:visible').count(), 0);
      assert.equal(await grid.evaluate(el => el.scrollWidth > el.clientWidth + 2), false,
        'Financial cards fit their available width');
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 2), false,
        'Page fits viewport with details open');
      assert.deepEqual(errors, []);
      await grid.screenshot({path: `/tmp/futmondo-rival-summary-${width}.png`});
      const longAmountsFit = await cards.evaluateAll(nodes => nodes.slice(1).every(card => {
        const value = card.querySelector('h3'), original = value.textContent;
        value.textContent = '1.981.333.085 €';
        const fits = value.scrollWidth <= value.clientWidth + 2;
        value.textContent = original;
        return fits;
      }));
      assert.ok(longAmountsFit, 'Billion-euro amounts fit within each summary card');
      const history = page.locator('.tab-pane.active[data-value="history"]');
      const historyTotals = history.locator('.rivals-history-totals');
      await historyTotals.locator('.rivals-history-headline').waitFor();
      assert.deepEqual(await historyTotals.locator('dt:visible').allTextContents(),
        ['Balance', 'Total income', 'Total expenses']);
      await historyTotals.locator('summary').click();
      await historyTotals.getByText('Starting cash', {exact: true}).waitFor();
      await historyTotals.getByText('Clause purchases (identified)', {exact: true}).waitFor();
      await history.getByRole('checkbox', {name: /Pivot by Player/}).check();
      await history.getByRole('columnheader', {name: /Related fees/i}).waitFor();
      assert.ok(await historyTotals.locator('details').evaluate(el => el.open));
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 2), false);
      const roster = page.locator('.tab-pane[data-value="roster"]');
      await page.locator('a[data-value="roster"]').click();
      const valuePanel = roster.locator('.fm-team-value-summary');
      await valuePanel.waitFor();
      await page.waitForFunction(() => document.querySelector('.fm-team-value-trend strong')?.textContent.includes('15.000'));
      assert.match(await valuePanel.locator('.fm-team-value-total').innerText(), /25\.000\.000/);
      assert.match(await valuePanel.locator('.fm-team-value-trend strong').innerText(), /\+15\.000/);
      for (const direction of ['up', 'down']) {
        assert.match(await valuePanel.locator('.fm-team-trend-' + direction).innerText(), /1$/);
      }
      await roster.getByText('Player 2', {exact:true}).waitFor();
      assert.equal(await roster.locator('.player-filter-disclosure, .rt-search, .rt-filter').count(), 0);
      assert.ok(await valuePanel.evaluate(el => Boolean(el.compareDocumentPosition(
        document.querySelector('#rivals-rival_squad_table-players_table')) & Node.DOCUMENT_POSITION_FOLLOWING)));
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 2), false);
      await table.getByRole('row').filter({hasText:'Beta'}).getByRole('cell').first().click();
      await page.locator('.tab-pane.active[data-value="history"]').waitFor();
      assert.equal(await page.locator('.shiny-output-error:visible').count(), 0);
      assert.deepEqual(errors, []);
      console.log(`PASS ${width}px: loading, team selection and hover, finance pivot and breakdown, roster trend and mobile layout`);
      await page.close();
    }
  } finally { await browser.close(); }
})().catch(error => {console.error(error.stack); process.exitCode = 1;});
