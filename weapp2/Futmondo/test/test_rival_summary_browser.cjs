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
      const ownRow=table.locator('.fm-own-team-row');
      await ownRow.waitFor();
      assert.equal(await ownRow.count(),1);
      assert.match(await ownRow.innerText(),/Alpha/);
      const darkOwn=await ownRow.evaluate(n=>getComputedStyle(n).backgroundColor);
      const cellsRevealTint=()=>ownRow.getByRole('cell').evaluateAll(cells=>cells.every(cell=>{
        const color=getComputedStyle(cell).backgroundColor;
        return color==='rgba(0, 0, 0, 0)'||color===getComputedStyle(cell.parentElement).backgroundColor;
      }));
      assert.ok(await cellsRevealTint(),'Cell backgrounds must reveal the own-team row tint');
      assert.notEqual(darkOwn,await table.getByRole('row').filter({hasText:'Beta'}).evaluate(n=>getComputedStyle(n).backgroundColor));
      await page.evaluate(()=>document.documentElement.dataset.fmTheme='light');
      assert.notEqual(await ownRow.evaluate(n=>getComputedStyle(n).backgroundColor),darkOwn);
      assert.ok(await cellsRevealTint(),'Light-theme cells must reveal the own-team row tint');
      await page.evaluate(()=>delete document.documentElement.dataset.fmTheme);
      await table.getByRole('row').filter({hasText:'Beta'}).getByRole('radio').click();
      assert.match(await ownRow.innerText(),/Alpha/,'Scouting another team preserves your team highlight');
      await table.getByRole('row').filter({hasText: 'Alpha'}).getByRole('radio').click();
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
      assert.ok((await wealth.innerText()).includes('25.000.000'));
      assert.ok((await profit.innerText()).includes('10.000.000'));
      await wealth.locator('summary').click();
      await profit.locator('summary').click();
      const wealthText = await wealth.locator('.rivals-calculation-breakdown').innerText();
      const profitText = await profit.locator('.rivals-calculation-breakdown').innerText();
      for (const amount of ['300.000.000', '25.000.000']) assert.ok(wealthText.includes(amount));
      for (const amount of ['40.000.000', '30.000.000', '10.000.000']) assert.ok(profitText.includes(amount));
      assert.equal(/[a-f0-9]{24}/.test(await grid.innerText()), false, 'No raw evidence IDs in summary details');
      assert.ok(/eligib|award|historical/i.test(await wealth.innerText()), 'Incomplete award evidence stays visible');
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
        assert.equal(await ownRow.count(),1);
        assert.match(await ownRow.innerText(),/Alpha/,'Sorting preserves your team highlight');
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
      console.log(`PASS ${width}px: six cards, numeric calculation details, sorted wealth/profit, selection and no page overflow`);
      await page.close();
    }
  } finally { await browser.close(); }
})().catch(error => {console.error(error.stack); process.exitCode = 1;});
