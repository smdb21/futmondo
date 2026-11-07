const assert = require('node:assert/strict');
const path = require('node:path');
const { chromium } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules', 'playwright'));
(async () => {
  const browser = await chromium.launch({headless: true, args: ['--no-sandbox']});
  try {
    for (const width of [1440, 360]) {
      const context = await browser.newContext({viewport: {width, height: 900}, colorScheme: 'light'});
      const page = await context.newPage();
      page.setDefaultTimeout(15000);
      const errors = [];
      page.on('pageerror', error => errors.push(error.message));
      await page.route('**/*', route => route.request().url().startsWith('http://127.0.0.1:38766/') ? route.continue() : route.abort());
      await page.goto('http://127.0.0.1:38766/');
      const select = page.getByLabel('Color theme');
      await select.waitFor();
      async function theme(mode) {
        await page.waitForFunction(mode => document.documentElement.dataset.fmTheme === mode, mode);
        assert.equal(await page.locator('body').evaluate(el => getComputedStyle(el).backgroundColor), mode === 'light' ? 'rgb(243, 247, 243)' : 'rgb(5, 8, 5)');
        assert.equal(await page.locator('html').evaluate(el => getComputedStyle(el).colorScheme), mode);
      }
      await theme('light');
      assert.equal(await select.inputValue(), 'system');
      await page.emulateMedia({colorScheme: 'dark'});
      await theme('dark');
      await select.selectOption('light');
      await theme('light');
      await page.emulateMedia({colorScheme: 'light'});
      await page.emulateMedia({colorScheme: 'dark'});
      await theme('light'); // Explicit choice takes precedence over the system.
      await page.reload();
      await select.waitFor();
      assert.equal(await select.inputValue(), 'light');
      await theme('light');
      await page.locator('#login-user_name').fill('offline@example.invalid');
      await page.locator('#login-password').fill('fixture');
      await page.locator('#login-login_button').click();
      // Wait for initial saved-page restoration before navigating.
      await page.locator('#today-market_players-players_table .rt-td').first().waitFor();
      await page.locator('.sidebar-menu a[data-value="yourteam"]').evaluate(el => el.click());
      const table = page.locator('#players_in_teams-players_table_in_teams-players_table');
      await table.locator('.rt-td').first().waitFor();
      await page.locator('.sidebar-menu a[data-value="plots"]').evaluate(el => el.click());
      const plot = page.locator('#plots-standings_plot .js-plotly-plot, #plots-standings_plot.js-plotly-plot').first();
      await plot.waitFor();
      await page.waitForFunction(() => Array.from(document.querySelectorAll('#plots-standings_plot.js-plotly-plot, #plots-standings_plot .js-plotly-plot')).some(el => el.layout && el.layout.paper_bgcolor === '#FFFFFF'));
      // Check an existing plot changes without discarding its data or zoom.
      await plot.evaluate(el => window.Plotly.relayout(el, {'xaxis.range': [0, 2], 'xaxis.autorange': false}));
      const before = await plot.evaluate(el => JSON.stringify(el.data));
      await select.selectOption('dark');
      await theme('dark');
      await page.waitForFunction(() => document.querySelector('#plots-standings_plot.js-plotly-plot, #plots-standings_plot .js-plotly-plot').layout.paper_bgcolor === '#0B120D');
      assert.equal(await plot.evaluate(el => JSON.stringify(el.data)), before);
      assert.deepEqual(await plot.evaluate(el => el.layout.xaxis.range), [0, 2]);
      await select.selectOption('system');
      await page.emulateMedia({colorScheme: 'light'});
      await theme('light');
      await page.waitForFunction(() => document.querySelector('#plots-standings_plot.js-plotly-plot, #plots-standings_plot .js-plotly-plot').layout.paper_bgcolor === '#FFFFFF');
      // A second tab's saved choice updates this tab too.
      const other = await context.newPage();
      await other.goto('http://127.0.0.1:38766/');
      await other.getByLabel('Color theme').selectOption('dark');
      await theme('dark');
      assert.equal(await select.inputValue(), 'dark');
      await other.close();
      await page.locator('.sidebar-menu a[data-value="yourteam"]').evaluate(el => el.click());
      await select.selectOption('light');
      assert.equal(await table.locator('.rt-td').first().evaluate(el => getComputedStyle(el).backgroundColor), 'rgb(255, 255, 255)');
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 2), false);
      const bounds = await select.boundingBox();
      assert.ok(bounds.x >= 0 && bounds.x + bounds.width <= width);
      await page.screenshot({path: `/tmp/futmondo-light-${width}.png`, fullPage: true});
      assert.deepEqual(errors, []);
      console.log(`PASS ${width}px: system updates, explicit override, reload persistence, cross-tab sync, new/existing plots, table colors, responsive selector`);
      await context.close();
    }
    // Storage restrictions must not prevent theme switching.
    const page = await browser.newPage({colorScheme: 'dark'});
    await page.addInitScript(() => Object.defineProperty(window, 'localStorage', {get() { throw new Error('Storage blocked'); }}));
    await page.goto('http://127.0.0.1:38766/');
    await page.getByLabel('Color theme').selectOption('light');
    assert.equal(await page.locator('html').getAttribute('data-fm-theme'), 'light');
    console.log('PASS storage-blocked fallback');
  } finally { await browser.close(); }
})().catch(error => { console.error(error.stack); process.exitCode = 1; });
