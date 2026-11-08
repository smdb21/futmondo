const assert = require('node:assert/strict');
const path = require('node:path');
const { chromium } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules', 'playwright'));
async function openSidebar(page) {
  if (await page.locator('body').evaluate(el => innerWidth < 768 && !el.classList.contains('sidebar-open'))) {
    await page.locator('.sidebar-toggle').click();
  }
  await page.locator('#fm-theme-toggle').scrollIntoViewIfNeeded();
}
async function choose(page, mode) {
  await openSidebar(page);
  await page.locator('#fm-theme-toggle').click();
  await page.locator('#fm-theme-options [data-theme="' + mode + '"]').click();
  assert.equal(await page.locator('#fm-theme-options').isHidden(), true);
}
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
      const toggle = page.locator('#fm-theme-toggle');
      await openSidebar(page);
      await toggle.waitFor();
      async function theme(mode) {
        await page.waitForFunction(mode => document.documentElement.dataset.fmTheme === mode, mode);
        assert.equal(await page.locator('body').evaluate(el => getComputedStyle(el).backgroundColor), mode === 'light' ? 'rgb(243, 247, 243)' : 'rgb(5, 8, 5)');
        assert.equal(await page.locator('html').evaluate(el => getComputedStyle(el).colorScheme), mode);
      }
      await theme('light');
      assert.equal(await toggle.getAttribute('data-theme-choice'), 'system');
      const options = page.locator('#fm-theme-options');
      assert.equal(await options.isHidden(), true);
      assert.equal(await toggle.locator('i').count(), 1);
      await toggle.focus();
      await toggle.press('ArrowDown');
      assert.equal(await options.isVisible(), true);
      assert.equal(await options.locator('[data-theme="system"]').evaluate(el => el === document.activeElement), true);
      await page.keyboard.press('ArrowRight');
      assert.equal(await options.locator('[data-theme="light"]').evaluate(el => el === document.activeElement), true);
      await page.keyboard.press('Escape');
      assert.equal(await options.isHidden(), true);
      assert.equal(await toggle.evaluate(el => el === document.activeElement), true);
      await toggle.click();
      await page.locator('.content-wrapper').dispatchEvent('click');
      assert.equal(await options.isHidden(), true);
      await page.emulateMedia({colorScheme: 'dark'});
      await theme('dark');
      await choose(page, 'light');
      await theme('light');
      await page.emulateMedia({colorScheme: 'light'});
      await page.emulateMedia({colorScheme: 'dark'});
      await theme('light'); // Explicit choice takes precedence over the system.
      await page.reload();
      await openSidebar(page);
      await toggle.waitFor();
      assert.equal(await toggle.getAttribute('data-theme-choice'), 'light');
      await theme('light');
      if (width < 768) await page.locator('.sidebar-toggle').click();
      await page.locator('#login-user_name').fill('offline@example.invalid');
      await page.locator('#login-password').fill('fixture');
      await page.locator('#login-login_button').click();
      // Wait for initial saved-page restoration before navigating.
      await page.locator('#today-market_players-players_table .rt-td').first().waitFor();
      await page.locator('.sidebar-menu a[data-value="admin"]').waitFor({state: 'attached'});
      assert.equal(await page.locator('.main-header #fm-theme-toggle').count(), 0);
      assert.equal(await toggle.evaluate(el => {
        const control = el.closest('.fm-theme-control');
        const menu = control.previousElementSibling;
        const lastItem = Array.from(menu.children).filter(child => child.tagName === 'LI').pop();
        return menu.id === 'menu' && lastItem.querySelector('a[data-value="admin"]') !== null && control.nextElementSibling === null;
      }), true, 'Theme is the final sidebar item directly after the menu ending in Admin');
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
      await choose(page, 'dark');
      await theme('dark');
      await page.waitForFunction(() => document.querySelector('#plots-standings_plot.js-plotly-plot, #plots-standings_plot .js-plotly-plot').layout.paper_bgcolor === '#0B120D');
      assert.equal(await plot.evaluate(el => JSON.stringify(el.data)), before);
      assert.deepEqual(await plot.evaluate(el => el.layout.xaxis.range), [0, 2]);
      await choose(page, 'system');
      await page.emulateMedia({colorScheme: 'light'});
      await theme('light');
      await page.waitForFunction(() => document.querySelector('#plots-standings_plot.js-plotly-plot, #plots-standings_plot .js-plotly-plot').layout.paper_bgcolor === '#FFFFFF');
      // A second tab's saved choice updates this tab too.
      const other = await context.newPage();
      await other.goto('http://127.0.0.1:38766/');
      await choose(other, 'dark');
      await theme('dark');
      assert.equal(await toggle.getAttribute('data-theme-choice'), 'dark');
      await other.close();
      await page.locator('.sidebar-menu a[data-value="yourteam"]').evaluate(el => el.click());
      await choose(page, 'light');
      assert.equal(await table.locator('.rt-td').first().evaluate(el => getComputedStyle(el).backgroundColor), 'rgb(255, 255, 255)');
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 2), false);
      await openSidebar(page);
      const bounds = await toggle.boundingBox();
      assert.ok(bounds.x >= 0 && bounds.x + bounds.width <= width);
      assert.ok(bounds.width <= 40, 'Collapsed theme trigger uses only 40px');
      assert.match(await toggle.locator('i').getAttribute('class'), /sun/);
      await toggle.click();
      assert.equal(await options.locator('button').count(), 3);
      assert.equal(await options.locator('[data-theme="light"]').getAttribute('aria-pressed'), 'true');
      const popup = await options.boundingBox();
      assert.ok(popup.x >= 0 && popup.x + popup.width <= width, 'Popup fits the viewport');
      await page.screenshot({path: `/tmp/futmondo-theme-icons-${width}.png`});
      await toggle.press('Escape');
      await page.screenshot({path: `/tmp/futmondo-light-${width}.png`, fullPage: true});
      assert.deepEqual(errors, []);
      console.log(`PASS ${width}px: system updates, explicit override, reload persistence, cross-tab sync, new/existing plots, table colors, compact icon popup, keyboard/outside dismissal`);
      await context.close();
    }
    // Storage restrictions must not prevent theme switching.
    const page = await browser.newPage({colorScheme: 'dark'});
    await page.addInitScript(() => Object.defineProperty(window, 'localStorage', {get() { throw new Error('Storage blocked'); }}));
    await page.goto('http://127.0.0.1:38766/');
    await choose(page, 'light');
    assert.equal(await page.locator('html').getAttribute('data-fm-theme'), 'light');
    console.log('PASS storage-blocked fallback');
  } finally { await browser.close(); }
})().catch(error => { console.error(error.stack); process.exitCode = 1; });
