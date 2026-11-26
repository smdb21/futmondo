// Run with test_account_summary_browser_app.R. All accounts and observations are synthetic.
const assert = require('node:assert/strict');
const path = require('node:path');
const { chromium } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES ||
  '/tmp/futmondo-review-tools/node_modules', 'playwright'));
const origin = 'http://127.0.0.1:' + (process.env.FUTMONDO_SUMMARY_TEST_PORT || 3892);

async function verify(browser, width, theme) {
  const page = await browser.newPage({ viewport: { width, height: width < 768 ? 740 : 900 },
    colorScheme: theme, reducedMotion: width === 320 ? 'reduce' : 'no-preference' });
  try {
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.route('**/*', route => new URL(route.request().url()).origin === origin ?
      route.continue() : route.abort());
    await page.goto(origin);
    const launcher = page.locator('#login-open_login');
    const dialog = page.locator('#login-login_dialog');
    const overview = page.locator('.fm-home-overview');
    await launcher.waitFor();
    await page.locator('.waiter-overlay').waitFor({state:'hidden'});
    await page.waitForFunction(() => !document.querySelector('#login-open_login').disabled);
    assert.equal(await dialog.isVisible(), false);
    assert.equal(await overview.isVisible(), false);
    const art = await page.locator('#login-welcome_visual').boundingBox();
    const button = await launcher.boundingBox();
    assert.ok(button.y >= art.y + art.height && button.y + button.height < page.viewportSize().height);
    assert.ok(width < 768 ? art.width <= 180 : art.width >= 300);
    if (width === 320) assert.equal(await page.locator('#login-welcome_visual canvas').isVisible(), false);
    const tiers = page.locator('.fm-welcome-tier');
    const first = await tiers.nth(0).boundingBox(), second = await tiers.nth(1).boundingBox();
    assert.ok(width < 768 ? second.y >= first.y + first.height : second.x >= first.x + first.width);
    const overflow = () => page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 1);
    assert.equal(await overflow(), false);
    await page.screenshot({path: '/tmp/fm-welcome-' + width + '-' + theme + '.png'});

    await launcher.click();
    assert.equal(await page.locator('#login-user_name').evaluate(el => el === document.activeElement), true);
    await page.locator('#login-password').fill('discarded');
    await page.keyboard.press('Escape');
    await dialog.waitFor({state:'hidden'});
    assert.equal(await page.locator('#login-password').inputValue(), '');
    assert.equal(await launcher.evaluate(el => el === document.activeElement), true);
    await launcher.press('Enter');
    await page.locator('#login-user_name').fill('offline@example.invalid');
    await page.locator('#login-password').fill('fixture');
    await page.locator('#login-password').press('Enter');
    await overview.waitFor();
    assert.equal(await dialog.isVisible(), false);
    assert.equal(await launcher.isVisible(), false);
    await page.locator('#open_notifications').waitFor();
    const destinations = page.locator('#home_destinations [data-fm-page]');
    await destinations.first().waitFor();
    const ids = await destinations.evaluateAll(links => links.map(link => link.dataset.fmPage));
    const pages = ['today','yourteam','lineup','players_in_championship','rivals','classification',
      'round_mvps','plots','notifications'];
    assert.deepEqual(ids.filter(id => id !== 'admin').sort(), pages.slice().sort());
    const positions = await destinations.evaluateAll(links => links.map(el => el.getBoundingClientRect().y));
    assert.equal(positions.filter(y => y === positions[0]).length, width < 768 ? 2 : 3);
    await page.waitForFunction(() => /12/.test(document.querySelector('#home_round').textContent) &&
      /10[.,\s]?000[.,\s]?000|10\s*m/i.test(document.querySelector('#home_balance').textContent) &&
      /17/.test(document.querySelector('#home_squad').textContent));
    assert.equal(await overflow(), false);
    await page.screenshot({path:'/tmp/fm-home-' + width + '-' + theme + '.png'});

    const home = page.locator('.fm-home-return');
    const openPage = id => page.locator('#home_destinations [data-fm-page="' + id + '"]').click();
    if (width < 768) {
      await page.locator('.sidebar-toggle').click();
      await page.locator('.sidebar-menu a[data-value="lineup"]').click();
      await page.waitForFunction(() => !document.body.classList.contains('sidebar-open'));
      await page.locator('#shiny-tab-lineup.active').waitFor();
      await home.click();
      await overview.waitFor();
    }
    for (const id of ids) {
      await openPage(id);
      await page.locator('#shiny-tab-' + id + '.active').waitFor();
      await page.evaluate(() => window.scrollTo(0, document.documentElement.scrollHeight));
      const bounds = await home.boundingBox();
      assert.ok(bounds && bounds.y >= 0 && bounds.y + bounds.height <= page.viewportSize().height + 1);
      await home.click();
      await overview.waitFor();
      await page.locator('#home_continue [data-fm-page="' + id + '"]').waitFor();
    }
    await openPage('rivals');
    await home.click();
    await page.locator('#home_continue [data-fm-page="rivals"]').click();
    await page.locator('#shiny-tab-rivals.active').waitFor();
    await home.click();
    await overview.waitFor();
    await page.evaluate(() => Shiny.addCustomMessageHandler('fixture-stats', function (value) {
      window.homeFixtureStats = value;
    }));
    async function readCounts() {
      await page.evaluate(() => {
        window.homeFixtureStats = null;
        Shiny.setInputValue('fixture_stats', Math.random(), {priority:'event'});
      });
      await page.waitForFunction(() => window.homeFixtureStats !== null);
      return page.evaluate(() => {
        const {reads, http, mutations} = window.homeFixtureStats;
        const resources = ['get_user_team_info','get_players_from_team','get_finished_rounds'];
        return {reads:reads.filter(name => resources.includes(name)).length, http, mutations};
      });
    }
    const before = await readCounts();
    await openPage('notifications');
    await page.locator('#shiny-tab-notifications.active').waitFor();
    const started = Date.now();
    await home.click();
    await overview.waitFor();
    const returnMs = Date.now() - started;
    const after = await readCounts();
    assert.deepEqual(after, before, 'returning to Home reuses the saved overview');
    assert.equal(after.http + after.mutations, 0);
    await page.locator('.fm-home-settings > summary').click();
    await page.locator('#login-logout_button').click();
    await launcher.waitFor();
    assert.equal(await overview.isVisible(), false);
    assert.equal(await home.isVisible(), false);
    assert.equal(await overflow(), false);
    assert.deepEqual(errors, []);
    console.log('PASS', width, theme, 'welcome, dialog, round/balance/squad, all Home routes, Continue and logout;',
      'warm Home return', returnMs + 'ms, zero repeat overview reads');
  } finally { await page.close(); }
}

(async () => {
  const browser = await chromium.launch({headless:true,args:['--no-sandbox'],
    executablePath:process.env.FUTMONDO_TEST_CHROMIUM || undefined});
  try {
    for (const [width, theme] of [[320,'dark'],[390,'light'],[1440,'light'],[1440,'dark']])
      await verify(browser, width, theme);
  } finally { await browser.close(); }
})().catch(error => {console.error(error);process.exitCode = 1;});
