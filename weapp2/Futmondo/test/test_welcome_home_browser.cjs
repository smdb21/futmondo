// Run with test_account_summary_browser_app.R. All accounts and observations are synthetic.
const assert = require('node:assert/strict');
const path = require('node:path');
const { chromium } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES ||
  '/tmp/futmondo-review-tools/node_modules', 'playwright'));
const origin = 'http://127.0.0.1:' + (process.env.FUTMONDO_SUMMARY_TEST_PORT || 3892);

// Inspect actual paint and hit testing. A transparent fixed button can still
// intercept taps, and the opacity transition may belong to its parent.
function floatingControlsMatch(visible) {
  return ['.fm-home-return', '#refresh_all'].every(selector => {
    const el = document.querySelector(selector);
    const rect = el.getBoundingClientRect();
    const painted = rect.width > 0 && rect.height > 0;
    let opacity = painted ? 1 : 0;
    for (let owner = el; owner && opacity > 0; owner = owner.parentElement) {
      const style = getComputedStyle(owner);
      opacity *= style.visibility === 'hidden' ? 0 : Number(style.opacity);
    }
    const hit = painted && el.contains(document.elementFromPoint(
      rect.left + rect.width / 2, rect.top + rect.height / 2));
    return visible ? opacity >= 0.99 && hit : opacity <= 0.01 && !hit;
  });
}

async function verifyFloatingControls(page, width, openPage) {
  const mobile = width < 768;
  const home = page.locator('.fm-home-return');
  const refresh = page.locator('#refresh_all');
  const waitForControls = visible => page.waitForFunction(floatingControlsMatch, visible, {timeout:4000});
  await openPage('yourteam');
  await page.locator('#shiny-tab-yourteam.active').waitFor();
  await page.waitForFunction(() => document.scrollingElement.scrollHeight > innerHeight + 60);
  await waitForControls(!mobile);
  await page.evaluate(() => {
    window.fixtureFloatingRefreshes = 0;
    $(document).on('shiny:inputchanged.floatingFixture', event => {
      if (event.name === 'refresh_all') {
        window.fixtureFloatingRefreshes++;
        window.fixtureRefreshPending = document.getElementById('league-refresh-status').textContent;
      }
    });
  });

  if (mobile) {
    // A stationary tap does not reveal the controls on a resting phone.
    await page.locator('#shiny-tab-yourteam .fm-information-heading h2').tap();
    assert.equal(await page.evaluate(floatingControlsMatch, false), true);
  }
  await page.evaluate(() => window.scrollBy(0, scrollY > 40 ? -32 : 32));
  await waitForControls(true);
  assert.equal(await home.innerText(), 'Home');
  assert.equal(await refresh.innerText(), 'Refresh');
  const a = await home.boundingBox(), b = await refresh.boundingBox();
  assert.ok(a && b && a.height >= 48 && a.width >= 48);
  assert.ok(Math.abs(a.width - b.width) < 1 && Math.abs(a.height - b.height) < 1);
  assert.ok(Math.abs(a.y - b.y) < 1 && a.x + a.width < b.x);
  assert.ok(a.x >= 0 && b.x + b.width <= width && a.y >= 0 &&
    a.y + a.height <= page.viewportSize().height);

  let idleMs;
  if (mobile) {
    // A second scroll keeps both controls tappable past the first three-second deadline.
    await page.waitForTimeout(1200);
    const scrollAt = await page.evaluate(() => {
      window.scrollBy(0, 12);
      return performance.now();
    });
    await page.waitForTimeout(2300);
    assert.equal(await page.evaluate(floatingControlsMatch, true), true);
    await waitForControls(false);
    idleMs = Math.round(await page.evaluate(() => performance.now()) - scrollAt);
    assert.ok(idleMs >= 3000, 'the controls stay available for three seconds after the last scroll');
    const stationary = await page.evaluate(() => scrollY);
    // The player table scrolls inside the page on a phone.
    const table = page.locator('#shiny-tab-yourteam .ReactTable:visible').first();
    await table.waitFor();
    const moved = await table.evaluate(table => {
      const scroller = [table, ...table.querySelectorAll('*')].find(el =>
        el.clientWidth > 40 && el.scrollWidth > el.clientWidth + 10 &&
        /auto|scroll/.test(getComputedStyle(el).overflowX));
      if (!scroller) return false;
      const before = scroller.scrollLeft;
      scroller.scrollLeft = before ? 0 : 10;
      return before !== scroller.scrollLeft;
    });
    assert.ok(moved, 'the nested table really scrolled');
    await waitForControls(true);
    assert.equal(await page.evaluate(() => scrollY), stationary);
    await waitForControls(false);
    if (width === 320) {
      assert.equal(await home.evaluate(el => {
        const style = getComputedStyle(el.closest('[role="group"]'));
        return style.transitionDuration.split(', ').every(d => parseFloat(d) === 0) ||
          !style.transitionProperty.split(', ').some(p => p === 'opacity' || p === 'all');
      }), true, 'the group changes visibility without animating opacity under reduced motion');
    }
  } else {
    await page.waitForTimeout(850);
    assert.equal(await page.evaluate(floatingControlsMatch, true), true);
  }
  assert.equal(await page.evaluate(() => window.fixtureFloatingRefreshes), 0);
  if (mobile) {
    await page.evaluate(() => window.scrollBy(0, -12));
    await waitForControls(true);
    await refresh.tap();
  } else {
    await refresh.click();
  }
  await page.waitForFunction(() => window.fixtureFloatingRefreshes === 1);
  assert.equal(await page.evaluate(() => window.fixtureRefreshPending), 'Requesting page refresh…');
  const status = page.locator('#league-refresh-status[role="status"]');
  await status.getByText('Refresh started. League balances and players are being reloaded.').waitFor();
  const box = await status.boundingBox();
  assert.ok(box && box.x >= 0 && box.x + box.width <= width && box.y >= 0 && box.y + box.height <= a.y);
  if (mobile) {
    await page.waitForTimeout(3200);
    assert.equal(await page.evaluate(floatingControlsMatch, true), true, 'the result outlasts the scroll timer');
    await waitForControls(false);
    // Exercise the floating Home control by touch from scrolled content.
    await page.evaluate(() => window.scrollBy(0, scrollY > 40 ? -24 : 24));
    await waitForControls(true);
    await home.tap();
    await page.locator('.fm-home-overview').waitFor();
    await openPage('yourteam');
    await page.locator('#shiny-tab-yourteam.active').waitFor();
    await waitForControls(false);
  }

  // Place the native tab cursor immediately before Home so this check does
  // not depend on the number of live table/filter controls on the page.
  await home.evaluate(el => {
    const before = document.createElement('button');
    before.id = 'fixture-before-floating';
    before.style.cssText = 'position:fixed;top:0;left:0;width:1px;height:1px';
    el.before(before);
    before.focus({preventScroll:true});
  });
  await page.keyboard.press('Tab');
  assert.equal(await home.evaluate(el => el === document.activeElement), true);
  await waitForControls(true);
  await page.keyboard.press('Tab');
  assert.equal(await refresh.evaluate(el => el === document.activeElement), true);
  await page.keyboard.press('Shift+Tab');
  await page.locator('#fixture-before-floating').evaluate(el => el.remove());
  await page.keyboard.press('Enter');
  await page.locator('.fm-home-overview').waitFor();
  await waitForControls(false);
  assert.equal(await page.evaluate(() => window.fixtureFloatingRefreshes), 1);
  console.log('PASS', width, 'Home and Refresh each', a.width + '×' + a.height + 'px;',
    mobile ? 'idle after '+idleMs+'ms, page and nested scroll, touch/keyboard Home, one Refresh' : 'persistent controls, keyboard Home, one Refresh');
}

async function verify(browser, width, theme) {
  const page = await browser.newPage({ viewport: { width, height: width < 768 ? 740 : 900 },
    colorScheme: theme, hasTouch: width < 768, reducedMotion: width === 320 ? 'reduce' : 'no-preference' });
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
    assert.equal(await page.evaluate(floatingControlsMatch, false), true);
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
    const round = page.locator('#home_round');
    await round.getByText('Round 11', {exact:true}).waitFor();
    await round.locator('.fm-browser-countdown').waitFor();
    assert.match(await round.innerText(), /\bClosed\b/);
    assert.doesNotMatch(await round.innerText(), /saved schedule|may have changed|out of date/i);
    const kickoff = round.locator('.fm-home-kickoff');
    const label = kickoff.getByText('Round 12 kickoff', {exact:true});
    const timestamp = kickoff.getByText(/^\d{2}\/\d{2}\/\d{4} \d{2}:\d{2} \S+$/);
    const clock = kickoff.locator('.fm-browser-countdown');
    await page.waitForFunction(() => /^(?:\d+d )?\d+h \d{2}m \d{2}s$/.test(
      document.querySelector('#home_round .fm-browser-countdown')?.textContent));
    const [left, right, timerLabel, countdown] = await Promise.all(
      [label, timestamp, kickoff.getByText('Time until kickoff', {exact:true}), clock].map(el => el.boundingBox()));
    assert.ok(left && right && timerLabel && countdown);
    assert.ok(Math.abs(timerLabel.y - countdown.y) < 1 && countdown.x >= timerLabel.x + timerLabel.width);
    assert.ok(Math.abs(left.y - right.y) < 1 && right.x >= left.x + left.width);
    assert.ok(countdown.y >= Math.max(left.y + left.height, right.y + right.height));
    const balance = page.locator('#home_balance');
    await balance.getByText('10.000.000 €', {exact:true}).waitFor();
    await balance.getByText('34.000.000 €', {exact:true}).waitFor();
    await balance.locator('.fm-home-squad-value + .fm-team-value-trend').getByText('+40.000 €', {exact:true}).waitFor();
    const squad = page.locator('#home_squad');
    await squad.getByText('17 players', {exact:true}).waitFor();
    assert.equal(await balance.locator('.fm-team-trend-counts').count(), 0);
    const row = label => squad.locator('.fm-home-detail').filter({has:page.getByText(label,{exact:true})});
    await balance.locator('.fm-home-offers').getByText('1 · 200.000 €', {exact:true}).waitFor();
    assert.match(await row('Players value').locator('.fm-team-trend-up').textContent(), /positive trend\s*2/i);
    assert.match(await row('Players value').locator('.fm-team-trend-down').textContent(), /negative trend\s*1/i);
    await row('Transfers').locator('.fm-team-trend-up').filter({hasText:/0\s+incoming/i}).waitFor();
    assert.match(await row('Transfers').innerText(), /0\s+leaving/i);
    assert.doesNotMatch(await overview.innerText(), /last observed/i);
    assert.equal(await overflow(), false);
    await page.screenshot({path:'/tmp/fm-home-' + width + '-' + theme + '.png'});

    const home = page.locator('.fm-home-return');
    const openPage = id => page.locator('#home_destinations [data-fm-page="' + id + '"]').click();
    const returnHome = async () => {
      // The header remains available on short mobile pages, even at rest.
      if (width < 768) await page.locator('#open_welcome').click();
      else await home.click();
    };
    await verifyFloatingControls(page, width, openPage);
    if (width < 768) {
      await page.locator('.sidebar-toggle').click();
      await page.locator('.sidebar-menu a[data-value="lineup"]').click();
      await page.waitForFunction(() => !document.body.classList.contains('sidebar-open'));
      await page.locator('#shiny-tab-lineup.active').waitFor();
      await returnHome();
      await overview.waitFor();
    }
    for (const id of ids) {
      await openPage(id);
      await page.locator('#shiny-tab-' + id + '.active').waitFor();
      await page.evaluate(() => window.scrollTo(0, document.documentElement.scrollHeight));
      await returnHome();
      await overview.waitFor();
      await page.locator('#home_continue [data-fm-page="' + id + '"]').waitFor();
    }
    await openPage('rivals');
    await returnHome();
    await page.locator('#home_continue [data-fm-page="rivals"]').click();
    await page.locator('#shiny-tab-rivals.active').waitFor();
    await returnHome();
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
        const resources = ['get_user_team_info','get_players_from_team','get_finished_rounds','get_market_players','get_championship_pressroom'];
        return {reads:reads.filter(name => resources.includes(name)).length, http, mutations};
      });
    }
    const before = await readCounts();
    await openPage('notifications');
    await page.locator('#shiny-tab-notifications.active').waitFor();
    const started = Date.now();
    await returnHome();
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
      width < 768 ? 'warm header Home return' : 'warm floating Home return',
      returnMs + 'ms, zero repeat overview reads');
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
