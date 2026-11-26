const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const { chromium } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules', 'playwright'));

(async () => {
  const browser = await chromium.launch({headless: true, executablePath: process.env.FUTMONDO_TEST_CHROMIUM || undefined, args: ['--no-sandbox']});
  try {
    const page = await browser.newPage({viewport: {width: 390, height: 800}});
    await page.evaluate(() => {
      window.loginStateHandlers = {};
      window.fixtureDraws = 0;
      const clear = CanvasRenderingContext2D.prototype.clearRect;
      CanvasRenderingContext2D.prototype.clearRect = function (...args) {
        if (this.canvas.closest('#login-welcome_visual')) window.fixtureDraws++;
        return clear.apply(this, args);
      };
      window.Shiny = {addCustomMessageHandler: (name, handler) => { if (handler.length !== 1) throw new Error('Shiny message handlers require one argument'); window.loginStateHandlers[name] = handler; }};
    });
    await page.setContent(fs.readFileSync(process.argv[2], 'utf8'));
    await page.evaluate(() => {
      window.loginSubmissions = 0;
      // Shiny's direct action-button handler receives the original click after capture.
      document.getElementById('login-login_button').addEventListener('click', () => { window.loginSubmissions += 1; });
    });
    const button = page.locator('#login-login_button');
    const progress = page.locator('#login-login_progress');
    const feedback = page.locator('#login-login_feedback');
    const launcher = page.locator('#login-open_login');
    const dialog = page.locator('#login-login_dialog');
    const user = page.locator('#login-user_name');
    const secret = page.locator('#login-password');
    assert.equal(await dialog.isVisible(), false);
    assert.equal(await launcher.isDisabled(), true);
    await feedback.evaluate(el => { el.textContent = 'Previous failed attempt'; });

    assert.equal(await button.isDisabled(), true);
    await page.evaluate(() => window.loginStateHandlers['login-login_state']({busy: false}));
    assert.equal(await button.isDisabled(), true);
    await button.evaluate(el => el.dispatchEvent(new MouseEvent('click', {bubbles: true, cancelable: true})));
    assert.equal(await page.evaluate(() => window.loginSubmissions), 0);
    await page.evaluate(() => window.loginStateHandlers['login-database_state']({ready: true}));
    assert.equal(await button.isEnabled(), true);
    await launcher.click();
    assert.equal(await dialog.isVisible(), true);
    assert.equal(await user.evaluate(el => el === document.activeElement), true);
    await user.fill('person@example.invalid');
    await secret.fill('cancelled-password');
    await secret.press('Escape');
    assert.equal(await dialog.isVisible(), false);
    assert.equal(await secret.inputValue(), '');
    assert.equal(await launcher.evaluate(el => el === document.activeElement), true);
    await launcher.click();
    await secret.fill('offline-password');
    console.log('PASS database gate, keyboard dialog focus, Escape, and password clearing');

    await button.click();
    assert.equal(await button.isDisabled(), true);
    assert.equal(await button.textContent(), 'Logging in…');
    assert.equal(await button.getAttribute('aria-busy'), 'true');
    assert.equal(await progress.isVisible(), true);
    assert.equal(await feedback.isVisible(), false);
    assert.equal(await page.locator('#login-close_login').isDisabled(), true);
    assert.equal(await page.evaluate(() => window.loginSubmissions), 1);
    console.log('PASS login immediately shows loading feedback before server completion');

    // Native disabled clicks, forced click dispatch, and Enter cannot submit twice.
    await button.evaluate(el => { el.click(); el.dispatchEvent(new MouseEvent('click', {bubbles: true, cancelable: true})); });
    await page.locator('#login-password').press('Enter');
    assert.equal(await page.evaluate(() => window.loginSubmissions), 1);
    console.log('PASS pending login suppresses duplicate clicks and Enter');

    await page.evaluate(() => window.loginStateHandlers['login-login_state']({busy: false}));
    assert.equal(await button.isEnabled(), true);
    assert.equal(await button.textContent(), 'Log in');
    assert.equal(await button.getAttribute('aria-busy'), 'false');
    assert.equal(await progress.isVisible(), false);
    assert.equal(await feedback.isVisible(), true);
    assert.equal(await page.locator('#login-close_login').isEnabled(), true);
    console.log('PASS server completion restores form controls and feedback');

    for (const field of ['password', 'user_name']) {
      const previous = await page.evaluate(() => window.loginSubmissions);
      await page.locator(`#login-${field}`).press('Enter');
      assert.equal(await button.isDisabled(), true);
      assert.equal(await progress.isVisible(), true);
      assert.equal(await page.evaluate(() => window.loginSubmissions), previous + 1);
      await page.evaluate(() => window.loginStateHandlers['login-login_state']({busy: false}));
    }
    console.log('PASS Enter in either credential input shares the loading state');

    await page.locator('#login-password').evaluate(el => el.dispatchEvent(new KeyboardEvent('keydown', {key: 'Enter', isComposing: true, bubbles: true, cancelable: true})));
    assert.equal(await button.isEnabled(), true);
    assert.equal(await page.evaluate(() => window.loginSubmissions), 3);
    console.log('PASS composing Enter does not submit credentials');
    await page.evaluate(() => window.loginStateHandlers['login-database_state']({ready: false}));
    await page.evaluate(() => window.loginStateHandlers['login-login_state']({busy: false}));
    assert.equal(await button.isDisabled(), true);
    console.log('PASS failed database readiness cannot be overridden by idle login feedback');
    await page.evaluate(() => window.loginStateHandlers['login-login_admitted']({}));
    assert.equal(await dialog.isVisible(), false);
    const visual = page.locator('#login-welcome_visual');
    const motion = page.locator('#login-toggle_motion');
    await page.waitForFunction(() => window.fixtureDraws > 0);
    await motion.click();
    assert.equal(await visual.getAttribute('data-fm-loading-paused'), '');
    const stopped = await page.evaluate(() => window.fixtureDraws);
    await page.waitForTimeout(160);
    assert.equal(await page.evaluate(() => window.fixtureDraws), stopped);
    await motion.click();
    assert.equal(await visual.getAttribute('data-fm-loading-paused'), null);
    const mobileWidth = (await visual.boundingBox()).width;
    assert.ok(mobileWidth <= 190);
    await page.setViewportSize({width: 1200, height: 850});
    assert.ok((await visual.boundingBox()).width >= mobileWidth * 1.8);
    const cards = page.locator('.fm-welcome-tier');
    assert.equal((await cards.nth(0).boundingBox()).y, (await cards.nth(1).boundingBox()).y);
    await page.emulateMedia({reducedMotion: 'reduce'});
    assert.equal(await motion.isVisible(), false);
    assert.equal(await visual.locator('svg').isVisible(), true);
    await page.setViewportSize({width: 320, height: 640});
    assert.ok((await cards.nth(0).boundingBox()).y < (await cards.nth(1).boundingBox()).y);
    const overflow = await page.evaluate(() => Array.from(document.querySelectorAll('body *'))
      .filter(el => el.getBoundingClientRect().right > innerWidth + 1).map(el => el.tagName + '.' + el.className).slice(0, 10));
    assert.deepEqual(overflow, []);
    console.log('PASS successful admission closes dialog; animation, feature layout, and reduced motion at mobile/desktop widths');
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error.stack); process.exitCode = 1; });
