const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const { chromium } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules', 'playwright'));

(async () => {
  const browser = await chromium.launch({headless: true, args: ['--no-sandbox']});
  try {
    const page = await browser.newPage({viewport: {width: 390, height: 800}});
    await page.evaluate(() => {
      window.loginStateHandlers = {};
      window.Shiny = {addCustomMessageHandler: (name, handler) => { window.loginStateHandlers[name] = handler; }};
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
    await page.locator('#login-user_name').fill('person@example.invalid');
    await page.locator('#login-password').fill('offline-password');
    await feedback.evaluate(el => { el.textContent = 'Previous failed attempt'; });

    await button.click();
    assert.equal(await button.isDisabled(), true);
    assert.equal(await button.textContent(), 'Logging in…');
    assert.equal(await button.getAttribute('aria-busy'), 'true');
    assert.equal(await progress.isVisible(), true);
    assert.equal(await feedback.isVisible(), false);
    assert.equal(await page.locator('#login-logout_button').isDisabled(), true);
    assert.equal(await page.evaluate(() => window.loginSubmissions), 1);
    console.log('PASS login immediately shows loading feedback before server completion');

    // Native disabled clicks, forced click dispatch, and Enter cannot submit twice.
    await button.evaluate(el => { el.click(); el.dispatchEvent(new MouseEvent('click', {bubbles: true, cancelable: true})); });
    await page.locator('#login-password').press('Enter');
    assert.equal(await page.evaluate(() => window.loginSubmissions), 1);
    console.log('PASS pending login suppresses duplicate clicks and Enter');

    await page.evaluate(() => window.loginStateHandlers['login-login_state']({busy: false}));
    assert.equal(await button.isEnabled(), true);
    assert.equal(await button.textContent(), 'Login');
    assert.equal(await button.getAttribute('aria-busy'), 'false');
    assert.equal(await progress.isVisible(), false);
    assert.equal(await feedback.isVisible(), true);
    assert.equal(await page.locator('#login-logout_button').isEnabled(), true);
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
    console.log('LOGIN BROWSER FEEDBACK: 5 passed / 0 failed');
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error.stack); process.exitCode = 1; });
