// Run against the offline app from Rscript test/test_progressive_browser_app.R.
const assert = require('node:assert/strict');
const path = require('node:path');
const { chromium } = require(path.join(
  process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules', 'playwright'
));

const origin = 'http://127.0.0.1:38766';
const url = `${origin}/?fixture=disconnect#current-page`;

async function connected(page) {
  await page.goto(url);
  await page.waitForFunction(() => window.Shiny?.shinyapp?.$socket?.readyState === 1);
  await page.locator('#login-login_button').waitFor();
}

async function main() {
  const browser = await chromium.launch({
    executablePath: process.env.FUTMONDO_TEST_CHROMIUM || undefined,
    args: ['--no-sandbox']
  });
  try {
    const page = await browser.newPage({ viewport: { width: 360, height: 660 }, reducedMotion: 'reduce' });
    const requests = [];
    await page.route('**/*', route => {
      const request = route.request();
      requests.push(request.url());
      return request.url().startsWith(`${origin}/`) ? route.continue() : route.abort();
    });
    // Match the upstream elements inserted into body by Shiny Server, whose
    // logical socket can stay open through several physical connection retries.
    await page.addInitScript(() => document.addEventListener('DOMContentLoaded', () => {
      if (!new URLSearchParams(location.search).has('fixture')) return;
      document.body.insertAdjacentHTML('beforeend',
        '<div id="ss-connect-dialog" style="display:none"></div><div id="ss-overlay" style="display:none"></div>');
    }));
    await connected(page);
    const dialog = page.getByRole('dialog', { name: 'Connection lost' });
    const refresh = dialog.getByRole('button', { name: 'Refresh page' });
    assert.equal(await dialog.isVisible(), false);
    await page.locator('#login-login_button').focus();
    await page.evaluate(() => {
      const message = document.querySelector('#ss-connect-dialog');
      message.innerHTML = '<label>Reconnect failed. Retrying…</label><a id="ss-reconnect-link" href="#">Try now</a>';
      message.style.display = 'block';
    });
    await page.locator('#ss-reconnect-link').waitFor({ state: 'visible' });
    assert.equal(await dialog.isVisible(), false);

    await page.evaluate(() => {
      document.querySelector('#ss-overlay').style.display = 'block';
      document.querySelector('#ss-connect-dialog').innerHTML =
        '<label>Disconnected from the server.</label><a id="ss-reload-link" href="#">Reload</a>';
    });
    await dialog.waitFor({ state: 'visible' });
    assert.equal(await page.locator('#ss-connect-dialog').isVisible(), false);
    assert.equal(await page.locator('#ss-overlay').isVisible(), false);
    assert.equal(await refresh.evaluate(el => el === document.activeElement), true);
    assert.equal(await refresh.evaluate(el => el.matches(':focus-visible')), true);
    await page.locator('#login-login_button').focus();
    assert.equal(await refresh.evaluate(el => el === document.activeElement), true);
    await page.keyboard.press('Escape');
    assert.equal(await dialog.isVisible(), true);

    const requestCount = requests.length;
    for (const theme of ['dark', 'light']) {
      await page.evaluate(mode => { document.documentElement.dataset.fmTheme = mode; }, theme);
      const bounds = await dialog.boundingBox();
      const artwork = dialog.locator('svg');
      assert.ok(bounds.x >= 16 && bounds.x + bounds.width <= 344, `${theme} dialog fits 360px`);
      assert.ok((await artwork.boundingBox()).height > 100, `${theme} illustration is visible`);
      assert.equal(await artwork.locator('g > rect').count(), 1);
      assert.equal(await artwork.locator('circle[r="76"]').count(), 1);
      assert.equal(await dialog.locator('canvas, [data-fm-loading]').count(), 0);
      assert.equal(await refresh.isVisible(), true);
      assert.equal(await dialog.evaluate(el => getComputedStyle(el).backgroundColor),
        theme === 'dark' ? 'rgb(11, 18, 13)' : 'rgb(255, 255, 255)');
    }
    const sweep = page.locator('#fm-disconnect .fm-radar-sweep');
    assert.equal(await sweep.evaluate(el => el.getAnimations().length), 0, 'reduced motion keeps the radar still');
    assert.equal(await sweep.isVisible(), true, 'the static sweep remains visible');
    await page.emulateMedia({ reducedMotion: 'no-preference' });
    const motion = await dialog.evaluate(el => {
      const sweep = el.querySelector('.fm-radar-sweep');
      const [animation] = sweep.getAnimations();
      const field = el.querySelector('svg > g > rect');
      const bounds = field.getBoundingClientRect().toJSON();
      animation.currentTime = 0;
      const start = sweep.getCTM();
      animation.currentTime = 1000;
      const quarter = sweep.getCTM();
      const center = new DOMPoint(210, 140);
      return {
        duration: animation.effect.getTiming().duration,
        quarterTurn: Math.hypot(quarter.a + start.b, quarter.b - start.a) < 0.01,
        centered: center.matrixTransform(start).matrixTransform(quarter.inverse()).toJSON(),
        fieldStill: JSON.stringify(bounds) === JSON.stringify(field.getBoundingClientRect().toJSON()),
        animated: el.getAnimations({ subtree: true }).length
      };
    });
    assert.equal(motion.duration, 4000);
    assert.equal(motion.quarterTurn, true);
    assert.ok(Math.hypot(motion.centered.x - 210, motion.centered.y - 140) < 0.01, 'sweep pivots at midfield');
    assert.equal(motion.fieldStill, true);
    assert.equal(motion.animated, 1, 'only the radar sweep animates');
    assert.equal(requests.length, requestCount, 'dialog and artwork need no requests');

    await page.evaluate(() => { document.querySelector('#ss-connect-dialog').style.display = 'none'; });
    await dialog.waitFor({ state: 'hidden' });
    assert.equal(await sweep.evaluate(el => el.getAnimations().length), 0, 'recovery stops the sweep');
    assert.equal(await page.locator('#login-login_button').evaluate(el => el === document.activeElement), true);
    await page.close();

    const plain = await browser.newPage();
    let socket;
    await plain.routeWebSocket('**/websocket/', ws => {
      socket = ws;
      ws.connectToServer();
    });
    await connected(plain);
    const plainDialog = plain.getByRole('dialog', { name: 'Connection lost' });
    await socket.close({ code: 1001, reason: 'Offline fixture disconnect' });
    await plainDialog.waitFor({ state: 'visible' });
    assert.equal(await plain.locator('#shiny-disconnected-overlay').isVisible(), false);
    await plain.evaluate(() => Shiny.shinyapp.reconnect());
    await plainDialog.waitFor({ state: 'hidden' });
    await plain.waitForFunction(() => Shiny.shinyapp.$socket?.readyState === 1);
    for (const [theme, ready, pending] of [
      ['dark', 'rgb(124, 255, 155)', 'rgb(244, 201, 93)'],
      ['light', 'rgb(20, 83, 45)', 'rgb(120, 88, 0)']
    ]) {
      await socket.close({ code: 1001, reason: 'Offline fixture disconnect before reload' });
      await plainDialog.waitFor({ state: 'visible' });
      await plain.evaluate(mode => { document.documentElement.dataset.fmTheme = mode; }, theme);
      const [, feedback] = await Promise.all([
        plain.waitForEvent('framenavigated', frame => frame === plain.mainFrame()),
        plainDialog.getByRole('button', { name: 'Refresh page' }).evaluate(button => {
          const before = getComputedStyle(button).backgroundColor;
          button.click();
          return new Promise(resolve => requestAnimationFrame(() => {
            resolve({ before, after: getComputedStyle(button).backgroundColor,
              disabled: button.getAttribute('aria-disabled') });
          }));
        })
      ]);
      assert.deepEqual(feedback, { before: ready, after: pending, disabled: 'true' }, theme);
      assert.equal(plain.url(), url);
      await plain.waitForFunction(() => window.Shiny?.shinyapp?.$socket?.readyState === 1);
      assert.equal(await plainDialog.isVisible(), false);
    }
    console.log('PASS: hosted retry and recovery; mobile radar sweep and reduced motion; focus, socket close, button color and same-URL reload');
  } finally {
    await browser.close();
  }
}

main().catch(error => {
  console.error(error);
  process.exitCode = 1;
});
