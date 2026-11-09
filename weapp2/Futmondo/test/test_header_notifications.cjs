// Start Rscript test/test_browser_app.R from the application directory first.
const assert = require('node:assert/strict');
const { chromium } = require((process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules') + '/playwright');
(async () => {
  const browser = await chromium.launch({headless:true,args:['--no-sandbox']});
  try {
    for (const width of [320,390,1440]) {
      const page = await browser.newPage({viewport:{width,height:900}});
      const errors = [];
      page.on('pageerror', error => errors.push(error.message));
      await page.route('**/*', route => route.request().url().startsWith('http://127.0.0.1:38765/') ? route.continue() : route.abort());
      await page.goto('http://127.0.0.1:38765/');
      const bell = page.locator('#open_notifications');
      await bell.waitFor();
      assert.equal(await bell.innerText(), '');
      assert.equal(await bell.locator('.fa-bell').count(), 1);
      assert.equal(await bell.getAttribute('aria-label'), 'Notifications');
      const brand = await page.locator('.fm-header-title').boundingBox();
      const mark = await bell.boundingBox();
      assert.ok(brand && mark && mark.x >= brand.x + brand.width, 'Bell follows Futmondo');
      assert.ok(Math.abs((brand.y + brand.height/2) - (mark.y + mark.height/2)) < 2, 'Bell and title share one line');
      assert.ok(mark.x >= 0 && mark.x + mark.width <= width, 'Header bell fits viewport');
      await page.locator('#login-user_name').fill('offline@example.invalid');
      await page.locator('#login-password').fill('fixture');
      await page.locator('#login-login_button').click();
      await page.waitForFunction(() => document.querySelector('#open_notifications')?.getAttribute('aria-label') === 'Notifications (1 unread)', null, {timeout:15000});
      assert.equal(await bell.locator('.fm-notification-unread').count(), 1);
      assert.equal(await bell.getAttribute('title'), 'Notifications (1 unread)');
      await bell.click();
      await page.locator('#shiny-tab-notifications.active').waitFor();
      assert.equal(await page.locator('#open_notifications').count(), 1);
      if (width > 767) {
        await page.locator('.sidebar-toggle').click();
        await page.waitForFunction(() => document.body.classList.contains('sidebar-collapse'), null, {timeout:15000});
        await bell.click();
        const compact = await bell.boundingBox();
        assert.ok(compact && compact.x >= 0 && compact.x + compact.width <= width, 'Collapsed header bell remains visible');
      }
      assert.deepEqual(errors, []);
      await page.screenshot({path:`/tmp/futmondo-header-notifications-${width}.png`});
      console.log(`PASS ${width}px: icon-only bell beside title, unread tooltip/dot, notification navigation and header fit`);
      await page.close();
    }
  } finally { await browser.close(); }
})().catch(error => { console.error(error.stack); process.exitCode = 1; });
