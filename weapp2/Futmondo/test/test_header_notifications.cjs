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
      const dot = bell.locator('.fm-notification-unread-dot');
      assert.equal(await dot.count(), 1);
      const dotBounds = await dot.boundingBox();
      assert.ok(dotBounds && dotBounds.width === 7 && dotBounds.height === 7, 'Unread header dot stays compact');
      assert.equal(await bell.getAttribute('title'), 'Notifications (1 unread)');
      await bell.click();
      await page.locator('#shiny-tab-notifications.active').waitFor();
      assert.equal(await page.locator('#open_notifications').count(), 1);
      const card = page.locator('#notifications-items .fm-notification-unread');
      await card.waitFor();
      const inbox = await page.locator('#notifications-items').boundingBox();
      const bounds = await card.boundingBox();
      assert.equal(await card.evaluate(el => getComputedStyle(el).position), 'static', 'Unread cards remain in normal document flow');
      assert.ok(inbox && bounds && bounds.width >= inbox.width - 2, `${width}px unread card fills the inbox`);
      assert.ok(bounds.x >= 0 && bounds.x + bounds.width <= width, `${width}px unread card fits viewport`);
      assert.ok(await card.locator('strong').innerText(), 'Unread message remains visible');
      assert.match(await card.innerText(), /Player 1/);
      assert.equal(await card.evaluate(el => el.scrollWidth > el.clientWidth + 2), false, `${width}px unread card has no horizontal overflow`);
      for (const label of ['Player details', 'Mark read']) {
        const button = card.getByRole('button', {name:label,exact:true});
        assert.ok(await button.isVisible(), `${width}px ${label} is visible`);
        const action = await button.boundingBox();
        assert.ok(action && action.x >= bounds.x && action.x + action.width <= bounds.x + bounds.width && action.y >= bounds.y && action.y + action.height <= bounds.y + bounds.height, `${width}px ${label} fits inside unread card`);
      }
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 2), false, `${width}px notifications page has no horizontal overflow`);
      if (width > 767) {
        await page.locator('.sidebar-toggle').click();
        await page.waitForFunction(() => document.body.classList.contains('sidebar-collapse'), null, {timeout:15000});
        await bell.click();
        const compact = await bell.boundingBox();
        assert.ok(compact && compact.x >= 0 && compact.x + compact.width <= width, 'Collapsed header bell remains visible');
      }
      assert.deepEqual(errors, []);
      await page.screenshot({path:`/tmp/futmondo-header-notifications-${width}.png`});
      console.log(`PASS ${width}px: header bell/dot, navigation, full-width unread card, visible message/actions and no horizontal overflow`);
      await page.close();
    }
  } finally { await browser.close(); }
})().catch(error => { console.error(error.stack); process.exitCode = 1; });
