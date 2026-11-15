const assert = require('node:assert/strict');
const {chromium} = require((process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules') + '/playwright');
(async () => {
  const browser = await chromium.launch({headless:true, executablePath:process.env.FUTMONDO_TEST_CHROMIUM || undefined, args:['--no-sandbox']});
  try {
    for (const width of [320,390,768,1440]) {
      for (const theme of ['dark','light']) {
        const page = await browser.newPage({viewport:{width,height:1000}});
        const errors = [];
        page.on('pageerror', e => errors.push(e.message));
        await page.route('**/*', route => new URL(route.request().url()).hostname === '127.0.0.1' ? route.continue() : route.abort());
        await page.goto('http://127.0.0.1:' + (process.env.FUTMONDO_ADMIN_TEST_PORT || '3917'));
        await page.locator('#beats').filter({hasText:'0'}).waitFor();
        await page.evaluate(mode => document.documentElement.dataset.fmTheme = mode, theme);
        const started = Date.now();
        await page.locator('#toggle_admin').click();
        await page.locator('.fm-admin-setting').first().waitFor();
        const visible = Date.now() - started;
        assert.ok(visible < 2000, `settings took ${visible}ms to render`);
        assert.equal(await page.locator('#admin-timezone_admin').count(), 0);
        const beat = Date.now();
        await page.locator('#heartbeat').click();
        await page.waitForFunction(() => document.querySelector('#beats').textContent === '1');
        const heartbeat = Date.now() - beat;
        assert.ok(heartbeat < 1000, `heartbeat took ${heartbeat}ms`);
        assert.equal(await page.locator('.fm-admin-section').count(), 4);
        const settingsText = await page.locator('#admin-league_settings').textContent();
        assert.ok(settingsText.includes("Only league's admin (might be different than LigaRadar admin) can make changes in Futmondo official app."));
        assert.ok(!settingsText.includes('change it in LigaRadar'));
        const geometry = await page.locator('.fm-admin-settings-grid .fm-admin-settings-card').evaluateAll(cards => cards.map(card => {
          const style = getComputedStyle(card);
          return {left:card.getBoundingClientRect().left, border:style.borderTopStyle, borderWidth:style.borderTopWidth,
            values:[...card.querySelectorAll('summary strong')].map(n => n.getBoundingClientRect().left)};
        }));
        assert.equal(geometry.length, 6);
        assert.equal(new Set(geometry.map(c => c.left)).size, width >= 992 ? 3 : 1);
        for (const card of geometry) {
          assert.equal(card.border, 'solid'); assert.equal(card.borderWidth, '1px');
          assert.ok(Math.max(...card.values) - Math.min(...card.values) < 1, 'setting values must share a column');
        }
        const arrival = page.locator('.fm-admin-setting').filter({hasText:'Clause arrival delay'});
        assert.equal(await arrival.locator('strong').innerText(), '4 days');
        assert.equal(await arrival.locator('p').isVisible(), false);
        await arrival.locator('summary').focus(); await page.keyboard.press('Enter');
        assert.equal(await arrival.locator('p').isVisible(), true);
        await page.keyboard.press('Enter');
        const reported = page.locator('.fm-admin-reported');
        await reported.locator('summary').click(); assert.equal(await reported.locator('dl').isVisible(), true);
        assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 2), false);
        await reported.locator('summary').click();
        await page.locator('.fm-admin').screenshot({path:`/tmp/admin-settings-${width}-${theme}.png`});
        // Admin navigation never loads a user timezone directory.
        await page.locator('#toggle_admin').click();
        await page.locator('#page').filter({hasText:'Another page'}).waitFor({timeout:1000});
        await page.locator('#toggle_admin').click();
        await page.locator('.fm-admin-setting').first().waitFor({timeout:1000});
        assert.equal(await page.locator('#reads').innerText(), '0');
        assert.deepEqual(errors, []);
        assert.equal(await page.locator('.shiny-output-error').count(), 0);
        console.log(`PASS ${width}px ${theme}: settings ${visible}ms, heartbeat ${heartbeat}ms; boxes/alignment/disclosures/navigation; zero timezone-directory reads`);
        await page.close();
      }
    }
  } finally { await browser.close(); }
})().catch(error => {console.error(error.stack);process.exitCode = 1;});
