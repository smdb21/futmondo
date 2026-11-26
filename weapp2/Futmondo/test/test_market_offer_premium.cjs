// Start the existing synthetic, HTTP-blocked test/test_browser_app.R first.
const assert = require('node:assert/strict');
const { chromium } = require((process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules') + '/playwright');
const money = value => `${value.toLocaleString('de-DE')} €`;
(async () => {
  const browser = await chromium.launch({headless:true,args:['--no-sandbox'],
    ...(process.env.FUTMONDO_TEST_CHROMIUM ? {executablePath:process.env.FUTMONDO_TEST_CHROMIUM} : {})});
  try {
    for (const width of [320,390,1440]) for (const theme of ['dark','light']) {
      const page = await browser.newPage({viewport:{width,height:1000}});
      const errors = [];
      page.on('pageerror', error => errors.push(error.message));
      await page.route('**/*', route => route.request().url().startsWith('http://127.0.0.1:38765/') ? route.continue() : route.abort());
      await page.goto('http://127.0.0.1:38765/');
      await page.evaluate(theme=>document.documentElement.dataset.fmTheme=theme,theme);
      await page.locator('#login-open_login').click();
      await page.locator('#login-user_name').fill('offline@example.invalid');
      await page.locator('#login-password').fill('fixture');
      await page.locator('#login-login_button').click();
      const today = page.locator('.sidebar-menu a[data-value="today"]');
      await today.waitFor();await today.evaluate(el => el.click());
      const table = page.locator('#today-market_players-players_table');
      await table.locator('.rt-tbody .fm-player-crest').first().click();
      const modal = page.locator('#shiny-modal');
      await modal.locator('button[id$="-btn_bid_market"]').click();
      const field = modal.locator('input[id$="-bid_amount"]');
      const preview = modal.locator('[id$="-bid_amount_preview"]');
      await field.waitFor();
      assert.equal(await field.inputValue(),'2000000');
      const timings = [];
      for (const pct of [0.5,1,2,4,5,10]) {
        const started = Date.now();
        const button = modal.getByRole('button', {name:`+${pct}%`,exact:true});
        await button.click();
        const expected = 2000000 + 2000000*pct/100;
        assert.equal(await field.inputValue(),String(expected));
        await preview.getByText(money(expected),{exact:true}).waitFor();
        timings.push(Date.now()-started);
        const bounds = await button.boundingBox();
        assert.ok(bounds && bounds.x>=0 && bounds.x+bounds.width<=width,`${width}px ${pct}% fits`);
      }
      await modal.getByRole('button', {name:'+10%',exact:true}).click();
      assert.equal(await field.inputValue(),'2200000','repeated clicks do not compound');
      await field.fill('2345678');await field.dispatchEvent('change');
      await preview.getByText('2.345.678 €',{exact:true}).waitFor();
      await modal.getByRole('button', {name:'+0.5%',exact:true}).click();
      assert.equal(await field.inputValue(),'2010000','manual edits do not change the premium base');
      await preview.getByText('2.010.000 €',{exact:true}).waitFor();
      assert.equal(await modal.evaluate(el=>el.scrollWidth>el.clientWidth+2),false);
      assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+2),false);
      assert.deepEqual(await modal.locator('.shiny-output-error:visible').allTextContents(),[]);
      assert.deepEqual(errors,[]);
      await page.screenshot({path:`/tmp/futmondo-market-offer-premium-${width}-${theme}.png`});
      await modal.getByRole('button',{name:'Cancel',exact:true}).click();
      await modal.waitFor({state:'hidden'});
      // Selecting the same row again reopens the card; the offer must start at its minimum.
      await table.locator('.rt-tbody .fm-player-crest').first().click();
      await modal.locator('button[id$="-btn_bid_market"]').click();
      await field.waitFor();assert.equal(await field.inputValue(),'2000000');
      console.log(`PASS ${width}px/${theme}: six premiums, noncompounding edits, live preview, cancellation/reopen and no overflow; max click-to-preview ${Math.max(...timings)}ms`);
      await page.close();
    }
  } finally {await browser.close();}
})().catch(error=>{console.error(error.stack);process.exitCode=1;});
