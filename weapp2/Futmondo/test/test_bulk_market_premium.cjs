// Start Rscript test/test_browser_app.R first; uses synthetic, HTTP-blocked data.
const assert = require('node:assert/strict');
const { chromium } = require((process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules') + '/playwright');
const money = value => `${value.toLocaleString('de-DE')} €`;
(async () => {
  const browser = await chromium.launch({headless:true,args:['--no-sandbox']});
  try {
    for (const width of [320,390,1440]) {
      const page = await browser.newPage({viewport:{width,height:1000}});
      const errors = [];
      page.on('pageerror', error => errors.push(error.message));
      await page.route('**/*', route => route.request().url().startsWith('http://127.0.0.1:38765/') ? route.continue() : route.abort());
      await page.goto('http://127.0.0.1:38765/');
      await page.locator('#login-user_name').fill('offline@example.invalid');
      await page.locator('#login-password').fill('fixture');
      await page.locator('#login-login_button').click();
      await page.waitForFunction(() => document.querySelector('#open_notifications')?.getAttribute('aria-label') === 'Notifications (1 unread)');
      const team = page.locator('.sidebar-menu a[data-value="yourteam"]');
      await team.waitFor();
      await team.evaluate(el => el.click());
      await page.locator('#shiny-tab-yourteam.active').waitFor();
      const open = page.locator('#players_in_teams-btn_put_all_on_market');
      await open.waitFor();
      await open.click();
      const modal = page.locator('#shiny-modal');
      const field = modal.locator('#players_in_teams-bulk_listing_clause_premium');
      const preview = modal.locator('#players_in_teams-bulk_listing_preview');
      await field.waitFor();
      assert.equal(await field.inputValue(), '0');
      await preview.getByText('17 player(s) · Total increase: +0 €', {exact:true}).waitFor();
      for (const pct of [0.5,1,2,4,5,10]) {
        const button = modal.getByRole('button', {name:`+${pct}%`,exact:true});
        await button.click();
        assert.equal(await field.inputValue(), String(pct));
        const extra = 2000000*pct/100;
        await preview.getByText(`17 player(s) · Total increase: +${money(extra*17)}`, {exact:true}).waitFor();
        assert.equal(await preview.getByText(`Extra: + ${money(extra)}`, {exact:true}).count(),17);
        assert.equal(await preview.getByText(`Listing: ${money(2000000+extra)}`, {exact:true}).count(),17);
        const bounds = await button.boundingBox();
        assert.ok(bounds && bounds.x >= 0 && bounds.x+bounds.width <= width, `${width}px +${pct}% fits`);
      }
      await field.fill('1.25');
      await field.dispatchEvent('change');
      await preview.getByText('17 player(s) · Total increase: +425.000 €', {exact:true}).waitFor();
      await modal.getByRole('radio', {name:'Closed clause plus premium',exact:true}).check();
      await preview.getByText('No players have a valid closed clause to use for this listing method.', {exact:true}).waitFor();
      assert.equal(await field.inputValue(), '1.25');
      await modal.getByRole('radio', {name:'Current market value',exact:true}).check();
      await preview.getByText('17 player(s) · Total increase: +425.000 €', {exact:true}).waitFor();
      for (const invalid of ['-1','1001','']) {
        await field.fill(invalid);
        await field.dispatchEvent('change');
        await preview.getByText('Enter a premium between 0 and 1000%.', {exact:true}).waitFor();
        assert.equal(await preview.getByText(/Total increase/).count(),0);
      }
      await modal.getByRole('button', {name:'+4%',exact:true}).click();
      await preview.getByText('17 player(s) · Total increase: +1.360.000 €', {exact:true}).waitFor();
      assert.equal(await modal.evaluate(el => el.scrollWidth>el.clientWidth+2),false,`${width}px modal does not overflow`);
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth>innerWidth+2),false,`${width}px page does not overflow`);
      assert.deepEqual(await modal.locator('.shiny-output-error:visible').allTextContents(),[]);
      assert.deepEqual(errors,[]);
      await page.screenshot({path:`/tmp/futmondo-bulk-market-premium-${width}.png`});
      await modal.getByRole('button', {name:'Cancel',exact:true}).click();
      await modal.waitFor({state:'hidden'});
      await open.click();
      await field.waitFor();
      assert.equal(await field.inputValue(),'0');
      await preview.getByText('17 player(s) · Total increase: +0 €', {exact:true}).waitFor();
      console.log(`PASS ${width}px: six presets, custom premium, exact row/total increases, clause eligibility, invalid values, modal fit and reopening`);
      await page.close();
    }
  } finally { await browser.close(); }
})().catch(error => {console.error(error.stack);process.exitCode=1;});
