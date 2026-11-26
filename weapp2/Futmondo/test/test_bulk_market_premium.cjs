// Start Rscript test/test_browser_app.R --bulk-market first; uses synthetic, HTTP-blocked data.
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
      await page.waitForFunction(() => window.Shiny?.shinyapp?.$socket?.readyState === 1);
      await page.locator('#login-open_login').click();
      await page.locator('#login-user_name').fill('offline@example.invalid');
      await page.locator('#login-password').fill('fixture');
      await page.locator('#login-login_button').click();
      await page.locator('.fm-home-tile[data-fm-page="yourteam"]').waitFor();
      const team = page.locator('.sidebar-menu a[data-value="yourteam"]');
      await team.evaluate(el => el.click());
      await page.locator('#shiny-tab-yourteam.active').waitFor();
      const open = page.locator('#players_in_teams-btn_put_all_on_market');
      await open.waitFor();
      await open.click();
      const modal = page.locator('#shiny-modal');
      const field = modal.locator('#players_in_teams-bulk_listing_clause_premium');
      const preview = modal.locator('#players_in_teams-bulk_listing_preview');
      const confirm = modal.locator('#players_in_teams-submit_put_all_on_market');
      const value = modal.getByRole('radio', {name:'Current market value',exact:true});
      const clause = modal.getByRole('radio', {name:'Closed clause value',exact:true});
      await field.waitFor();
      assert.equal(await field.inputValue(), '0');
      await preview.getByText('Total increase: +0 €', {exact:true}).waitFor();
      assert.equal(await confirm.isEnabled(), true);
      await modal.getByRole('button', {name:'Cancel',exact:true}).click();
      await modal.waitFor({state:'hidden'});
      await open.click();
      await preview.getByText('Market-value pricing applies to 17 of 17 players.', {exact:true}).waitFor();
      await page.waitForFunction(() => document.getElementById('players_in_teams-submit_put_all_on_market')?.disabled === false);
      let appliedPremium = 0;
      for (const pct of [1,1,0.5,2,4,5,10]) {
        const button = modal.getByRole('button', {name:`+${pct}%`,exact:true});
        await button.click();
        appliedPremium += pct;
        assert.equal(await field.inputValue(), String(appliedPremium));
        const extra = 2000000*appliedPremium/100;
        await preview.getByText(`Total increase: +${money(extra*17)}`, {exact:true}).waitFor();
        assert.equal(await preview.getByText(`Extra: +${money(extra)}`, {exact:true}).count(),17);
        assert.equal(await preview.getByText(`Listing: ${money(2000000+extra)}`, {exact:true}).count(),17);
        const bounds = await button.boundingBox();
        assert.ok(bounds && bounds.x >= 0 && bounds.x+bounds.width <= width, `${width}px +${pct}% fits`);
      }
      await field.fill('1.25');
      await field.dispatchEvent('change');
      await preview.getByText('Total increase: +425.000 €', {exact:true}).waitFor();
      await modal.getByRole('button', {name:'+1%',exact:true}).click();
      assert.equal(await field.inputValue(), '2.25');
      await clause.check();
      await preview.getByText('Closed-clause pricing applies to 2 of 17 players.', {exact:true}).waitFor();
      await preview.getByText('Left out 15 players with open clauses.', {exact:true}).waitFor();
      await preview.getByText('Total increase: +180.000 €', {exact:true}).waitFor();
      assert.equal(await preview.getByText('Listing: 4.090.000 €', {exact:true}).count(), 2);
      assert.equal(await preview.getByText('Player 3', {exact:true}).count(), 0);
      assert.equal(await confirm.isEnabled(), true);
      assert.equal(await field.inputValue(), '2.25');
      await value.check();
      await preview.getByText('Market-value pricing applies to 17 of 17 players.', {exact:true}).waitFor();
      await preview.getByText('Total increase: +765.000 €', {exact:true}).waitFor();
      for (const invalid of ['-1','1001','']) {
        await field.fill(invalid);
        await field.dispatchEvent('change');
        await preview.getByText('Enter a premium between 0 and 1000%.', {exact:true}).waitFor();
        assert.equal(await preview.getByText(/Total increase/).count(),0);
        assert.equal(await confirm.isDisabled(), true);
        if (invalid === '1001') {
          await field.fill('1');
          await field.dispatchEvent('change');
          await preview.getByText('Total increase: +340.000 €', {exact:true}).waitFor();
          assert.equal(await confirm.isEnabled(), true);
        }
      }
      await modal.getByRole('button', {name:'+4%',exact:true}).click();
      await preview.getByText('Total increase: +1.360.000 €', {exact:true}).waitFor();
      assert.equal(await field.inputValue(), '4');
      assert.equal(await modal.evaluate(el => el.scrollWidth>el.clientWidth+2),false,`${width}px modal does not overflow`);
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth>innerWidth+2),false,`${width}px page does not overflow`);
      assert.deepEqual(await modal.locator('.shiny-output-error:visible').allTextContents(),[]);
      assert.deepEqual(errors,[]);
      await page.screenshot({path:`/tmp/futmondo-bulk-market-premium-${width}.png`});
      await field.fill('-1');
      await field.dispatchEvent('change');
      await preview.getByText('Enter a premium between 0 and 1000%.', {exact:true}).waitFor();
      assert.equal(await confirm.isDisabled(), true);
      await modal.getByRole('button', {name:'Cancel',exact:true}).click();
      await modal.waitFor({state:'hidden'});
      await open.click();
      await field.waitFor();
      assert.equal(await field.inputValue(),'0');
      await preview.getByText('Total increase: +0 €', {exact:true}).waitFor();
      assert.equal(await confirm.isEnabled(), true);
      await clause.check();
      await preview.getByText('Closed-clause pricing applies to 2 of 17 players.', {exact:true}).waitFor();
      await preview.getByText('Total increase: +0 €', {exact:true}).waitFor();
      assert.equal(await preview.getByText('Listing: 4.000.000 €', {exact:true}).count(), 2);
      await field.fill('1.25');
      await field.dispatchEvent('change');
      await preview.getByText('Total increase: +100.000 €', {exact:true}).waitFor();
      await confirm.click();
      await modal.waitFor({state:'hidden'});
      const notification = page.locator('.shiny-notification').filter({hasText:'Futmondo confirmed 2 of 2 requested listings.'});
      await notification.waitFor();
      assert.equal(await notification.evaluate(el => el.classList.contains('shiny-notification-warning')), false);
      assert.deepEqual(errors,[]);
      console.log(`PASS ${width}px: six presets, custom premium, exact row/total increases, clause eligibility, invalid values, modal fit and two synthetic confirmations`);
      await page.close();
    }
  } finally { await browser.close(); }
})().catch(error => {console.error(error.stack);process.exitCode=1;});
