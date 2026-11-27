const assert = require('node:assert/strict');
const { chromium } = require((process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules') + '/playwright');

(async () => {
  const browser = await chromium.launch({headless:true, args:['--no-sandbox'],
    ...(process.env.FUTMONDO_TEST_CHROMIUM ? {executablePath:process.env.FUTMONDO_TEST_CHROMIUM} : {})});
  try {
    for (const width of [320, 390, 1440]) {
      const page = await browser.newPage({viewport:{width, height:780}});
      const errors = [];
      page.on('pageerror', error => errors.push(error.message));
      await page.route('**/*', route => new URL(route.request().url()).hostname === '127.0.0.1' ? route.continue() : route.abort());
      await page.goto('http://127.0.0.1:3895/');
      await page.waitForFunction(() => Shiny.shinyapp.isConnected());
      const modal = page.locator('#shiny-modal');
      const open = async (name, action, control) => {
        const started = Date.now();
        await page.getByRole('button', {name, exact:true}).click();
        await modal.waitFor({state:'visible'});
        const cardPainted = Date.now();
        const actionButton = modal.getByRole('button', {name:action});
        await actionButton.waitFor({state:'visible'});
        const actionPainted = Date.now();
        await actionButton.click();
        await control.waitFor({state:'visible'});
        return [cardPainted - started, actionPainted - cardPainted, Date.now() - actionPainted];
      };
      const confirm = async (button, pending, acknowledgement) => {
        await modal.getByRole('button', {name:button, exact:true}).click();
        await modal.getByRole('status').getByText(pending).waitFor();
        assert.equal(await modal.getByRole('button', {name:button, exact:true}).isDisabled(), true);
        assert.equal(await modal.getByRole('button', {name:'Cancel', exact:true}).isDisabled(), true);
        await page.keyboard.press('Escape');
        await page.waitForTimeout(150);
        assert.equal(await modal.isVisible(), true);
        assert.equal(await modal.getByRole('status').locator('.fa-spinner').count(), 1);
        assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 2), false);
        await page.getByText(acknowledgement).waitFor();
        await modal.waitFor({state:'hidden'});
      };
      await page.getByRole('button', {name:'Open market player'}).click();
      const card = modal.locator('.player-card');
      const stats = card.locator('.player-card-metrics');
      await stats.getByText('Avg 6.6').waitFor();
      await card.getByText(/Ownership: Free Agent/).waitFor();
      await card.getByRole('status').filter({hasText:'Checking availability…'}).waitFor();
      assert.equal(await card.getByRole('button', {name:'Checking availability…'}).count(), 0);
      assert.equal(await modal.getByText('Player details', {exact:true}).count(), 1);
      assert.equal((await card.innerText()).split('Listed on Market').length - 1, 1);
      assert.equal(await card.getByText(/Ownership: Free Agent/).evaluate(el =>
        el.tagName === 'P' && getComputedStyle(el).borderTopStyle === 'none'), true);
      assert.deepEqual(await stats.locator('dt').allTextContents(), ['Total points', 'Last 5 matches', 'Market value']);
      assert.match(await stats.innerText(), /1\.852\.568 €[\s\S]*-43\.038 € \(-2\.32%\)/);
      const [total, recent, value] = await stats.locator('dt').evaluateAll(nodes =>
        nodes.map(el => el.getBoundingClientRect().top));
      assert.equal(total, recent);
      assert.equal(value === total, width > 540);
      assert.equal(await card.evaluate(el => el.scrollWidth <= el.clientWidth + 1), true);
      const history = card.getByRole('heading', {name:'Value and points history'});
      const historyTop = await history.evaluate(el => el.getBoundingClientRect().top);
      assert.ok(historyTop < 600, 'History heading should fit in the initial viewport');
      await card.getByRole('button', {name:'Refresh history'}).waitFor();
      await card.getByText('Recent rounds', {exact:true}).click();
      assert.equal(await card.locator('.player-recent-rounds').getAttribute('open'), '');
      console.log('PASS ' + width + 'px: single status, aligned scores, signed valuation; history starts at ' + Math.round(historyTop) + 'px');
      await card.getByRole('button', {name:'Close player card'}).click();
      await modal.waitFor({state:'hidden'});
      await page.getByRole('button', {name:'Open rival player'}).click();
      const offer = card.getByRole('button', {name:'Offer to Rival'});
      const buyout = card.getByRole('button', {name:'Buy Clause'});
      await buyout.waitFor();
      await page.mouse.move(0, 0);
      for (const theme of ['light', 'dark']) {
        await page.evaluate(mode => document.documentElement.dataset.fmTheme = mode, theme);
        for (const button of [offer, buyout]) {
          const check = await button.evaluate(async el => {
            await Promise.all(el.getAnimations().map(animation => animation.finished));
            const style = getComputedStyle(el);
            const surface = getComputedStyle(el.closest('.modal-content')).backgroundColor;
            const rect = el.getBoundingClientRect();
            const area = el.closest('.player-card-actions').getBoundingClientRect();
            return {
              outlined: style.borderTopStyle === 'solid' && parseFloat(style.borderTopWidth) >= 2 &&
                style.borderTopColor !== surface,
              filled: style.backgroundColor !== surface && style.backgroundColor !== 'rgba(0, 0, 0, 0)',
              fits: rect.left >= area.left - 1 && rect.right <= area.right + 1 && rect.height >= 44
            };
          });
          assert.deepEqual(check, {outlined:true, filled:true, fits:true}, theme + ' ' + await button.innerText());
        }
      }
      await card.getByRole('button', {name:'Close player card'}).click();
      await modal.waitFor({state:'hidden'});
      const field = modal.getByLabel('Your offer amount (EUR):');
      const market = await open('Open market player', 'Make Market Offer', field);
      assert.equal(await field.inputValue(), '150');
      await modal.getByRole('button', {name:'+0.5%', exact:true}).click();
      assert.equal(await field.inputValue(), '151');
      await field.fill('999');
      await confirm('Submit Market Offer', /Sending offer/, /Your squad has reached/);
      const direct = await open('Open rival player', 'Offer to Rival', modal.getByLabel('Your purchase offer amount (EUR):'));
      await confirm('Submit Offer', /Sending offer/, /Direct offer of/);
      const clause = await open('Open rival player', 'Buy Clause', modal.getByText('Confirm Release Clause Buyout'));
      await confirm('Confirm Clause Buyout', /Sending purchase/, /executed successfully/);
      await open('Open market player', 'Make Market Offer', field);
      await page.keyboard.press('Escape');
      await modal.waitFor({state:'hidden'});
      assert.deepEqual(errors, []);
      console.log('PASS ' + width + 'px: click-to-card, card-to-action and action-to-editor ' + [market,direct,clause].map(pair => pair.join('+')).join('/') + ' ms (market/owner/clause); spinner and locked controls remained visible during each delayed response');
      await page.close();
    }
  } finally { await browser.close(); }
})().catch(error => { console.error(error.stack); process.exitCode = 1; });
