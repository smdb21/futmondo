// Start the existing synthetic, HTTP-blocked test/test_player_offer_browser.R first.
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
      await page.route('**/*', route => route.request().url().startsWith('http://127.0.0.1:3895/') ? route.continue() : route.abort());
      await page.goto('http://127.0.0.1:3895/');
      await page.evaluate(theme=>document.documentElement.dataset.fmTheme=theme,theme);
      const modal = page.locator('#shiny-modal');
      const open = async () => {
        await page.getByRole('button', {name:'Open market player'}).click();
        await modal.getByRole('button', {name:'Make Market Offer'}).click();
      };
      await open();
      const field = modal.locator('input[id$="-bid_amount"]');
      const preview = modal.locator('[id$="-bid_amount_preview"]');
      await field.waitFor();
      const group = modal.getByRole('group');
      const buttons = group.getByRole('button');
      const presets = [1,2,4,5,10];
      assert.deepEqual(await buttons.allTextContents(), presets.map(p => `+${p}%`));
      const bounds = await buttons.evaluateAll(nodes => nodes.map(n => n.getBoundingClientRect().toJSON()));
      assert.ok(bounds.every(r => r.top === bounds[0].top && r.left >= 0 && r.right <= width && r.height >= 44));
      const total = preview.locator('.fm-offer-total');
      const percent = preview.locator('.fm-offer-percent');
      const warning = preview.locator('.fm-offer-warning');
      async function check(amount, pct) {
        assert.equal(await field.inputValue(), String(amount));
        assert.equal(await total.textContent(), money(amount));
        assert.equal(await percent.textContent(), pct);
      }
      await check(150, '0%');
      assert.match(await warning.textContent(), /below the player’s market value/);
      const timings = [];
      for (const [pct, amount, effective] of [
        [1,152,'+1.33%'], [1,153,'+2%'], [2,156,'+4%'], [4,162,'+8%'],
        [5,170,'+13.33%'], [10,185,'+23.33%']
      ]) {
        const started = Date.now();
        await group.getByRole('button', {name:`+${pct}%`,exact:true}).click();
        await check(amount, effective);
        timings.push(Date.now() - started);
      }
      await field.fill('180');
      await check(180, '+20%');
      await group.getByRole('button', {name:'+10%',exact:true}).click();
      await check(195, '+30%');
      await field.fill('135');
      await check(135, '-10%');
      assert.match(await warning.textContent(), /below the listed price/);
      await field.fill('');
      assert.equal(await preview.innerText(), '');
      await field.fill('1852568');
      assert.equal(await warning.textContent(), '');
      assert.notEqual(await total.evaluate(n => getComputedStyle(n).color),
        await percent.evaluate(n => getComputedStyle(n).color));
      assert.equal(await modal.evaluate(el=>el.scrollWidth>el.clientWidth+2),false);
      assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+2),false);
      assert.deepEqual(await modal.locator('.shiny-output-error:visible').allTextContents(),[]);
      assert.deepEqual(errors,[]);
      await page.screenshot({path:`/tmp/futmondo-market-offer-premium-${width}-${theme}.png`});
      await modal.getByRole('button',{name:'Cancel',exact:true}).click();
      await modal.waitFor({state:'hidden'});
      await open();
      await field.waitFor();
      await check(150, '0%');
      console.log(`PASS ${width}px/${theme}: cumulative premiums, manual edits, valuation warnings and reset; max click-to-preview ${Math.max(...timings)}ms`);
      await page.close();
    }
  } finally {await browser.close();}
})().catch(error=>{console.error(error.stack);process.exitCode=1;});
