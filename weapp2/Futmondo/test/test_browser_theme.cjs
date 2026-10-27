const assert = require('node:assert/strict');
const path = require('node:path');
const { chromium } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules','playwright'));
(async () => {
  const browser = await chromium.launch({headless:true,args:['--no-sandbox']});
  try {
    for (const width of [1440,390,360]) {
      const page = await browser.newPage({viewport:{width,height:1000}});
      await page.route('**/*',route => route.request().url().startsWith('http://127.0.0.1:38765/') ? route.continue() : route.abort());
      await page.goto('http://127.0.0.1:38765/');
      // Test the production UI without an extra fixture stylesheet, before login.
      await page.locator('#login-user_name').waitFor();
      for (const selector of ['.content-wrapper','.main-header .navbar','.main-sidebar','.box','#login-user_name','#login-password','.selectize-input']) {
        const elements = page.locator(selector);
        for (let i=0;i<await elements.count();i++) {
          const element=elements.nth(i);
          if(!await element.isVisible())continue;
          const background=await element.evaluate(el=>getComputedStyle(el).backgroundColor);
          assert.ok(['rgb(5, 8, 5)','rgb(11, 18, 13)'].includes(background),`${width}px login ${selector}: ${background}`);
        }
      }
      await page.screenshot({path:`/tmp/futmondo-login-${width}.png`,fullPage:true});
      await page.locator('#login-user_name').fill('offline@example.invalid');
      await page.locator('#login-password').fill('fixture');
      await page.locator('#login-login_button').click();
      await page.locator('.sidebar-menu a[data-value="yourteam"]').waitFor();
      for (const tab of ['today','yourteam','market','players_in_championship','rivals','classification','notifications','plots','round_mvps']) {
        await page.locator(`.sidebar-menu a[data-value="${tab}"]`).evaluate(el=>el.click());
        await page.waitForTimeout(350);
        const body = await page.locator('body').evaluate(el=>({background:getComputedStyle(el).backgroundColor,font:getComputedStyle(el).fontFamily}));
        assert.equal(body.background,'rgb(5, 8, 5)');assert.match(body.font,/monospace/);
        const errors = await page.locator('.shiny-output-error:visible').allTextContents();
        assert.deepEqual(errors,[],`${width}px ${tab}: ${errors}`);
      }
      await page.locator('.sidebar-menu a[data-value="yourteam"]').evaluate(el=>el.click());
      await page.waitForTimeout(500);
      const overflow = await page.evaluate(()=>document.documentElement.scrollWidth > window.innerWidth+2);
      assert.equal(overflow,false,`Page overflow at ${width}px`);
      await page.screenshot({path:`/tmp/futmondo-terminal-${width}.png`,fullPage:true});
      console.log(`PASS terminal colours, monospace, nine tabs, and responsive page at ${width}px`);
      const roster=page.locator('#players_in_teams-players_table_in_teams-players_table');
      await roster.locator('.rt-select-input').first().check();
      const modal=page.locator('.modal:visible');await modal.waitFor();
      await modal.locator('.player-recent-round-chip').first().waitFor();
      const chips=await modal.locator('.player-recent-round-chip').allTextContents();
      assert.ok(chips.some(x=>x.includes('0 pts'))&&chips.some(x=>x.includes('-2 pts')),`${width}px round scores: ${chips}`);
      await modal.getByRole('radio',{name:'Round points',exact:true}).check();
      await page.waitForFunction(()=>Array.from(document.querySelectorAll('.modal .js-plotly-plot')).some(p=>p.data&&p.data.some(t=>Array.from(t.y||[]).includes(-2))));
      assert.deepEqual(await modal.locator('.shiny-output-error:visible').allTextContents(),[],`${width}px player card errors`);
      assert.match(await modal.innerText(),/Player profile is unavailable/);
      await modal.locator('button[id$="-refresh_history"]').click();
      await page.waitForTimeout(250);
      assert.equal(await modal.locator('.player-recent-round-chip').count(),2);
      const modalOverflow=await modal.evaluate(el=>el.scrollWidth>el.clientWidth+2);
      assert.equal(modalOverflow,false,`Player card overflow at ${width}px`);
      await page.screenshot({path:`/tmp/futmondo-player-card-${width}.png`,fullPage:true});
      console.log(`PASS player-card zero/negative scores, round plot, unavailable profile and refresh at ${width}px`);
      await page.close();
    }
  } finally { await browser.close(); }
})().catch(error=>{console.error(error.stack);process.exitCode=1;});
