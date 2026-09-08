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
      for (const tab of ['today','yourteam','market','players_in_championship','rivals','classification','notifications','intelligence','automation']) {
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
      await page.close();
    }
  } finally { await browser.close(); }
})().catch(error=>{console.error(error.stack);process.exitCode=1;});
