const fs=require('node:fs');
const assert=require('node:assert/strict');
const {chromium}=require('/tmp/futmondo-review-tools/node_modules/playwright');
(async()=>{
  const browser=await chromium.launch({args:['--no-sandbox']});
  try {
    for(const width of [360,1440]) {
      const page=await browser.newPage({viewport:{width,height:300}});
      await page.setContent(fs.readFileSync('/tmp/futmondo-recommendation-controls.html','utf8'));
      const label=page.locator('.today-heuristic-label'), button=page.locator('.today-recommendation-action');
      assert.equal(await label.evaluate(e=>getComputedStyle(e).backgroundColor),'rgb(5, 8, 5)');
      assert.equal(await button.evaluate(e=>getComputedStyle(e).backgroundColor),'rgb(11, 18, 13)');
      assert.ok((await button.boundingBox()).height>=44);
      await button.focus();
      assert.equal(await button.evaluate(e=>getComputedStyle(e).outlineStyle),'solid');
      console.log(`PASS dark recommendation controls, touch height and keyboard focus at ${width}px`);
      await page.close();
    }
  } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
