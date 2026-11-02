const assert = require('node:assert/strict');
const path = require('node:path');
const {pathToFileURL} = require('node:url');
const {chromium} = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules', 'playwright'));
(async () => {
  const browser = await chromium.launch({headless:true,args:['--no-sandbox']});
  try {
    for (const width of [1440,390]) {
      const page = await browser.newPage({viewport:{width,height:1000}});
      page.setDefaultTimeout(10000);
      await page.goto(pathToFileURL(process.argv[2]).href);
      await page.locator('.fm-metric-badge').first().waitFor();
      const badges = await page.locator('.fm-metric-badge').evaluateAll(nodes => nodes.map(n => ({score:Number(n.textContent),bg:getComputedStyle(n).backgroundColor,fg:getComputedStyle(n).color})));
      assert.equal(badges.find(x=>x.score===856).bg,'rgb(255, 107, 107)');
      assert.equal(badges.find(x=>x.score===-2).bg,'rgb(11, 18, 13)');
      assert.ok(badges.some(x=>x.score===0)); assert.equal(badges.length,5);
      const luminance = color => color.match(/[\d.]+/g).slice(0,3).map(Number).map(v=>{v/=255;return v<=0.04045?v/12.92:((v+0.055)/1.055)**2.4;}).reduce((s,v,i)=>s+v*[0.2126,0.7152,0.0722][i],0);
      for(const badge of badges){const b=luminance(badge.bg),f=luminance(badge.fg);assert.ok((Math.max(b,f)+.05)/(Math.min(b,f)+.05)>=4.5);}
      assert.ok((await page.locator('.rt-tbody').innerText()).includes('02/11/2026 13:33'));
      await page.locator('.rt-th').filter({hasText:'Points'}).click();
      const sorted=await page.locator('.fm-metric-badge').allTextContents();
      assert.deepEqual(sorted.map(Number),[-2,0,771,791,856]);
      const after=await page.locator('.fm-metric-badge').evaluateAll(nodes=>nodes.map(n=>({score:Number(n.textContent),bg:getComputedStyle(n).backgroundColor,fg:getComputedStyle(n).color})));
      assert.deepEqual(after.sort((a,b)=>a.score-b.score),badges.sort((a,b)=>a.score-b.score));
      const arrival=page.locator('.fm-admin-setting').filter({hasText:'Clause arrival delay'});
      assert.equal(await arrival.locator('summary strong').innerText(),'4 days');
      assert.equal(await arrival.locator('p').isVisible(),false);
      await arrival.locator('summary').click();assert.equal(await arrival.locator('p').isVisible(),true);
      await arrival.locator('summary').click();
      const extra=page.locator('.fm-admin-reported');assert.equal(await extra.locator('dl').isVisible(),false);
      await extra.locator('summary').click();assert.equal(await extra.locator('dl').isVisible(),true);
      assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+2),false);
      await extra.locator('summary').click();
      const cards=await page.locator('.fm-admin-settings-card').evaluateAll(nodes=>nodes.map(n=>({top:n.getBoundingClientRect().top,left:n.getBoundingClientRect().left})));
      assert.equal(new Set(cards.map(c=>c.left)).size,width===1440?3:1);
      if(width===390)assert.equal(new Set(cards.map(c=>c.top)).size,6);
      await page.locator('.fm-admin-settings-grid').screenshot({path:`/tmp/futmondo-admin-compact-${width}.png`});
      console.log(`PASS ${width}px: observed values, stable sorted red points pills, contrast, compact settings and no page overflow`);
      await page.close();
    }
  } finally {await browser.close();}
})().catch(e=>{console.error(e.stack);process.exitCode=1;});
