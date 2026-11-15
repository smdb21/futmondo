const assert=require('node:assert/strict');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
(async()=>{
 const browser=await chromium.launch({headless:true,args:['--no-sandbox']});
 try{
  for(const width of [320,390,1440]){
   const context=await browser.newContext({viewport:{width,height:1000},timezoneId:'America/Los_Angeles'});
   const page=await context.newPage();const errors=[];page.on('pageerror',e=>errors.push(e.message));
   await page.route('**/*',r=>new URL(r.request().url()).hostname==='127.0.0.1'?r.continue():r.abort());
   await page.addInitScript(()=>{navigator.geolocation.getCurrentPosition=()=>{throw Error('Unexpected location permission request')};navigator.geolocation.watchPosition=()=>{throw Error('Unexpected location permission request')};});
   await page.goto(`http://127.0.0.1:3891/?case=personal-${width}-${Date.now()}`);
   await page.waitForFunction(()=>document.querySelector('#dates').textContent==='15/11/2026 07:02 CET'&&document.querySelector('#save_timezone')&&!document.querySelector('#save_timezone').disabled);
   assert.ok((await page.locator('#incoming').innerText()).includes('15/11/2026 07:02 CET'));
   assert.equal(await page.locator('#notices').innerText(),'15/11/2026 07:02 CET');
   assert.equal(JSON.parse(await page.locator('#stats').innerText()).writes,0,'no automatic/browser preference writes');
   assert.equal(await page.locator('#admin-timezone_user,#timezone_user,#timezone_search').count(),0);
   assert.ok((await page.locator('#timezone_settings').textContent()).includes('Default: Spain (Europe/Madrid)'));
   await page.locator('#timezone_settings summary').click();
   await page.evaluate(()=>$('#display_timezone')[0].selectize.setValue('America/Los_Angeles'));
   await page.locator('#save_timezone').click();
   await page.waitForFunction(()=>document.querySelector('#save_timezone').disabled);
   const start=Date.now();await page.locator('#heartbeat').click();await page.waitForFunction(()=>document.querySelector('#beats').textContent==='1');
   const elapsed=Date.now()-start;assert.ok(elapsed<500,`held save blocked heartbeat for ${elapsed}ms`);
   assert.equal(await page.locator('#dates').innerText(),'15/11/2026 07:02 CET','unacknowledged change must not publish');
   await page.waitForFunction(()=>document.querySelector('#dates').textContent==='14/11/2026 22:02 PST');
   await page.locator('#switch_user').click();
   await page.waitForFunction(()=>document.querySelector('#dates').textContent==='15/11/2026 15:02 JST');
   await page.locator('#timezone_settings summary').click();
   await page.evaluate(()=>$('#display_timezone')[0].selectize.setValue('Europe/Madrid'));
   await page.locator('#save_timezone').click();
   await page.waitForFunction(()=>document.querySelector('#dates').textContent==='15/11/2026 07:02 CET'&&document.querySelector('#save_timezone')&&!document.querySelector('#save_timezone').disabled);
   await page.locator('#switch_user').click();
   await page.waitForFunction(()=>document.querySelector('#dates').textContent==='14/11/2026 22:02 PST');
   await page.locator('#signout').click();await page.waitForFunction(()=>document.querySelector('#dates').textContent==='15/11/2026 07:02 CET');
   await page.locator('#signout').click();await page.waitForFunction(()=>document.querySelector('#dates').textContent==='14/11/2026 22:02 PST');
   await page.reload();await page.waitForFunction(()=>document.querySelector('#dates').textContent==='14/11/2026 22:02 PST');
   assert.equal(JSON.parse(await page.locator('#stats').innerText()).writes,2,'session reloads reuse durable choices');
   await page.locator('#timezone_settings summary').click();
   assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+2),false);
   assert.deepEqual(errors,[]);assert.equal(await page.locator('.shiny-output-error').count(),0);
   await page.screenshot({path:`/tmp/personal-timezone-${width}.png`,fullPage:true});
   console.log(`PASS ${width}px: Spain default despite LA browser/legacy auto value; personal saves, account isolation, durable reload, no admin editor; held-save heartbeat ${elapsed}ms`);
   await context.close();
  }
 }finally{await browser.close();}
})().catch(e=>{console.error(e.stack);process.exitCode=1});
