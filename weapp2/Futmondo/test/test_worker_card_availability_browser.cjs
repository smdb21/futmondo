const assert=require('node:assert/strict');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:process.env.FUTMONDO_TEST_CHROMIUM||undefined,args:['--no-sandbox']});
 try{
  for(const width of [360,1440]){
   const page=await browser.newPage({viewport:{width,height:960}});const errors=[];
   page.on('pageerror',e=>errors.push(e.message));
   await page.route('**/*',r=>new URL(r.request().url()).hostname==='127.0.0.1'?r.continue():r.abort());
   await page.goto('http://127.0.0.1:3907');
   const verify=page.locator('#card-btn_verify_availability'),offer=page.locator('#card-btn_bid_market');
   await verify.waitFor();await page.waitForFunction(()=>document.querySelector('#card-btn_verify_availability').disabled);assert.equal(await verify.isDisabled(),true);assert.equal(await offer.isDisabled(),true);
   const stats=async()=>{
    await page.evaluate(()=>{window.fixtureStats=null;Shiny.addCustomMessageHandler('fixture-stats',x=>window.fixtureStats=x);Shiny.setInputValue('fixture_stats',Math.random(),{priority:'event'});});
    await page.waitForFunction(()=>window.fixtureStats!==null);return await page.evaluate(()=>fixtureStats);
   };
   assert.equal((await stats()).live,1);
   await page.screenshot({path:`/tmp/futmondo-card-availability-before-${width}.png`,fullPage:true});
   assert.equal((await stats()).live,1);assert.equal(await offer.isDisabled(),true);
   const begin=Date.now();await page.locator('#heartbeat').click();
   await page.waitForFunction(()=>document.querySelector('#heartbeat_value').textContent==='1');
   const heartbeat=Date.now()-begin;assert.ok(heartbeat<750,`heartbeat ${heartbeat}ms`);
   await page.waitForTimeout(1200);assert.equal((await stats()).live,1);
   await page.locator('#finish_success').click();await page.waitForFunction(()=>!document.querySelector('#card-btn_bid_market').disabled);
   assert.equal(await verify.count(),0);
   await page.screenshot({path:`/tmp/futmondo-card-availability-after-${width}.png`,fullPage:true});
   await offer.click();await page.locator('#card-bid_amount').waitFor();
   assert.equal(await page.locator('#card-submit_bid').isEnabled(),true);assert.equal((await stats()).live,1);
   await page.locator('.modal').getByRole('button',{name:'Cancel',exact:true}).click();
   await page.locator('#reset_card').click();await verify.waitFor();
   await page.waitForFunction(()=>document.querySelector('#card-btn_verify_availability').disabled);
   await page.locator('#finish_failure').click();await page.waitForFunction(()=>!document.querySelector('#card-btn_verify_availability').disabled);
   assert.equal((await stats()).live,2);assert.equal(await offer.count(),0);
   assert.match(await page.locator('#card-action_buttons').textContent(),/network|connect|unavailable|retry|load/i);
   await verify.click();await page.waitForFunction(()=>document.querySelector('#card-btn_verify_availability').disabled);
   await page.locator('#finish_partial').click();await page.waitForFunction(()=>!document.querySelector('#card-btn_verify_availability').disabled);
   assert.equal(await offer.isDisabled(),true);assert.match(await page.locator('#card-action_buttons').textContent(),/cash balance did not load/);
   assert.equal((await stats()).live,3);
   await verify.click();await page.waitForFunction(()=>document.querySelector('#card-btn_verify_availability').disabled);
   await page.locator('#finish_success').click();await page.waitForFunction(()=>!document.querySelector('#card-btn_bid_market').disabled);
   assert.equal((await stats()).live,4);
   await page.locator('#hold_saved').click();await verify.waitFor();
   await page.waitForFunction(()=>document.querySelector('#card-btn_verify_availability').disabled);
   assert.equal(await verify.isDisabled(),true);assert.equal((await stats()).passive_pending,true);
   assert.equal((await stats()).live,5);
   const pendingBegin=Date.now();await page.locator('#heartbeat').click();
   await page.waitForFunction(()=>document.querySelector('#heartbeat_value').textContent==='2');
   const pendingHeartbeat=Date.now()-pendingBegin;assert.ok(pendingHeartbeat<750,`pending stored heartbeat ${pendingHeartbeat}ms`);
   await page.locator('#finish_success').click();await page.waitForFunction(()=>!document.querySelector('#card-btn_bid_market').disabled);
   assert.equal((await stats()).passive_pending,true);
   console.log(`PASS ${width}px saved capacity still pending: automatic check runs without waiting for worker publication, heartbeat ${pendingHeartbeat}ms, live success enables actions`);
   assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false);
   assert.deepEqual(errors,[]);
   console.log(`PASS ${width}px actual card clicks: opening starts one held check without a click, heartbeat ${heartbeat}ms, verified offer modal opens, network/partial failures block and retry, zero overflow`);
   await page.close();
  }
 }finally{await browser.close();}
})().catch(e=>{console.error(e.stack);process.exitCode=1});
