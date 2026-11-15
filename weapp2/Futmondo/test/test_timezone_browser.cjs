const assert=require('node:assert/strict');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
(async()=>{
 const browser=await chromium.launch({headless:true,args:['--no-sandbox']});
 try{
  for(const width of [390,1440]){
   const context=await browser.newContext({viewport:{width,height:1000},timezoneId:'America/Los_Angeles'});
   const page=await context.newPage();const errors=[];page.on('pageerror',e=>errors.push(e.message));
   await page.route('**/*',r=>new URL(r.request().url()).hostname==='127.0.0.1'?r.continue():r.abort());
   await page.addInitScript(()=>{navigator.geolocation.getCurrentPosition=()=>{throw Error('Unexpected location permission request')};navigator.geolocation.watchPosition=()=>{throw Error('Unexpected location permission request')};});
   await page.goto('http://127.0.0.1:3891');
   await page.waitForFunction(()=>document.querySelector('#dates').textContent==='14/11/2026 22:02 PST');
   assert.ok((await page.locator('#incoming').innerText()).includes('14/11/2026 22:02 PST'));
   assert.equal(await page.locator('#notices').innerText(),'14/11/2026 22:02 PST');
   await page.waitForFunction(()=>document.querySelector('#admin-timezone_user')?.selectize?.options.admin);
   await page.evaluate(()=>$('#admin-timezone_user')[0].selectize.setValue('alice'));
   await page.waitForFunction(()=>document.querySelector('#admin-timezone_choice')?.value===((window.innerWidth===390)?'Europe/Madrid':'Asia/Kolkata'));
   await page.evaluate(()=>$('#admin-timezone_choice')[0].selectize.setValue('Asia/Kolkata'));
   await page.locator('#admin-timezone_save').click();
   await page.waitForFunction(()=>document.querySelector('#admin-timezone_status').textContent.includes('Timezone saved: Asia/Kolkata'));
   await page.waitForFunction(()=>!document.querySelector('#admin-timezone_save').disabled);
   assert.equal(await page.locator('#dates').innerText(),'14/11/2026 22:02 PST','another account must remain isolated');
   await page.evaluate(()=>$('#admin-timezone_user')[0].selectize.setValue('admin'));
   await page.waitForFunction(()=>document.querySelector('#admin-timezone_editor [data-user-id]')?.dataset.userId==='admin' && document.querySelector('#admin-timezone_choice')?.value==='America/Los_Angeles');
   await page.evaluate(()=>$('#admin-timezone_choice')[0].selectize.setValue('Europe/Madrid'));
   await page.locator('#admin-timezone_save').click();
   await page.waitForFunction(()=>document.querySelector('#dates').textContent==='15/11/2026 07:02 CET');
   assert.equal(await page.locator('#notices').innerText(),'15/11/2026 07:02 CET');
   await page.locator('#signout').click();await page.locator('#signout').click();
   await page.waitForFunction(()=>document.querySelector('#dates').textContent==='15/11/2026 07:02 CET');
   await page.reload();await page.waitForFunction(()=>document.querySelector('#dates').textContent==='15/11/2026 07:02 CET');
   await page.locator('#timezone_settings summary').click();
   await page.evaluate(()=>$('#display_timezone')[0].selectize.setValue('America/Los_Angeles'));
   await page.locator('#save_timezone').click();
   await page.waitForFunction(()=>document.querySelector('#dates').textContent==='14/11/2026 22:02 PST');
   const start=Date.now();await page.locator('#heartbeat').click();await page.waitForFunction(()=>document.querySelector('#beats').textContent==='1');
   assert.ok(Date.now()-start<500);assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+2),false);assert.deepEqual(errors,[]);
   console.log(`PASS ${width}px browser detection without geolocation, module-wide dates, user/admin saves, cross-account isolation, reload persistence, responsive controls and heartbeat ${Date.now()-start}ms`);
   await context.close();
  }
 }finally{await browser.close();}
})().catch(e=>{console.error(e.stack);process.exitCode=1});
