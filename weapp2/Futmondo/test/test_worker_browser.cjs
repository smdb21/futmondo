const assert=require('assert/strict');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:process.env.FUTMONDO_TEST_CHROMIUM||undefined,args:['--no-sandbox']});
 try{
  for(const width of [360,390,1440]){
   const page=await browser.newPage({viewport:{width,height:900},timezoneId:'Europe/Madrid'});
   const errors=[];page.on('pageerror',x=>errors.push(x.message));
   await page.route('**/*',route=>new URL(route.request().url()).hostname==='127.0.0.1'?route.continue():route.abort());
   await page.goto('http://127.0.0.1:3897');
   await page.waitForFunction(()=>document.querySelector('#worker_status_message')?.textContent.includes('Background sync is enabled'));
   await page.locator('#worker_settings_details summary').click();
   assert.equal(await page.locator('#worker_enable,#worker_quiet_start,#worker_quiet_end').count(),0);
   await page.getByLabel('League B',{exact:true}).uncheck();
   // An unrelated worker poll must not reset unsaved settings.
   await page.waitForTimeout(5500);
   assert.equal(await page.getByLabel('League B',{exact:true}).isChecked(),false);
   await page.locator('#worker_save').click();
   await page.waitForFunction(()=>document.querySelector('#worker_setting_feedback')?.textContent.includes('Background sync settings saved.'));
   const before=await page.locator('.fm-browser-countdown').textContent();
   await page.waitForFunction(old=>document.querySelector('.fm-browser-countdown').textContent!==old,before,{timeout:4000});
   const start=Date.now();await page.locator('#heartbeat').click();
   await page.waitForFunction(()=>document.querySelector('#heartbeat_value').textContent==='1');
   const elapsed=Date.now()-start;assert.ok(elapsed<500,'heartbeat '+elapsed+'ms');
   assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false);
   assert.deepEqual(errors,[]);
   await page.evaluate(()=>{window.fixtureStats=null;Shiny.addCustomMessageHandler('fixture-stats',x=>window.fixtureStats=x);Shiny.setInputValue('fixture_stats',Math.random(),{priority:'event'});});
   await page.waitForFunction(()=>window.fixtureStats!==null);
   assert.equal((await page.evaluate(()=>fixtureStats)).connects,1);
   assert.deepEqual((await page.evaluate(()=>fixtureStats)).excluded,['league-b']);
   await page.screenshot({path:'/tmp/futmondo-worker-'+width+'.png',fullPage:true});
   await page.locator('#worker_disconnect').click();
   await page.getByText('signed out',{exact:true}).waitFor();
   console.log('PASS automatic sync, retained league edit, disconnect/signout, local clock and heartbeat at '+width+'px; heartbeat='+elapsed+'ms');
   await page.close();
  }
 }finally{await browser.close()}
})().catch(e=>{console.error(e);process.exitCode=1});
