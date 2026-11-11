const assert=require('assert/strict');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
(async()=>{
 const browser=await chromium.launch({headless:true,args:['--no-sandbox']});
 try{
  for(const width of [360,390,1440]){
   const page=await browser.newPage({viewport:{width,height:900},timezoneId:'Europe/Madrid'});
   const errors=[];page.on('pageerror',x=>errors.push(x.message));
   await page.route('**/*',route=>new URL(route.request().url()).hostname==='127.0.0.1'?route.continue():route.abort());
   await page.goto('http://127.0.0.1:3897');
   await page.getByText('Background sync',{exact:true}).click();
   await page.locator('#worker_timezone').waitFor();
   assert.equal(await page.locator('#worker_quiet_start').inputValue(),'0');
   assert.equal(await page.locator('#worker_quiet_end').inputValue(),'7');
   await page.locator('#worker_timezone').fill('Europe/Madrid');
   await page.locator('#worker_enable').check();
   await page.locator('#worker_save').click();
   await page.getByText('Background sync settings saved.').waitFor();
   await page.locator('#worker_status').getByText(/Background sync is enabled/).waitFor();
   // An unrelated worker poll must not reset unsaved settings.
   if(!await page.locator('#worker_settings_details').evaluate(el=>el.open))await page.locator('summary').click();
   await page.locator('#worker_quiet_end').fill('9');
   await page.waitForTimeout(5500);
   assert.equal(await page.locator('#worker_quiet_end').inputValue(),'9');
   const before=await page.locator('.fm-browser-countdown').textContent();
   await page.waitForFunction(old=>document.querySelector('.fm-browser-countdown').textContent!==old,before,{timeout:4000});
   const start=Date.now();await page.locator('#heartbeat').click();
   await page.waitForFunction(()=>document.querySelector('#heartbeat_value').textContent==='1');
   const elapsed=Date.now()-start;assert.ok(elapsed<500,'heartbeat '+elapsed+'ms');
   assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false);
   assert.deepEqual(errors,[]);
   await page.screenshot({path:'/tmp/futmondo-worker-'+width+'.png',fullPage:true});
   console.log('PASS worker consent/settings, retained edit, local clock and heartbeat at '+width+'px; heartbeat='+elapsed+'ms');
   await page.close();
  }
 }finally{await browser.close()}
})().catch(e=>{console.error(e);process.exitCode=1});
