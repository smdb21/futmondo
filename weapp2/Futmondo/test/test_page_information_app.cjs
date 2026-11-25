// Start Rscript test/test_browser_app.R first (deterministic API/DB fixtures).
const assert=require('node:assert/strict');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:process.env.FUTMONDO_TEST_CHROMIUM||undefined,args:['--no-sandbox']});
 try{
  for(const width of [320,390,1440]){
   const page=await browser.newPage({viewport:{width,height:1000}});page.setDefaultTimeout(15000);
   const errors=[];page.on('pageerror',e=>errors.push(e.message));
   await page.route('**/*',route=>route.request().url().startsWith('http://127.0.0.1:38765/')?route.continue():route.abort());
   await page.goto('http://127.0.0.1:38765/');
   const coachKey='futmondo.coach.v1:offline-user:champ';
   const coachCheckbox=page.locator('#lineup-automatically_hire_coach');
   assert.equal(await coachCheckbox.isChecked(),true,'New coach preferences default to enabled');
   await page.evaluate(key=>localStorage.setItem(key,'false'),coachKey);
   await page.locator('#login-user_name').fill('offline@example.invalid');await page.locator('#login-password').fill('fixture');
   await page.locator('#login-login_button').click();
   await page.locator('#shiny-tab-today.active').waitFor();
   assert.equal(await page.locator('#shiny-tab-today .fm-coach-control').count(),0);
   assert.equal(await coachCheckbox.isVisible(),false);
   await page.waitForFunction(()=>document.getElementById('lineup-automatically_hire_coach').checked===false);
   for(const tab of ['today','yourteam','players_in_championship','rivals','classification','round_mvps','notifications','plots','lineup']){
    await page.locator(`.sidebar-menu a[data-value="${tab}"]`).evaluate(n=>n.click());
    const root=page.locator(`#shiny-tab-${tab}.active`);await root.waitFor();
    const panel=root.locator('.fm-information-heading > .fm-info-panel').first();
    await panel.locator(':scope > summary').waitFor();
    assert.equal(await page.locator('.content .fm-info > summary:visible').count(),1,`${tab}: one page icon`);
    await panel.locator(':scope > summary').click();
    assert(await panel.evaluate(n=>n.open));
    assert((await panel.locator(':scope > .fm-info-body').innerText()).trim(),`${tab}: populated panel`);
    assert.equal(await panel.locator('.fm-info-source-label').count(),0,`${tab}: no redundant note titles`);
    if(tab==='lineup'){
     assert.equal(await coachCheckbox.isVisible(),true);
     await panel.locator(':scope > .fm-info-body').getByText('Disabled',{exact:true}).waitFor();
     await coachCheckbox.check();
     await page.waitForFunction(key=>localStorage.getItem(key)==='true',coachKey);
     await coachCheckbox.uncheck();
     await page.waitForFunction(key=>localStorage.getItem(key)==='false',coachKey);
    }
    assert(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth+2),`${tab}: no overflow at ${width}`);
    await panel.locator(':scope > summary').click();
   }
   assert.deepEqual(errors,[]);
   if(width===390){
    await page.locator('.sidebar-menu a[data-value="today"]').evaluate(n=>n.click());
    await page.locator('#shiny-tab-today.active').waitFor();
    await page.screenshot({path:'/tmp/futmondo-page-information-app-mobile.png',fullPage:true});
   }
   console.log(`PASS full app ${width}px: nine pages, one populated panel each, no overflow or browser errors`);
   await page.close();
  }
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
