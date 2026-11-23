const assert=require('assert/strict');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
(async()=>{
 const browser=await chromium.launch({headless:true,args:['--no-sandbox']});
 try{
  for(const width of [360,390,1440]){
   const page=await browser.newPage({viewport:{width,height:900}});
   await page.goto('http://127.0.0.1:3914',{waitUntil:'networkidle'});
   const schedule=page.locator('#card-schedule_clause');await schedule.waitFor({state:'visible',timeout:20000});
   if(width===360){
    await page.locator('#card-test_locked_clause').click();
    await page.locator('#card-confirm_clause_button').click();
    await page.locator('#finish_blocked').dispatchEvent('click');
    const blocked=page.locator('.shiny-notification').filter({hasText:'Locked-clause test stopped before sending a request to Futmondo.'});
    await blocked.waitFor();
    assert.match(await blocked.textContent(),/Amount exceeds verified spendable funds/);
    assert.equal(await page.getByText('Locked-clause diagnostic response:',{exact:false}).count(),0);
    await blocked.locator('.shiny-notification-close').click();
    console.log('PASS the locked-clause diagnostic displays the local funds block at 360px');
   }
   await schedule.click();await page.getByText('This schedules a purchase attempt. Success is not guaranteed. The purchase may leave your balance negative.',{exact:true}).waitFor();
   await page.waitForFunction(()=>getComputedStyle(document.querySelector('#shiny-modal')).opacity==='1');
   await page.locator('.modal-title').scrollIntoViewIfNeeded();
   await page.screenshot({path:`/tmp/futmondo-scheduled-clause-modal-${width}.png`,fullPage:false});
   assert.equal(await page.locator('#card-schedule_clause_consent').isChecked(),false);
   await page.locator('#card-confirm_scheduled_clause').click();
   assert.equal(await page.locator('#shiny-modal').isVisible(),true);
   assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth+1));
   await page.locator('#card-schedule_clause_consent').check();await page.locator('#card-confirm_scheduled_clause').click();
   await page.locator('#shiny-modal').waitFor({state:'hidden'});
   const started=Date.now();await page.locator('#heartbeat').click();
   await page.waitForFunction(()=>document.querySelector('#beats').textContent.trim()==='1',null,{timeout:2500});
   console.log(`MEASURE width=${width} held_verification_heartbeat_ms=${Date.now()-started}`);
   await page.locator('#finish').click();
   await page.locator('#card-cancel_scheduled_clause').waitFor({state:'visible',timeout:10000}).catch(async error=>{console.log('Fixture notifications:',await page.locator('.shiny-notification').allTextContents());console.log('Fixture state:',await page.locator('#fixture_state').textContent());console.log('Card actions:',await page.locator('#card-action_buttons').textContent());throw error});
   const pending=page.locator('#pending details');await pending.locator('summary').click();
   await page.getByText('Cancel all scheduled purchases and revoke access',{exact:true}).waitFor();
   assert.ok((await pending.textContent()).includes('2066'));
   assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth+1));
   await page.screenshot({path:`/tmp/futmondo-scheduled-clause-pending-${width}.png`,fullPage:true});
   await page.locator('#card-cancel_scheduled_clause').click();
   await page.locator('#clause_results').getByText('Scheduled purchase results',{exact:true}).waitFor();
   await page.locator('#logout').click();
   await page.locator('#clause_results').getByText('Scheduled purchase results',{exact:true}).waitFor({state:'hidden'});
   await page.locator('#login').click();
   await page.locator('#clause_results').getByText('Scheduled purchase results',{exact:true}).waitFor();
   assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth+1));
   await page.locator('#clause_results').getByRole('button',{name:'Acknowledge',exact:true}).click();
   await page.locator('#clause_results').getByText('Scheduled purchase results',{exact:true}).waitFor({state:'hidden'});
   await page.screenshot({path:`/tmp/futmondo-scheduled-clause-${width}.png`,fullPage:true});
   console.log(`PASS actual scheduling consent, held-request navigation, pending date, cancellation, next-login outcome and no overflow at ${width}px`);
   await page.close();
  }
 }finally{await browser.close()}
})().catch(error=>{console.error(error);process.exit(1)});
