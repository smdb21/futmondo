const assert=require('node:assert/strict');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
(async()=>{
 const browser=await chromium.launch({headless:true,args:['--no-sandbox']});
 try{
  for(const width of [320,390,1440]){
   const page=await browser.newPage({viewport:{width,height:1100}});const errors=[];page.on('pageerror',e=>errors.push(e.message));
   await page.route('**/*',r=>new URL(r.request().url()).hostname==='127.0.0.1'?r.continue():r.abort());
   await page.goto('http://127.0.0.1:3892');
   await page.locator('#offers').getByText('Offers for your players (1 observed)',{exact:true}).waitFor();
   assert.equal(await page.locator('#offers .fm-info-error').count(),0);
   assert.ok((await page.locator('#offers').innerText()).includes('Rival'));
   assert.ok(!(await page.locator('#offers').innerText()).includes('could not be verified'));
   for(const id of ['incoming','outgoing']){
    const row=page.locator(`#${id} .fm-incoming-round-line`);assert.equal(await row.locator('.fm-info').count(),1);
    const label=await row.locator('.fm-incoming-round').boundingBox();const trigger=row.locator('.fm-info > summary');const icon=await trigger.boundingBox();
    assert.ok(icon.x>=label.x+label.width-1,`${width}px ${id}: icon must sit to the label's right`);
    assert.ok(Math.abs((icon.y+icon.height/2)-(label.y+label.height/2))<3,`${width}px ${id}: icon must share the line`);
    await trigger.click();assert.equal(await row.locator('.fm-info').getAttribute('open'),'');
    await row.getByText(/Observed:/).waitFor({state:'visible'});
    const after=await trigger.boundingBox();assert.ok(Math.abs(after.y-icon.y)<2,'expanded info must keep icon beside label');
    assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+2),false);
    await trigger.press('Enter');assert.equal(await row.locator('.fm-info').getAttribute('open'),null);
   }
   const summary=page.locator('#offers .fm-info > summary');await summary.click();
   await page.locator('#offer_stamp').waitFor({state:'visible'});const stamp=await page.locator('#offer_stamp').innerText();
   await page.locator('#offers .fm-offers-panel').evaluate(el=>el.dataset.fixtureIdentity='preserved');
   await page.locator('#advance').click();await page.waitForFunction(old=>document.querySelector('#offer_stamp').textContent!==old,stamp);
   assert.equal(await page.locator('#offers .fm-offers-panel').getAttribute('data-fixture-identity'),'preserved');
   const started=Date.now();await page.locator('#heartbeat').click();await page.waitForFunction(()=>document.querySelector('#beats').textContent==='1');
   assert.ok(Date.now()-started<500);assert.equal(await page.locator('#calls').innerText(),'0');assert.deepEqual(errors,[]);
   console.log(`PASS ${width}px: successful saved offers have no false red error, incoming/leaving icons stay beside timing labels open/closed, keyboard works, no overflow, timestamp advances without panel replacement or I/O; heartbeat ${Date.now()-started}ms`);
   await page.close();
  }
 }finally{await browser.close();}
})().catch(e=>{console.error(e.stack);process.exitCode=1});
