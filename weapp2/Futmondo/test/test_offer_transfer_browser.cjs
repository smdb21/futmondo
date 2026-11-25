const assert=require('node:assert/strict');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:process.env.FUTMONDO_TEST_CHROMIUM||undefined,args:['--no-sandbox']});
 try{
  for(const width of [320,390,1440]){
   const page=await browser.newPage({viewport:{width,height:1100}});const errors=[];page.on('pageerror',e=>errors.push(e.message));
   await page.route('**/*',route=>{
    const url=new URL(route.request().url());
    if(url.hostname==='127.0.0.1')return route.continue();
    if(url.hostname==='static02.mondocore.com'&&url.pathname.endsWith('.png'))
     return route.fulfill({contentType:'image/svg+xml',body:'<svg xmlns="http://www.w3.org/2000/svg" width="20" height="20"/>'});
    return route.abort();
   });
   await page.goto('http://127.0.0.1:3892');
   await page.locator('#offers').getByText('Offers for your players (3 observed)',{exact:true}).waitFor();
   const table=page.locator('#received_offers_table');
   const playerNames=async()=>table.locator('.fm-offer-player button').allTextContents();
   await table.getByRole('columnheader',{name:'Profit'}).waitFor();
   assert.equal(await page.locator('#offers .fm-info-error').count(),0);
   await table.getByRole('columnheader',{name:'Offer'}).click();
   assert.deepEqual(await playerNames(),['B.Méndez','M.Asensio','Isco']);
   await table.getByRole('columnheader',{name:'Profit'}).click();
   assert.deepEqual(await playerNames(),['M.Asensio','B.Méndez','Isco']);
   const first=table.locator('.rt-tbody [role="row"]').first();
   assert.equal(await first.getByRole('img',{name:'Real Madrid'}).getAttribute('src').then(url=>new URL(url).pathname.split('/').pop()),'real-madrid.png');
   assert.match(await first.innerText(),/\+2\.50 M€/);
   assert.match(await table.locator('.rt-tbody').innerText(),/-1\.00 M€/);
   await first.getByRole('button',{name:'Marco Asensio'}).click();
   await page.locator('#opened_offer').getByText('rival-bid',{exact:true}).waitFor();
   await table.getByRole('columnheader',{name:'Profit'}).click();
   assert.deepEqual(await playerNames(),['B.Méndez','M.Asensio','Isco']);
   for(const tab of ['yourteam','today','players_in_championship']){
    await page.locator('#tabs').evaluate((el,value)=>el.selectize.setValue(value),tab);
    await page.waitForFunction(()=>document.querySelector('#worker_roster_source_times')?.textContent.includes('Roster observed:'));
    assert.equal(await page.getByText(/Roster observed:/).count(),1);
    assert.equal(await page.locator('#worker_status #worker_roster_source_times').count(),1);
    for(const id of ['incoming','outgoing'])assert.ok(!(await page.locator('#'+id).innerText()).includes('Roster observed:'));
   }
   await page.locator('#worker_status .fm-info > summary').click();
   await page.locator('#worker_roster_source_times').waitFor({state:'visible'});
   assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+2),false);
   const rosterStamp=await page.locator('#worker_roster_source_times').innerText();
   await page.locator('#worker_status > details').evaluate(el=>el.dataset.fixtureIdentity='preserved');

   await table.getByText('Rival · Rival with a long <club> name',{exact:true}).waitFor();
   assert.equal(await table.locator('club').count(),0);
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
   await page.waitForFunction(old=>document.querySelector('#worker_roster_source_times').textContent!==old,rosterStamp);
   assert.equal(await page.locator('#worker_status > details').getAttribute('data-fixture-identity'),'preserved');
   assert.equal(await page.locator('#offers .fm-offers-panel').getAttribute('data-fixture-identity'),'preserved');
   assert.deepEqual(await playerNames(),['B.Méndez','M.Asensio','Isco'],'Timestamp updates preserve sorted rows');
   const started=Date.now();await page.locator('#heartbeat').click();await page.waitForFunction(()=>document.querySelector('#beats').textContent==='1');
   assert.ok(Date.now()-started<500);assert.equal(await page.locator('#calls').innerText(),'0');assert.deepEqual(errors,[]);
   console.log(`PASS ${width}px: roster timestamps appear once in the top note on all three pages, sortable offer/profit, short names, crests, exact click, safe bidder text and saved status, incoming/leaving icons stay beside timing labels open/closed, keyboard works, no overflow, timestamp advances without panel replacement or I/O; heartbeat ${Date.now()-started}ms`);
   await page.close();
  }
 }finally{await browser.close();}
})().catch(e=>{console.error(e.stack);process.exitCode=1});
