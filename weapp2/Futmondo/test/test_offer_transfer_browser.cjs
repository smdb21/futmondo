const assert=require('node:assert/strict');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:process.env.FUTMONDO_TEST_CHROMIUM||undefined,args:['--no-sandbox']});
 try{
  for(const width of [320,360,390,1440]){
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
   const system=table.locator('.fm-offer-bidder[title="Liga Radar"]');
   assert.deepEqual((await system.allTextContents()).map(label=>label.trim()),['Liga Radar','Liga Radar']);
   const systemRow=system.first().locator('xpath=ancestor::*[@role="row"]');
   assert.match(await systemRow.innerText(),/25\.271\.066\s€.*-29\.64 M€/s);
   const layout=await table.evaluate(el=>{
    const rows=[...el.querySelectorAll('.rt-tbody [role="row"]')];
    const oneLine=[...el.querySelectorAll('.rt-th .rt-text-content, .rt-td-inner, .fm-offer-player-name, .fm-offer-bidder')]
     .every(node=>getComputedStyle(node).whiteSpace==='nowrap');
    const names=[...el.querySelectorAll('button.fm-offer-player-name')];
    const hasBorder=names.some(node=>parseFloat(getComputedStyle(node).borderTopWidth)>0);
    const metrics=[...el.querySelectorAll('.rt-align-right .rt-td-inner span[title]')]
     .filter(node=>/\d/.test(node.textContent));
    const clipped=[...metrics.map(node=>node.parentElement),...el.querySelectorAll('.fm-offer-bidder[title="Liga Radar"]')]
     .filter(node=>node.scrollWidth>node.clientWidth+1).map(node=>node.textContent.trim());
    const rival=el.querySelector('.fm-offer-rival.fm-offer-bidder');
    return {oneLine,hasBorder,clipped,rivalOverflow:rival.scrollWidth>rival.clientWidth,
     rivalEllipsis:getComputedStyle(rival).textOverflow==='ellipsis',
     rowHeight:Math.max(...rows.map(row=>row.getBoundingClientRect().height))};
   });
   assert.equal(layout.oneLine,true,`${width}px: all cells and headers stay on one line`);
   assert.equal(layout.hasBorder,false,`${width}px: player buttons have no border`);
   assert.deepEqual(layout.clipped,[],`${width}px: amounts, profit and Liga Radar fit their columns`);
   assert.ok(layout.rowHeight<=34,`${width}px: compact rows are ${layout.rowHeight}px tall`);
   if(width<400)assert.ok(layout.rivalOverflow&&layout.rivalEllipsis);
   const player=first.getByRole('button',{name:'Marco Asensio'});
   assert.equal(await player.getAttribute('title'),'Marco Asensio');
   assert.equal(await table.locator('[title="P/L unavailable"]').count(),1);
   const scrollWidth=await table.locator('.rt-table').evaluate(node=>node.scrollWidth-node.clientWidth);
   if(width<=360)assert.ok(scrollWidth>0);
   else assert.ok(scrollWidth<=1);
   await table.getByRole('columnheader',{name:'From'}).focus();
   await page.keyboard.press('Tab');
   assert.equal(await player.evaluate(node=>document.activeElement===node&&getComputedStyle(node).outlineStyle==='solid'),true);
   await player.press('Enter');
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

   const rival=table.getByText('Rival · Rival with a long <club> name',{exact:true});
   await rival.waitFor();
   assert.equal(await rival.getAttribute('title'),'Rival · Rival with a long <club> name');
   assert.equal(await table.locator('club').count(),0);
   assert.ok(!(await page.locator('#offers').innerText()).includes('could not be verified'));
   for(const id of ['incoming','outgoing']){
    const panel=page.locator(`#${id}`);
    const groups=panel.locator('.fm-transfer-group');
    assert.deepEqual(await groups.getByRole('heading').allTextContents(),['Before round starts','At round start','After round starts']);
    const suffix=id==='incoming'?'in':'out';
    for(const [index,players] of [[0,[suffix,`second-${suffix}`]],[1,[`kickoff-${suffix}`]],[2,[`late-${suffix}`]]]){
     assert.deepEqual(await groups.nth(index).getByRole('link').evaluateAll(links=>links.map(link=>link.id)),players.map(player=>`${id}_open_${player}`));
    }
    const row=groups.first().locator('.fm-incoming-player').first();
    assert.equal(await row.locator('.fm-info').count(),1);
    const time=row.locator('.fm-transfer-time > strong');
    assert.match(await time.innerText(),/^(Joins|Leaves): .* (CET|CEST)$/);
    const timeBox=await time.boundingBox();
    const trigger=row.locator('.fm-info > summary');const icon=await trigger.boundingBox();
    assert.ok(icon.x>=timeBox.x+timeBox.width-1,`${width}px ${id}: icon must sit to the right of the transfer time`);
    assert.ok(Math.abs((icon.y+icon.height/2)-(timeBox.y+timeBox.height/2))<3,`${width}px ${id}: icon must share the line`);
    await trigger.click();assert.equal(await row.locator('.fm-info').getAttribute('open'),'');
    await row.getByText(/Observed:/).waitFor({state:'visible'});
    const after=await trigger.boundingBox();assert.ok(Math.abs(after.y-icon.y)<2,'expanded info must keep the icon beside the transfer time');
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
   console.log(`PASS ${width}px: roster timestamps appear once in the top note on all three pages, sortable offer/profit, single-line borderless rows (${layout.rowHeight}px), full numeric values and Liga Radar, crests, exact keyboard action, safe bidder text and saved status, incoming/leaving icons stay beside timing labels open/closed, keyboard works, no overflow, timestamp advances without panel replacement or I/O; heartbeat ${Date.now()-started}ms`);
   await page.close();
  }
 }finally{await browser.close();}
})().catch(e=>{console.error(e.stack);process.exitCode=1});
