const assert=require('node:assert/strict');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules') + '/playwright');
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:process.env.FUTMONDO_TEST_CHROMIUM||undefined,args:['--no-sandbox']});
 try {
  for(const width of [320,390,1440])for(const theme of ['light','dark']){
   const page=await browser.newPage({viewport:{width,height:1000},colorScheme:theme});
   const errors=[];page.on('pageerror',e=>errors.push(e.message));
   await page.route('**/*',route=>new URL(route.request().url()).hostname==='127.0.0.1'?route.continue():
    /fixture-crest.png$/.test(route.request().url())?route.fulfill({contentType:'image/svg+xml',body:'<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24"><circle cx="12" cy="12" r="11" fill="orange"/></svg>'}):route.abort());
   await page.goto('http://127.0.0.1:8569');
   const plot=id=>page.locator('#plots-'+id);
   const wait=id=>page.waitForFunction(id=>document.querySelector('#plots-'+id)?.data?.length>0,id);
   await wait('standings_plot');
   const rounds=await plot('standings_plot').evaluate(el=>({type:el.layout.xaxis.type,x:[...el.data[0].x],y:[...el.data[0].y]}));
   assert.equal(rounds.type,'category');assert.deepEqual(rounds.x,['6','6.2','7']);assert.deepEqual(rounds.y,[10,15,12]);
   assert.equal(await page.locator('[id="plots-rank_round"]').count(),0);
   const from=page.getByLabel('From round',{exact:true});
   const through=page.getByLabel('Through round',{exact:true});
   await from.selectOption('6.2');
   await through.selectOption('7');
   const waitRounds=(x,y)=>page.waitForFunction(({x,y})=>{
    const line=document.querySelector('#plots-standings_plot')?.data?.[0];
    return line && JSON.stringify(line.x)===JSON.stringify(x) && JSON.stringify(line.y)===JSON.stringify(y);
   },{x,y});
   await waitRounds(['6.2','7'],[15,12]);
   assert.deepEqual(await through.locator('option').evaluateAll(opts=>opts.map(o=>o.value)),['latest','6','6.2','7']);
   await through.selectOption('6.2');
   await waitRounds(['6.2'],[15]);
   assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+2),false);
   await through.selectOption('6');
   await page.getByText('Choose an end round at or after the start on the chart.',{exact:true}).waitFor();
   await from.selectOption('first');
   await through.selectOption('latest');
   await waitRounds(rounds.x,rounds.y);
   for(const [tab,id] of [['Squad values','values_plot'],['Cash & transfers','cash_plot'],['Rank progression','rank_plot'],['Owned by real team','owned_club_plot']]){
    await page.getByRole('tab',{name:tab,exact:true}).click();await wait(id);
    assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+2),false,`${tab} overflow at ${width}`);
    if(id==='values_plot')assert.equal(await plot(id).evaluate(el=>el.layout.xaxis.tickformat),'%b-%d');
    if(id==='cash_plot'){
     assert.equal(await plot(id).evaluate(el=>el.data[0].marker.color),'#608BC1');
     const rects=await page.evaluate(()=>['plots-cash_metric','plots-cash_dates'].map(id=>document.getElementById(id).closest('[class*="col-sm-"]').getBoundingClientRect().width));
     if(width===1440)assert.ok(rects[0]>rects[1]);
    }
    if(id==='owned_club_plot'){
     const data=await plot(id).evaluate(el=>({color:el.data[0].marker.color,labels:[...el.data[0].text],images:el.layout.images.length,top:el.layout.margin.t}));
     assert.equal(data.color,'#608BC1');assert.deepEqual(data.labels,[1,2,3]);assert.equal(data.images,3);assert.ok(data.top>=60);
     await page.waitForFunction(()=>{
      const images=[...document.querySelectorAll('#plots-owned_club_plot .imagelayer image')];
      return images.length===3&&images.every(img=>img.getBoundingClientRect().width>=21);
     });
     const toolbar=await page.locator('#plots-owned_club_plot .modebar').boundingBox();
     const tabs=await page.locator('.nav-pills').boundingBox();assert.ok(toolbar.y>=tabs.y+tabs.height);
     await page.locator('#plots-owned_club_plot').screenshot({path:`/tmp/plots-owned-${width}-${theme}.png`});
    }
   }
   await page.getByRole('button',{name:'Measure saved reads'}).click();await page.waitForTimeout(200);
   const before=await page.locator('#counts').innerText();
   await page.getByRole('tab',{name:'Rank progression',exact:true}).click();
   await page.evaluate(()=>Shiny.setInputValue('plots-rank_range',[6.2,7],{priority:'event'}));
   await page.waitForFunction(()=>document.querySelector('#plots-rank_plot').data[0].x.length===2);
   await page.getByRole('button',{name:'Measure saved reads'}).click();await page.waitForTimeout(200);
   assert.equal(await page.locator('#counts').innerText(),before,'Range changes reuse saved reads');
   assert.deepEqual(errors,[]);console.log(`PASS ${width}px ${theme}: all plots, subrounds, toolbar, crests, colors, no overflow or extra range reads`);
   await page.close();
  }
 }finally{await browser.close()}
})().catch(e=>{console.error(e);process.exitCode=1});
