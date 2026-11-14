const assert=require('node:assert/strict');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:process.env.FUTMONDO_TEST_CHROMIUM||undefined,args:['--no-sandbox']});
 try{
  for(const width of [360,1440]){
   const page=await browser.newPage({viewport:{width,height:900}});
   const errors=[];page.on('pageerror',e=>errors.push(e.message));
   await page.route('**/*',r=>new URL(r.request().url()).hostname==='127.0.0.1'?r.continue():r.abort());
   await page.goto('http://127.0.0.1:3898');
   await page.locator('#today-kpi_cash_box').getByText('Current balance',{exact:true}).waitFor();
   await page.waitForFunction(()=>document.querySelector('#today-kpi_cash_box').textContent.includes('20.000.001'));
   await page.locator('#today-recent_transfers').getByText('No completed transfers available.',{exact:true}).waitFor();
   await page.waitForTimeout(1000);
   await page.evaluate(()=>{
    window.renderEvents=[];
    const ids=['worker_status','today-kpi_cash_box','today-kpi_valuation_box','today-recent_transfers','round_countdown'];
    $(document).on('shiny:recalculating.fixture shiny:value.fixture',e=>{
     if(ids.includes(e.target.id))window.renderEvents.push({type:e.type,id:e.target.id});
    });
   });
   const checkedBefore=await page.locator('#worker_status_checked_at').innerText();
   const countdownNode=await page.locator('.fm-browser-countdown').evaluate(el=>{el.dataset.fixtureIdentity='kept';return el.textContent;});
   // Two real polls deliver NEW versions/source times with unchanged facts.
   await page.waitForTimeout(11000);
   assert.notEqual(await page.locator('#worker_status_checked_at').innerText(),checkedBefore,'source-check text must stay current');
   assert.equal(await page.locator('.fm-browser-countdown').getAttribute('data-fixture-identity'),'kept','clock node replaced');
   assert.notEqual(await page.locator('.fm-browser-countdown').innerText(),countdownNode,'browser clock must keep ticking');
   assert.deepEqual(await page.evaluate(()=>renderEvents),[],`${width}px metadata-only redraws`);
   await page.locator('#publish').click();
   await page.waitForFunction(()=>document.querySelector('#today-kpi_cash_box').textContent.includes('20.000.002'));
   await page.waitForTimeout(300);
   const events=await page.evaluate(()=>renderEvents);
   assert.equal(events.filter(e=>e.id==='today-kpi_cash_box'&&e.type==='shiny:value').length,1,JSON.stringify(events));
   assert.equal(events.filter(e=>e.id==='today-kpi_cash_box'&&e.type==='shiny:recalculating').length,1,JSON.stringify(events));
   await page.locator('#refresh_saved').click();
   await page.evaluate(()=>{window.fixtureStats=null;Shiny.addCustomMessageHandler('fixture-stats',x=>window.fixtureStats=x);Shiny.setInputValue('fixture_stats',Math.random(),{priority:'event'});});
   await page.waitForFunction(()=>window.fixtureStats!==null);
   assert.equal((await page.evaluate(()=>fixtureStats)).enqueues,0,'read-only display must never enqueue acquisition');
   const start=Date.now();await page.locator('#heartbeat').click();
   await page.waitForFunction(()=>document.querySelector('#heartbeat_value').textContent==='1');
   const ms=Date.now()-start;assert.ok(ms<500,`heartbeat ${ms}ms`);
   assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false);
   assert.deepEqual(errors,[]);
   console.log(`PASS ${width}px: new snapshot versions/timestamps caused zero banner/card/header/transfer-widget redraws; saved-check text advanced; new balance painted once; refresh queued zero acquisitions; heartbeat ${ms}ms; no horizontal overflow`);
   await page.close();
  }
 }finally{await browser.close();}
})().catch(e=>{console.error(e.stack);process.exitCode=1});
