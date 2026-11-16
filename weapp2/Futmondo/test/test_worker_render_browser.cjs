const assert=require('node:assert/strict');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:process.env.FUTMONDO_TEST_CHROMIUM||undefined,args:['--no-sandbox']});
 try{
  for(const width of [320,390,1440]){
   const page=await browser.newPage({viewport:{width,height:900}});
   const errors=[];page.on('pageerror',e=>errors.push(e.message));
   await page.route('**/*',r=>new URL(r.request().url()).hostname==='127.0.0.1'?r.continue():r.abort());
   await page.goto('http://127.0.0.1:3898');
   await page.locator('#today-kpi_cash_box').getByText('Current balance',{exact:true}).waitFor();
   await page.waitForFunction(()=>document.querySelector('#today-kpi_cash_box').textContent.includes('20.000.001'));
   await page.locator('#today-recent_transfers').getByText('No completed transfers available.',{exact:true}).waitFor();
   await page.locator('#today-market_players-players_table .ReactTable').waitFor();
   await page.waitForTimeout(1000);
   await page.evaluate(()=>{
    window.renderEvents=[];
    const ids=['worker_status','today-kpi_cash_box','today-kpi_valuation_box','today-recent_transfers','account_finance_summary','round_finance_amounts','round_countdown','today-market_players-players_table'];
    $(document).on('shiny:recalculating.fixture shiny:value.fixture',e=>{
     if(ids.includes(e.target.id))window.renderEvents.push({type:e.type,id:e.target.id});
    });
   });
   const table=page.locator('#today-market_players-players_table .ReactTable');
   await table.evaluate(el=>el.dataset.fixtureIdentity='kept');
   const headerColor=await page.locator('.fm-account-finance').evaluate(el=>getComputedStyle(el).color);
   const checkedBefore=await page.locator('#worker_status_checked_at').textContent();
   const financeCheckedBefore=await page.locator('#round_finance_checked').textContent();
   const countdownNode=await page.locator('.fm-browser-countdown').evaluate(el=>{el.dataset.fixtureIdentity='kept';return el.textContent;});
   // Two real polls deliver NEW versions/source times with unchanged facts.
   await page.waitForTimeout(11000);
   assert.notEqual(await page.locator('#worker_status_checked_at').textContent(),checkedBefore,'source-check text must stay current');
   assert.notEqual(await page.locator('#round_finance_checked').textContent(),financeCheckedBefore,'financial source-check text must stay current');
   assert.equal(await page.locator('.fm-browser-countdown').getAttribute('data-fixture-identity'),'kept','clock node replaced');
   assert.notEqual(await page.locator('.fm-browser-countdown').innerText(),countdownNode,'browser clock must keep ticking');
   assert.deepEqual(await page.evaluate(()=>renderEvents),[],`${width}px metadata-only redraws`);
   for(const button of ['expire','restore','expire','restore']){
    await page.locator('#'+button).click();
    await page.waitForFunction(expired=>document.querySelector('#round_finance_status').textContent.includes('out of date')===expired,button==='expire');
    assert.equal(await page.locator('.fm-account-finance').evaluate(el=>getComputedStyle(el).color),headerColor);
    assert.equal(await page.locator('.fm-browser-countdown').getAttribute('data-fixture-identity'),'kept');
    assert.equal(await table.getAttribute('data-fixture-identity'),'kept');
   }
   assert.deepEqual(await page.evaluate(()=>renderEvents),[],`${width}px expiry/recovery must leave unchanged amounts and players mounted`);
   await page.locator('#refresh_saved').click();
   const holdStart=Date.now();await page.locator('#heartbeat').click();
   await page.waitForFunction(()=>document.querySelector('#heartbeat_value').textContent==='1');
   const heldMs=Date.now()-holdStart;assert.ok(heldMs<500,`held database heartbeat ${heldMs}ms`);
   await page.waitForTimeout(1100);
   assert.equal(await page.locator('.fm-browser-countdown').getAttribute('data-fixture-identity'),'kept');
   assert.equal(await table.getAttribute('data-fixture-identity'),'kept');
   assert.deepEqual(await page.evaluate(()=>renderEvents),[],`${width}px unchanged delayed refresh must not repaint`);

   await page.locator('#publish').click();
   await page.waitForFunction(()=>document.querySelector('#today-kpi_cash_box').textContent.includes('20.000.002'));
   await page.waitForTimeout(300);
   const events=await page.evaluate(()=>renderEvents);
   assert.equal(events.filter(e=>e.id==='today-kpi_cash_box'&&e.type==='shiny:value').length,1,JSON.stringify(events));
   assert.equal(events.filter(e=>e.id==='today-kpi_cash_box'&&e.type==='shiny:recalculating').length,1,JSON.stringify(events));
   await page.locator('#publish_player').click();
   await page.waitForFunction(()=>document.querySelector('#today-market_players-players_table').textContent.includes('3.000.000'));
   await page.evaluate(()=>{window.fixtureStats=null;Shiny.addCustomMessageHandler('fixture-stats',x=>window.fixtureStats=x);Shiny.setInputValue('fixture_stats',Math.random(),{priority:'event'});});
   await page.waitForFunction(()=>window.fixtureStats!==null);
   assert.equal((await page.evaluate(()=>fixtureStats)).enqueues,0,'read-only display must never enqueue acquisition');
   const start=Date.now();await page.locator('#heartbeat').click();
   await page.waitForFunction(()=>document.querySelector('#heartbeat_value').textContent==='2');
   const ms=Date.now()-start;assert.ok(ms<500,`heartbeat ${ms}ms`);
   assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false);
   assert.deepEqual(errors,[]);
   console.log(`PASS ${width}px: unchanged source versions, expiry/recovery and delayed refresh kept header color/DOM and player table stable; source clocks advanced; actual balance and player changes painted; zero acquisition jobs; held database heartbeat ${heldMs}ms; heartbeat ${ms}ms; no horizontal overflow`);
   await page.close();
  }
 }finally{await browser.close();}
})().catch(e=>{console.error(e.stack);process.exitCode=1});
