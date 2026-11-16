const assert=require('node:assert/strict');
const {spawn}=require('node:child_process');
const net=require('node:net');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
const delay=ms=>new Promise(resolve=>setTimeout(resolve,ms));
(async()=>{
 let app,browser,log='';
 try{
  const port=await new Promise(resolve=>{const server=net.createServer();server.listen(0,'127.0.0.1',()=>{const p=server.address().port;server.close(()=>resolve(p))})});
  const url=`http://127.0.0.1:${port}`;
  app=spawn('Rscript',['test/test_worker_card_availability_browser_app.R'],{env:{...process.env,FUTMONDO_SELECTION_TEST_PORT:String(port)}});
  app.stdout.on('data',x=>log+=x);app.stderr.on('data',x=>log+=x);
  let ready=false;
  for(let i=0;i<150;i++){if(app.exitCode!==null)throw Error(log);try{if((await fetch(url)).ok){ready=true;break}}catch{}await delay(100)}
  assert(ready,'fixture startup');
  browser=await chromium.launch({headless:true,executablePath:process.env.FUTMONDO_TEST_CHROMIUM,args:['--no-sandbox']});
  for(const width of [390,1440]){
   const page=await browser.newPage({viewport:{width,height:960}});page.setDefaultTimeout(15000);
   const errors=[];page.on('pageerror',e=>errors.push(e.message));
   await page.route('**/*',r=>new URL(r.request().url()).hostname==='127.0.0.1'?r.continue():r.abort());
   await page.goto(url);
   const stats=async()=>{
    await page.evaluate(()=>{window.fixtureStats=null;Shiny.addCustomMessageHandler('fixture-stats',x=>window.fixtureStats=x);Shiny.setInputValue('fixture_stats',Math.random(),{priority:'event'});});
    await page.waitForFunction(()=>window.fixtureStats!==null);return page.evaluate(()=>fixtureStats);
   };
   const send=async name=>page.evaluate(key=>Shiny.setInputValue(key,Math.random(),{priority:'event'}),name);
   const offer=page.locator('#table-selected_player-btn_bid_market');
   const select=async text=>page.locator('#table-players_table .rt-td').filter({hasText:text}).click();
   await page.locator('#table-players_table .rt-td').filter({hasText:'Dani fixture'}).waitFor();
   const start=Date.now();await select('Dani fixture');await offer.waitFor();await page.waitForFunction(()=>document.querySelector('#table-selected_player-btn_bid_market')?.disabled===false);
   const elapsed=Date.now()-start;assert.equal((await stats()).financial,0);
   assert.equal(await page.locator('#table-selected_player-btn_verify_availability').count(),0);
   // Exercise a real 32-second pending source on the phone, not just a clock jump.
   if(width===390)await page.waitForTimeout(32000);
   else{await send('advance');await page.waitForTimeout(150)}
   assert.equal(await offer.isEnabled(),true);assert.equal((await stats()).financial,0);
   await page.evaluate(()=>Shiny.setInputValue('heartbeat',1,{priority:'event'}));
   const heart=Date.now();await page.waitForFunction(()=>document.querySelector('#heartbeat_value').textContent==='1');const heartbeat=Date.now()-heart;
   assert(heartbeat<750,`heartbeat ${heartbeat}ms`);
   await offer.click();
   const submit=page.locator('#table-selected_player-submit_bid');await submit.waitFor();
   assert.equal(await submit.isDisabled(),true);assert.equal((await stats()).financial,1);
   await send('finish_funds');await page.waitForFunction(()=>document.querySelector('#table-selected_player-submit_bid')?.disabled===false);
   assert.equal((await stats()).financial,1);
   await page.locator('.modal-footer').getByRole('button',{name:'Cancel',exact:true}).click();
   await page.locator('.modal-content').waitFor({state:'detached'});
   await select('Dani fixture');await offer.waitFor();assert.equal(await offer.isEnabled(),true);assert.equal((await stats()).financial,1);
   assert.equal(await page.locator('#table-selected_player-btn_verify_availability').count(),0);
   await page.locator('.modal-footer').getByRole('button',{name:'Close',exact:true}).click();
   await page.locator('#old_table').click();await select('Stale fixture');
   await page.locator('#table-selected_player-btn_verify_availability').waitFor();
   assert.equal((await stats()).financial,1);assert.equal((await stats()).summaries,2);
   await send('finish_summary');
   await page.waitForFunction(()=>document.querySelector('#table-selected_player-btn_offer_owner')?.disabled===false);
   assert((await page.locator('#table-selected_player-action_buttons').textContent()).includes('Verified owner'));
   assert.equal((await stats()).financial,1);assert.equal(await page.locator('#table-selected_player-btn_verify_availability').count(),0);
   assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth),false);assert.deepEqual(errors,[]);
   await page.screenshot({path:`/tmp/futmondo-target-selection-${width}.png`,fullPage:true});
   console.log(`PASS ${width}px actual table selection: ready ${elapsed}ms, heartbeat ${heartbeat}ms, held history adds zero finance reads; editor safely verifies; stale target refresh changes ownership; zero errors/overflow`);
   await page.close();
  }
 }catch(e){if(log.includes('Error'))process.stderr.write(log);throw e}
 finally{if(browser)await browser.close();if(app)app.kill('SIGTERM')}
})().catch(e=>{console.error(e.stack);process.exitCode=1});
