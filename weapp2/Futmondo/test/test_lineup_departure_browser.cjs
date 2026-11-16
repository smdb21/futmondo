const assert=require('node:assert/strict'),net=require('node:net');
const {spawn}=require('node:child_process');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
const delay=ms=>new Promise(resolve=>setTimeout(resolve,ms));
(async()=>{let app,browser,log='';try{
 const port=await new Promise(resolve=>{const server=net.createServer();server.listen(0,'127.0.0.1',()=>{const p=server.address().port;server.close(()=>resolve(p));});});
 app=spawn('Rscript',['test/test_lineup_departure_browser_app.R'],{env:{...process.env,FUTMONDO_DEPARTURE_TEST_PORT:String(port)},stdio:['ignore','pipe','pipe']});
 app.stdout.on('data',x=>log+=x);app.stderr.on('data',x=>log+=x);const url=`http://127.0.0.1:${port}`;
 let ready=false;for(let i=0;i<200;i++){if(app.exitCode!==null)throw Error(log);try{if((await fetch(url)).ok){ready=true;break;}}catch{}await delay(100);}assert(ready,log);
 browser=await chromium.launch({headless:true,executablePath:process.env.FUTMONDO_TEST_CHROMIUM||undefined,args:['--no-sandbox']});
 for(const width of [390,1440]){
  const p=await browser.newPage({viewport:{width,height:1000}});p.setDefaultTimeout(12000);
  const errors=[];p.on('pageerror',e=>errors.push(e.message));await p.route('**/*',r=>r.request().url().startsWith(url)?r.continue():r.abort());
  await p.goto(url);await p.waitForFunction(()=>document.querySelector('#diagnostics').textContent.includes('worker_reads'));
  const navigate=async page=>{await p.locator(`a[data-value="${page}"]`).click();};
  const complete=()=>p.waitForFunction(()=>JSON.parse(document.querySelector('#diagnostics').textContent).complete===true);
  const counts=async()=>JSON.parse(await p.locator('#diagnostics').textContent());
  for(const page of ['yourteam','lineup','today','lineup','yourteam','lineup']){
   await navigate(page);if(page==='today')continue;await complete();
   if(page==='yourteam')await p.waitForFunction(()=>document.querySelector('#departing').textContent.includes('Leaving players (0)'));
   if(page==='lineup'){
    await p.waitForFunction(()=>document.querySelectorAll('#lineup-pitch .fm-lineup-player').length===15);
    await p.waitForFunction(()=>!document.querySelector('#lineup-status').textContent.includes('Departure eligibility is not verified'));
    assert.doesNotMatch(await p.locator('[data-fm-info-page]').allTextContents().then(a=>a.join(' ')),/Roster is refreshing|Transaction history is refreshing/);
   }
  }
  await p.locator('#publish').click();await p.waitForFunction(()=>JSON.parse(document.querySelector('#diagnostics').textContent).source>=4);
  const before=await counts();assert.equal(before.blocked_io,0);assert.equal(before.submit,0);
  assert.equal(await p.locator('.fm-info-entry .fm-info-error').count(),0);
  await p.locator('#lineup-clear').click();await p.waitForFunction(()=>[...document.querySelectorAll('#lineup-pitch .fm-lineup-player')].every(n=>n.dataset.playerId===''));
  const draft=await p.locator('#lineup-pitch .fm-lineup-player').evaluateAll(a=>a.map(n=>n.dataset.playerId));
  await navigate('yourteam');await navigate('lineup');await complete();
  assert.deepEqual(await p.locator('#lineup-pitch .fm-lineup-player').evaluateAll(a=>a.map(n=>n.dataset.playerId)),draft);
  await p.locator('#publish').click();await complete();await p.waitForTimeout(150);
  let after=await counts();assert.equal(after.source,before.source);assert.equal(after.observed,before.observed);assert.equal(after.submit,0);
  await p.locator('#lineup-reload').click();await complete();await p.waitForTimeout(200);after=await counts();
  assert(after.worker_reads>before.worker_reads,'Lineup refresh reaches the shared DB readers');
  assert.equal(after.observed,before.observed,'Reusing observations preserves their source timestamp');
  assert.deepEqual(await p.locator('#lineup-pitch .fm-lineup-player').evaluateAll(a=>a.map(n=>n.dataset.playerId)),draft);
  const t=Date.now();await p.locator('#heartbeat').click();await p.waitForFunction(()=>document.querySelector('#heartbeat_value').textContent==='1');const heartbeat=Date.now()-t;
  assert.equal(await p.evaluate(()=>document.documentElement.scrollWidth>innerWidth+2),false);assert.deepEqual(errors,[]);
  await p.screenshot({path:`/tmp/futmondo-departure-handoff-${width}.png`,fullPage:true});
  console.log(`PASS ${width}px: confirmed zero shared across navigation, saved timestamps preserved, refresh/draft retained; heartbeat ${heartbeat}ms`);
  await p.close();
 }
}finally{if(browser)await browser.close();if(app)app.kill();}})().catch(e=>{console.error(e);process.exitCode=1;});
