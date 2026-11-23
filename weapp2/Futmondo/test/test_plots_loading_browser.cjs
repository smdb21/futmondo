const assert=require('node:assert/strict');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:process.env.FUTMONDO_TEST_CHROMIUM||undefined,args:['--no-sandbox']});
 try{for(const width of [390,1440]){
  const page=await browser.newPage({viewport:{width,height:1000}});
  const errors=[];page.on('pageerror',e=>errors.push(e.message));
  await page.addInitScript(()=>{
   window.plotPublications=0;
   document.addEventListener('DOMContentLoaded',()=>window.jQuery(document).on('shiny:value',e=>{
    if(e.target.id==='plots-standings_plot')window.plotPublications++;
   }));
  });
  await page.route('**/*',route=>new URL(route.request().url()).hostname==='127.0.0.1'?route.continue():
   /fixture-crest.png$/.test(route.request().url())?route.fulfill({contentType:'image/svg+xml',body:'<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24"><circle cx="12" cy="12" r="11" fill="orange"/></svg>'}):route.abort());
  await page.goto('http://127.0.0.1:8571');
  await page.waitForFunction(()=>document.getElementById('plots-standings_plot').data?.filter(x=>x.y?.some(Number.isFinite)).length===9);
  assert.equal(await page.locator('.fm-plot-loading:visible').count(),1);
  const heartbeat=async n=>{
   const started=Date.now();await page.evaluate(n=>Shiny.setInputValue('pulse',n,{priority:'event'}),n);
   await page.waitForFunction(n=>document.getElementById('heartbeat').textContent===String(n),n);
   const elapsed=Date.now()-started;assert.ok(elapsed<1500,`Heartbeat took ${elapsed}ms`);return elapsed;
  };
  const timings=[await heartbeat(1)];
  await page.getByRole('tab',{name:'Squad values',exact:true}).click();
  await page.waitForFunction(()=>document.getElementById('plots-values_plot').data?.length===10);
  timings.push(await heartbeat(2));
  await page.getByRole('tab',{name:'Standings',exact:true}).click();
  await page.getByRole('button',{name:'Release held history',exact:true}).click();
  await page.waitForFunction(()=>document.getElementById('plots-standings_plot').data?.filter(x=>x.y?.some(Number.isFinite)).length===10);
  await page.waitForFunction(()=>![...document.querySelectorAll('.fm-plot-loading')].some(e=>e.getClientRects().length));
  const series=await page.locator('#plots-standings_plot').evaluate(e=>e.data.find(x=>x.name==='Team 1'));
  assert.deepEqual(series.y,[1,3,6,10,15,21,28,36,45,55,66,78,91,105]);
  await page.waitForTimeout(250);
  const before=await page.evaluate(()=>window.plotPublications);assert.ok(before>0,'Chart publication counter is active');
  for(let i=0;i<3;i++){
   await page.getByRole('button',{name:'Publish same saved facts',exact:true}).click();
   await page.waitForTimeout(350);timings.push(await heartbeat(3+i));
  }
  assert.equal(await page.evaluate(()=>window.plotPublications),before,'Unchanged facts must not republish the chart');
  await page.getByRole('tab',{name:'Owned by real team',exact:true}).click();
  await page.waitForFunction(()=>document.querySelectorAll('#plots-owned_club_plot .imagelayer image').length===20);
  timings.push(await heartbeat(6));
  const cdp=await page.context().newCDPSession(page);await cdp.send('Performance.enable');
  const metrics=async()=>Object.fromEntries((await cdp.send('Performance.getMetrics')).metrics.map(m=>[m.name,m.value]));
  const first=await metrics();await page.waitForTimeout(1200);const last=await metrics();
  assert.ok(last.TaskDuration-first.TaskDuration<.5,'Idle chart must not loop on redraw');
  assert.ok(Math.abs(last.Nodes-first.Nodes)<100,'Idle chart DOM must stay bounded');
  assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+2),false);
  assert.match(await page.locator('#stats').innerText(),/ 0$/,'No source HTTP');
  assert.deepEqual(errors,[]);
  console.log(`PASS ${width}px: partial then complete saved history, status recovery, no unchanged redraws, 20 crests; heartbeats ${timings.join('/')}ms; idle CPU ${((last.TaskDuration-first.TaskDuration)*1000).toFixed(1)}ms`);
  await page.screenshot({path:`/tmp/plots-loading-recovery-${width}.png`});await page.close();
 }}finally{await browser.close()}
})().catch(e=>{console.error(e);process.exitCode=1});
