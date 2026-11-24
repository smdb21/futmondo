const assert=require('node:assert/strict');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:process.env.FUTMONDO_TEST_CHROMIUM||undefined,args:['--no-sandbox']});
 try{for(const width of [390,1440])for(const theme of ['light','dark']){
  const page=await browser.newPage({viewport:{width,height:1000},colorScheme:theme});
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
  const axis = await page.locator('#plots-standings_plot').evaluate(e=>({
    ticks:[...e.querySelectorAll('.ytick text')].map(t=>({label:t.textContent,top:t.getBoundingClientRect().top,bottom:t.getBoundingClientRect().bottom})),
    grids:e.querySelectorAll('.ygrid').length,
    hasDtick:Object.prototype.hasOwnProperty.call(e.layout.yaxis,'dtick'),
    nticks:e.layout.yaxis.nticks,
    opacity:getComputedStyle(e).opacity
  }));
  assert.equal(axis.hasDtick,false,'Points axis must omit dtick, including an empty array');
  assert.equal(axis.nticks,6);
  assert.ok(axis.ticks.length>=2&&axis.ticks.length<=8,'Readable bounded points tick count');
  assert.ok(axis.grids<=8);
  const tickRects=axis.ticks.slice().sort((a,b)=>a.top-b.top);
  for(let i=1;i<tickRects.length;i++)assert.ok(tickRects[i].top>=tickRects[i-1].bottom+1,'Points labels must not overlap');
  assert.equal(axis.opacity,'1');
  const heartbeat=async n=>{
   const started=Date.now();await page.evaluate(n=>Shiny.setInputValue('pulse',n,{priority:'event'}),n);
   await page.waitForFunction(n=>document.getElementById('heartbeat').textContent===String(n),n);
   const elapsed=Date.now()-started;assert.ok(elapsed<1500,`Heartbeat took ${elapsed}ms`);return elapsed;
  };
  const timings=[await heartbeat(1)];
  const infoPanel=page.locator('.fm-info-panel');
  const roundWarning=infoPanel.getByText(/^Some saved round scores are missing\./);
  const checkRoundWarning=async () => {
   await infoPanel.waitFor({state:'visible'});
   await infoPanel.evaluate(panel=>{panel.open=true;});
   await roundWarning.waitFor();
   assert.equal(await roundWarning.count(),1);
  };
  await checkRoundWarning();
  await page.getByRole('tab',{name:'Rank progression',exact:true}).click();
  await page.waitForFunction(()=>document.getElementById('plots-rank_plot').data?.length>0);
  await checkRoundWarning();
  await page.getByRole('tab',{name:'Squad values',exact:true}).click();
  await page.waitForFunction(()=>document.getElementById('plots-values_plot').data?.length===10);
  timings.push(await heartbeat(2));
  await page.getByRole('tab',{name:'Money movements',exact:true}).click();
  await page.getByText('Loading saved transfer history.',{exact:true}).waitFor();
  await roundWarning.waitFor({state:'detached'});
  assert.equal(await page.getByText('League charts optimized for a single full-width view on phones.').count(),0);
  await page.locator('.fm-plot-loading').filter({hasText:'Loading saved round scores'}).waitFor({state:'detached'});
  timings.push(await heartbeat(3));
  await page.getByRole('button',{name:'Release held history',exact:true}).click();
  const movementPlot=page.locator('#plots-transfers_plot');
  const waitForAmounts=async amounts=>page.waitForFunction(amounts=>{
   const bars=document.getElementById('plots-transfers_plot').data?.[0];
   return bars?.type==='bar' && JSON.stringify(Array.from(bars.y))===JSON.stringify(amounts);
  },amounts);
  await waitForAmounts([300,300]);
  await page.getByText('Loading saved transfer history.',{exact:true}).waitFor({state:'hidden'});
  await heartbeat(4);
  const beforeGrouping=await page.locator('#stats').innerText();
  assert.match(beforeGrouping,/ 0 0$/,'Cold chart reads only the saved date-and-amount projection');
  for(const [period,amounts] of [['Day',[100,200,300]],['Month',[100,500]],['Week',[300,300]]]){
   await page.getByRole('radio',{name:period,exact:true}).check();
   await waitForAmounts(amounts);
  }
  await heartbeat(5);
  assert.equal(await page.locator('#stats').innerText(),beforeGrouping,'Period switches add no worker database reads');
  assert.equal(await movementPlot.evaluate(el=>el.layout.xaxis.type),'date');
  await page.getByRole('tab',{name:'Standings',exact:true}).click();
  await page.waitForFunction(()=>document.getElementById('plots-standings_plot').data?.filter(x=>x.y?.some(Number.isFinite)).length===10);
  await page.waitForFunction(()=>![...document.querySelectorAll('.fm-plot-loading')].some(e=>e.getClientRects().length));
  const series=await page.locator('#plots-standings_plot').evaluate(e=>e.data.find(x=>x.name==='Team 1'));
  assert.deepEqual(series.y,Array.from({length:14},(_,i)=>{const n=i+1;return 5*n*(n+1)+n;}));
  await page.locator('.fm-information-scope').screenshot({path:'/tmp/plots-points-axis-'+width+'-'+theme+'.png'});
  await page.waitForTimeout(250);
  const before=await page.evaluate(()=>window.plotPublications);assert.ok(before>0,'Chart publication counter is active');
  for(let i=0;i<3;i++){
   await page.getByRole('button',{name:'Publish same saved facts',exact:true}).click();
   await page.waitForTimeout(350);timings.push(await heartbeat(6+i));
  }
  assert.equal(await page.evaluate(()=>window.plotPublications),before,'Unchanged facts must not republish the chart');
  await page.getByRole('tab',{name:'Owned by real team',exact:true}).click();
  await page.waitForFunction(()=>document.querySelectorAll('#plots-owned_club_plot .imagelayer image').length===20);
  timings.push(await heartbeat(9));
  const cdp=await page.context().newCDPSession(page);await cdp.send('Performance.enable');
  const metrics=async()=>Object.fromEntries((await cdp.send('Performance.getMetrics')).metrics.map(m=>[m.name,m.value]));
  const first=await metrics();await page.waitForTimeout(1200);const last=await metrics();
  assert.ok(last.TaskDuration-first.TaskDuration<.5,'Idle chart must not loop on redraw');
  assert.ok(Math.abs(last.Nodes-first.Nodes)<100,'Idle chart DOM must stay bounded');
  assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+2),false);
  assert.match(await page.locator('#stats').innerText(),/ 0 0$/,'No full Pressroom read or source HTTP');
  assert.deepEqual(errors,[]);
  console.log(`PASS ${width}px ${theme}: partial then complete saved history, status recovery, three transfer groupings, no unchanged redraws, 20 crests; heartbeats ${timings.join('/')}ms; idle CPU ${((last.TaskDuration-first.TaskDuration)*1000).toFixed(1)}ms`);
  await page.screenshot({path:`/tmp/plots-loading-recovery-${width}.png`});await page.close();
 }}finally{await browser.close()}
})().catch(e=>{console.error(e);process.exitCode=1});
