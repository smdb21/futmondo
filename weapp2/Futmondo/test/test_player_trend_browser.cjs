const assert=require('node:assert/strict');
const path=require('node:path');
const {chromium}=require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules','playwright'));
(async()=>{
 const browser=await chromium.launch({headless:true,args:['--no-sandbox']});
 try{
  for(const width of [320,390,1440]){
   const page=await browser.newPage({viewport:{width,height:1000},colorScheme:'dark'});
   const errors=[];page.on('pageerror',error=>errors.push(error.message));
   await page.route('**/*',route=>route.request().url().startsWith('http://127.0.0.1:3921/')?route.continue():route.abort());
   await page.goto('http://127.0.0.1:3921/');
   const plot=page.locator('#chart');
   await page.waitForFunction(()=>document.querySelector('#chart')?._fullLayout&&document.querySelector('#neutral')?._fullLayout);
   const inspect=()=>plot.evaluate(el=>{
    const axis=name=>{const a=el._fullLayout[name];return a?{color:a.tickfont.color,title:a.title.font.color,visible:a.visible}:{visible:false};};
    const labels=Array.from(el.querySelectorAll('.xtick text')).map(text=>{const r=text.getBoundingClientRect();return {left:r.left,right:r.right,text:text.textContent};});
    return {labels,y:axis('yaxis'),y2:axis('yaxis2'),x2:axis('xaxis2'),
     data:el.data.map(trace=>({name:trace.name,color:trace.line.color,count:trace.y.length})),
     ticks:Array.from(el.querySelectorAll('.ytick text,.y2tick text')).map(text=>({axis:text.parentElement.getAttribute('class'),fill:getComputedStyle(text).fill})),
     titles:Array.from(el.querySelectorAll('.ytitle,.y2title')).map(text=>({axis:text.getAttribute('class'),fill:getComputedStyle(text).fill}))};
   });
   for(const scenario of ['both','value','points','undated']){
    await page.locator('#scenario').evaluate((el,value)=>el.selectize.setValue(value),scenario);
    await page.waitForFunction(s=>{const e=document.querySelector('#chart');return e?.data?.length===(['both','undated'].includes(s)?2:1)&&!!e._fullLayout.xaxis2?.visible===(s==='undated');},scenario);
    for(const theme of ['dark','light']){
     await page.evaluate(mode=>{localStorage.setItem('futmondo.color-theme',mode);window.dispatchEvent(new StorageEvent('storage',{key:'futmondo.color-theme',newValue:mode}));},theme);
     await page.waitForFunction(theme=>document.querySelector('#chart').layout.paper_bgcolor===(theme==='dark'?'#0B120D':'#FFFFFF'),theme);
     const state=await inspect();
     const labels=state.labels.sort((a,b)=>a.left-b.left);
     assert.ok(labels.length>0&&labels.length<=3,`${width}/${scenario}: sparse date ticks`);
     for(let i=1;i<labels.length;i++)assert.ok(labels[i-1].right+2<=labels[i].left,`${width}/${scenario}: dates overlap ${JSON.stringify(labels)}`);
     for(const trace of state.data){
      const axis=trace.name==='Valuation'?state.y:state.y2;
      assert.equal(axis.color.toLowerCase(),trace.color.toLowerCase());assert.equal(axis.title.toLowerCase(),trace.color.toLowerCase());
      assert.equal(trace.count,trace.name==='Valuation'?124:4);
     }
     for(const tick of state.ticks)assert.equal(tick.fill,tick.axis==='ytick'?'rgb(0, 255, 136)':'rgb(244, 201, 93)');
     for(const title of state.titles)assert.equal(title.fill,title.axis==='ytitle'?'rgb(0, 255, 136)':'rgb(244, 201, 93)');
     if(scenario==='undated'){assert.equal(state.x2.color,'#F4C95D');assert.equal(state.x2.title,'#F4C95D');}
     const neutral=await page.locator('#neutral').evaluate(el=>({color:el._fullLayout.yaxis.tickfont.color,muted:getComputedStyle(document.documentElement).getPropertyValue('--fm-muted').trim()}));
     assert.equal(neutral.color.toLowerCase(),neutral.muted.toLowerCase());
     assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+1),false);
     if(width===390&&scenario==='both')await page.screenshot({path:`/tmp/futmondo-trend-axis-${theme}.png`,fullPage:true});
    }
   }
   await page.locator('#scenario').evaluate(el=>el.selectize.setValue('both'));
   await page.waitForFunction(()=>document.querySelector('#chart').data.length===2&&!document.querySelector('#chart')._fullLayout.xaxis2?.visible);
   await plot.evaluate(el=>Plotly.relayout(el,{'xaxis.range':['2026-11-15','2027-01-15'],'xaxis.autorange':false}));
   const before=await plot.evaluate(el=>({data:JSON.stringify(el.data),range:el.layout.xaxis.range}));
   await page.evaluate(()=>{window.dispatchEvent(new StorageEvent('storage',{key:'futmondo.color-theme',newValue:'dark'}));});
   await page.waitForFunction(()=>document.querySelector('#chart').layout.paper_bgcolor==='#0B120D');
   const after=await plot.evaluate(el=>({data:JSON.stringify(el.data),range:el.layout.xaxis.range}));
   assert.deepEqual(after,before,'theme update preserves source data and zoom');
   const begin=Date.now();await page.locator('#heartbeat').click();await page.waitForFunction(()=>document.querySelector('#heartbeat_value').textContent==='1');
   const heartbeat=Date.now()-begin;assert.ok(heartbeat<1500);
   assert.deepEqual(errors,[]);assert.deepEqual(await page.locator('.shiny-output-error:visible').allTextContents(),[]);
   console.log(`PASS ${width}px: four history modes, both themes, nonoverlapping date ticks, series-colored ticks/titles, neutral theme defaults, source/zoom retention; heartbeat ${heartbeat}ms`);
   await page.close();
  }
 }finally{await browser.close();}
})().catch(error=>{console.error(error);process.exit(1);});
