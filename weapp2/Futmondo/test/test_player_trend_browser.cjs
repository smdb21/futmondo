const assert=require('node:assert/strict');
const path=require('node:path');
const {chromium}=require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules','playwright'));
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:process.env.FUTMONDO_TEST_CHROMIUM||undefined,args:['--no-sandbox']});
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
    const labels=Array.from(el.querySelectorAll('.xtick text')).map(text=>{const r=text.getBoundingClientRect();return {left:r.left,right:r.right,top:r.top,bottom:r.bottom,text:text.textContent,size:getComputedStyle(text).fontSize};});
    const bounds=el.getBoundingClientRect();const title=el.querySelector('.xtitle')?.getBoundingClientRect();
    return {bounds:{left:bounds.left,right:bounds.right,bottom:bounds.bottom},titleTop:title?.top,labels,tickangle:el._fullLayout.xaxis.tickangle,y:axis('yaxis'),y2:axis('yaxis2'),x2:axis('xaxis2'),
     data:el.data.map(trace=>({name:trace.name,color:trace.line.color,count:trace.y.length})),
     ticks:Array.from(el.querySelectorAll('.ytick text,.y2tick text')).map(text=>({axis:text.parentElement.getAttribute('class'),fill:getComputedStyle(text).fill})),
     titles:Array.from(el.querySelectorAll('.ytitle,.y2title')).map(text=>({axis:text.getAttribute('class'),fill:getComputedStyle(text).fill}))};
   });
   for(const scenario of ['both','value','points','undated']){
    await page.locator('#scenario').evaluate((el,value)=>el.selectize.setValue(value),scenario);
    await page.waitForFunction(s=>{const e=document.querySelector('#chart');const names=e?.data?.map(x=>x.name).sort().join(',');return names===(s==='value'?'Valuation':s==='points'?'Points':'Points,Valuation')&&!!e._fullLayout.xaxis2?.visible===(s==='undated');},scenario);
    for(const theme of ['dark','light']){
     await page.evaluate(mode=>{localStorage.setItem('futmondo.color-theme',mode);window.dispatchEvent(new StorageEvent('storage',{key:'futmondo.color-theme',newValue:mode}));},theme);
     await page.waitForFunction(theme=>document.querySelector('#chart').layout.paper_bgcolor===(theme==='dark'?'#0B120D':'#FFFFFF'),theme);
     await plot.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))));
     const state=await inspect();
     const labels=state.labels.sort((a,b)=>a.left-b.left);
     const calendarLabels=['01 Nov','15 Nov','01 Dec','15 Dec','01 Jan','15 Jan','01 Feb','15 Feb','01 Mar'];
     // A line-only date axis starts at 01 Nov 01:00 in Madrid; midnight is
     // just outside its range. Dated point markers add padding around it.
     assert.deepEqual(labels.map(label=>label.text),scenario==='points'?calendarLabels.slice(0,2):
       scenario==='both'?calendarLabels:calendarLabels.slice(1),`${width}/${scenario}: first and fifteenth calendar ticks`);
     assert.ok(labels.every(label=>label.size==='10px'),`${width}/${scenario}: smaller date labels`);
     assert.equal(state.tickangle,90,`${width}/${scenario}: vertical date labels`);
     assert.ok(labels.every(label=>/^\d{2} (Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)$/.test(label.text)),`${width}/${scenario}: abbreviated month labels without years`);
     assert.ok(labels.every(label=>label.left>=state.bounds.left-1&&label.right<=state.bounds.right+1&&label.bottom<=state.bounds.bottom+1),`${width}/${scenario}: date labels stay inside plot`);
     if(state.titleTop)assert.ok(labels.every(label=>label.bottom+1<=state.titleTop),`${width}/${scenario}: date labels do not overlap axis title`);
     for(let i=1;i<labels.length;i++)assert.ok(labels[i-1].right+2<=labels[i].left,`${width}/${scenario}: dates overlap ${JSON.stringify(labels)}`);
     for(const trace of state.data){
      const axis=trace.name==='Valuation'?state.y:state.y2;
      assert.equal(trace.color,trace.name==='Valuation'?(theme==='light'?'#15803D':'#00ff88'):'#F28C28');
      assert.equal(axis.color.toLowerCase(),trace.color.toLowerCase());assert.equal(axis.title.toLowerCase(),trace.color.toLowerCase());
      assert.equal(trace.count,trace.name==='Valuation'?124:4);
     }
     const valuationFill=theme==='light'?'rgb(21, 128, 61)':'rgb(0, 255, 136)';
     for(const tick of state.ticks)assert.equal(tick.fill,tick.axis==='ytick'?valuationFill:'rgb(242, 140, 40)');
     for(const title of state.titles)assert.equal(title.fill,title.axis==='ytitle'?valuationFill:'rgb(242, 140, 40)');
     if(scenario==='undated'){assert.equal(state.x2.color,'#F28C28');assert.equal(state.x2.title,'#F28C28');}
     const neutral=await page.locator('#neutral').evaluate(el=>({color:el._fullLayout.yaxis.tickfont.color,muted:getComputedStyle(document.documentElement).getPropertyValue('--fm-muted').trim()}));
     assert.equal(neutral.color.toLowerCase(),neutral.muted.toLowerCase());
     assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+1),false);
     if(width===390&&scenario==='both')await page.screenshot({path:`/tmp/futmondo-trend-axis-${theme}.png`,fullPage:true});
    }
   }
   await page.locator('#scenario').evaluate(el=>el.selectize.setValue('both'));
   await page.waitForFunction(()=>document.querySelector('#chart').data.length===2&&!document.querySelector('#chart')._fullLayout.xaxis2?.visible);
   await plot.evaluate(el=>Plotly.relayout(el,{'xaxis.range':['2026-11-15','2027-01-15'],'xaxis.autorange':false}));
   const sourceAndZoom=()=>plot.evaluate(el=>({data:JSON.stringify(el.data.map(trace=>({...trace,line:{...trace.line,color:undefined}}))),range:el.layout.xaxis.range}));
   const before=await sourceAndZoom();
   await page.evaluate(()=>{window.dispatchEvent(new StorageEvent('storage',{key:'futmondo.color-theme',newValue:'dark'}));});
   await page.waitForFunction(()=>document.querySelector('#chart').layout.paper_bgcolor==='#0B120D');
   const after=await sourceAndZoom();
   assert.deepEqual(after,before,'theme update preserves source data and zoom');
   const begin=Date.now();await page.locator('#heartbeat').click();await page.waitForFunction(()=>document.querySelector('#heartbeat_value').textContent==='1');
   const heartbeat=Date.now()-begin;assert.ok(heartbeat<1500);
   assert.deepEqual(errors,[]);assert.deepEqual(await page.locator('.shiny-output-error:visible').allTextContents(),[]);
   console.log(`PASS ${width}px: four history modes, both themes, small nonoverlapping first/fifteenth date ticks, series-colored ticks/titles, neutral theme defaults, source/zoom retention; heartbeat ${heartbeat}ms`);
   await page.close();
  }
 }finally{await browser.close();}
})().catch(error=>{console.error(error);process.exit(1);});
