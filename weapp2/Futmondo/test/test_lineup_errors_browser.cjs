#!/usr/bin/env node
// Self-starting synthetic browser fixture. Set FUTMONDO_LINEUP_COMPARE_BASELINE=true
// and FUTMONDO_LINEUP_BASELINE_ROOT to compare an archived pre-fix source tree.
const assert=require('node:assert/strict'),net=require('node:net');
const {spawn}=require('node:child_process');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
const delay=ms=>new Promise(r=>setTimeout(r,ms));
const freePort=()=>new Promise(r=>{const s=net.createServer();s.listen(0,'127.0.0.1',()=>{const p=s.address().port;s.close(()=>r(p));});});
async function launch(baseline){const port=await freePort(),url=`http://127.0.0.1:${port}`;let log='';
 const app=spawn('Rscript',['test/test_lineup_error_browser.R'],{env:{...process.env,FUTMONDO_LINEUP_ERROR_PORT:String(port),FUTMONDO_LINEUP_BASELINE:baseline?'true':'false'},stdio:['ignore','pipe','pipe']});
 app.stdout.on('data',x=>log+=x);app.stderr.on('data',x=>log+=x);
 for(let i=0;i<200;i++){if(app.exitCode!==null)throw Error(log);try{if((await fetch(url)).ok)return{app,url,log:()=>log};}catch{}await delay(100);}app.kill();throw Error('Fixture startup failed: '+log);}
(async()=>{let browser;const apps=[];try{browser=await chromium.launch({headless:true,args:['--no-sandbox']});
 if(process.env.FUTMONDO_LINEUP_COMPARE_BASELINE==='true'){
  const old=await launch(true);apps.push(old.app);const p=await browser.newPage({viewport:{width:390,height:900}});await p.goto(old.url);
  await p.waitForFunction(()=>document.querySelector('#lineup-status').textContent.includes('App copy not saved: context mismatch'));
  const text=await p.locator('#lineup-status').textContent();assert.match(text,/Round schedule observations are incomplete or invalid/);
  assert.equal(await p.locator('#lineup-status .fm-info-error').count(),2);
  console.log('PASS original source reproduces both screenshot errors');await p.close();old.app.kill();
  if(process.env.FUTMONDO_LINEUP_COMPARE_ONLY==='true')return;
 }
 const fixed=await launch(false);apps.push(fixed.app);
 for(const width of [390,1440])for(const theme of ['light','dark']){
  const p=await browser.newPage({viewport:{width,height:1100}}),errors=[];p.on('pageerror',e=>errors.push(e.message));
  await p.route('**/*',r=>r.request().url().startsWith(fixed.url)?r.continue():r.abort());
  await p.goto(fixed.url);await p.evaluate(t=>document.documentElement.dataset.fmTheme=t,theme);
  await p.waitForFunction(()=>document.querySelector('#fixture_counts').textContent.includes('"stored":true'));
  await p.waitForFunction(()=>document.querySelectorAll('#lineup-pitch .fm-lineup-player').length===15);
  await p.waitForTimeout(300);const count=async()=>JSON.parse(await p.locator('#fixture_counts').textContent());
  assert.equal(await p.locator('#lineup-status .fm-info-error').count(),0);
  assert.equal(await p.locator('.fm-lineup-editor .fm-info[open]').count(),0,'Technical information starts collapsed');
  const help=p.locator('.fm-lineup-refresh-help');await help.locator('summary').click();
  assert.equal(await help.locator('.fm-info-body').isVisible(),true);await help.locator('summary').click();
  const redraws=await p.evaluate(()=>new Promise(resolve=>{let n=0;const observer=new MutationObserver(records=>n+=records.length);
   observer.observe(document.querySelector('#lineup-pitch'),{childList:true,subtree:true,attributes:true});
   setTimeout(()=>{observer.disconnect();resolve(n)},500)}));assert.equal(redraws,0,'Idle pitch must not redraw');
  let before=await count();await p.locator('#lineup-best').click();
  await p.waitForFunction(()=>document.querySelector('#lineup-status').textContent.includes('Review your selections, then Save'));
  assert.equal(await p.locator('#lineup-pitch [data-player-id="p2"]').count(),1);
  assert.match(await p.locator('#lineup-pitch [data-player-id="p2"] .fm-lineup-points').textContent(),/-1/);
  await p.waitForTimeout(200);let after=await count();assert.deepEqual(after,before,'Optimize must not acquire or save');
  const selected=await p.locator('#lineup-pitch .fm-lineup-player').evaluateAll(es=>es.map(e=>e.dataset.playerId));
  await p.locator('#invalid_calendar').click();await p.waitForFunction(()=>document.querySelector('#lineup-status').textContent.includes('Round schedule observations are incomplete or invalid'));
  assert.ok(await p.locator('#lineup-status .fm-info-error').count()>0);
  const error=p.locator('#lineup-status .fm-info-error').first();assert.equal(await error.getAttribute('open'),null);
  assert.equal(await error.locator('.fm-info-body').isVisible(),false);await error.locator('summary').click();
  assert.equal(await error.locator('.fm-info-body').isVisible(),true);await error.locator('summary').click();
  assert.deepEqual(await p.locator('#lineup-pitch .fm-lineup-player').evaluateAll(es=>es.map(e=>e.dataset.playerId)),selected);
  await p.locator('#valid_calendar').click();await p.waitForFunction(()=>!document.querySelector('#lineup-status').textContent.includes('Round schedule observations are incomplete or invalid'));
  await p.waitForTimeout(200);assert.equal(await p.locator('#lineup-status .fm-info-error').count(),0);assert.deepEqual(await count(),before);
  const tick=Date.now();await p.locator('#heartbeat').click();await p.waitForFunction(()=>document.querySelector('#heartbeat_value').textContent==='1');const heartbeat=Date.now()-tick;
  assert.equal(await p.evaluate(()=>document.documentElement.scrollWidth>innerWidth+2),false);
  await p.screenshot({path:`/tmp/futmondo-lineup-errors-${theme}${width===390?'':'-desktop'}.png`,fullPage:true});
  before=await count();await p.reload();await p.waitForFunction(()=>document.querySelectorAll('#lineup-pitch .fm-lineup-player').length===15);await p.waitForTimeout(300);
  after=await count();assert.equal(after.source,before.source);assert.equal(after.db_write,before.db_write);assert.equal(after.db_read,before.db_read+1);assert.equal(after.observed_at,before.observed_at);assert.equal(after.submit,0);
  assert.equal(await p.locator('#lineup-status .fm-info-error').count(),0);assert.deepEqual(errors,[]);
  console.log(`PASS ${width}px ${theme}: correct scoring, collapsed strict failures, signed optimization, durable reload; idle redraws ${redraws}, heartbeat ${heartbeat}ms`);await p.close();
 }
 console.log('LINEUP ERROR BROWSER: all checks passed');
 }finally{if(browser)await browser.close();apps.forEach(a=>{if(a.exitCode===null)a.kill();});}})().catch(e=>{console.error(e);process.exitCode=1});
