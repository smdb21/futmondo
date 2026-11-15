#!/usr/bin/env node
// Starts the synthetic R fixture; production CSS/JS are exercised in Chromium.
const assert = require('node:assert/strict');
const net = require('node:net');
const fs = require('node:fs');
const { spawn } = require('node:child_process');
const { chromium } = require((process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules') + '/playwright');
const delay = ms => new Promise(resolve => setTimeout(resolve, ms));
const port = () => new Promise(resolve => {const server=net.createServer();server.listen(0,'127.0.0.1',()=>{const value=server.address().port;server.close(()=>resolve(value));});});
(async()=>{
 let app,browser,log='';
 try {
  const chosen=await port();
  app=spawn('Rscript',['test/test_info_disclosures_browser.R'],{cwd:process.cwd(),env:{...process.env,FUTMONDO_INFO_TEST_PORT:String(chosen)},stdio:['ignore','pipe','pipe']});
  app.stdout.on('data',chunk=>log+=chunk);app.stderr.on('data',chunk=>log+=chunk);
  const url=`http://127.0.0.1:${chosen}`;
  let ready=false;
  for(let i=0;i<200;i++){if(app.exitCode!==null)throw Error(log);try{const response=await fetch(url);if(response.ok){ready=true;break;}}catch{}await delay(100);}
  assert(ready,'R fixture did not start');
  browser=await chromium.launch({headless:true,args:['--no-sandbox']});
  const results=[];
  for(const width of [320,390,1440])for(const theme of ['light','dark']){
   console.log('Checking',width,theme);
   const page=await browser.newPage({viewport:{width,height:1050}});
   page.setDefaultTimeout(10000);
   const errors=[];const external=[];const requests=[];
   page.on('pageerror',error=>errors.push(error.message));
   page.on('request',request=>{requests.push(request.url());if(!request.url().startsWith(url))external.push(request.url());});
   await page.goto(url);
   await page.waitForFunction(()=>document.querySelector('#counts').textContent.includes('io_attempts'));
   await page.evaluate(mode=>document.documentElement.setAttribute('data-fm-theme',mode),theme);
   const counts=async()=>JSON.parse(await page.locator('#counts').textContent());
   const noOverflow=async()=>{const issue=await page.evaluate(()=>({width:innerWidth,scroll:document.documentElement.scrollWidth,offenders:[...document.querySelectorAll('body *')].filter(n=>n.getBoundingClientRect().right>innerWidth+1).slice(0,8).map(n=>({tag:n.tagName,id:n.id,class:n.className,text:n.textContent.slice(0,80),right:n.getBoundingClientRect().right}))}));assert(issue.scroll<=issue.width+1,JSON.stringify(issue));};
   const isOpen=selector=>page.locator(selector).evaluate(node=>node.open);
   const hidden=selector=>page.locator(selector).evaluate(node=>getComputedStyle(node).display==='none');
   const initial=await counts();assert.equal(initial.io_attempts,0);assert.equal(initial.retries,0);
   await page.locator('#tooltip_table .fm-team-label').first().waitFor();
   await page.waitForFunction(()=>document.querySelector('#tooltip_table .fm-departing-before-round .fm-team-label')?.title.includes('Scheduled to leave'));
   assert.equal(await page.locator('#tooltip_table input[type=radio], #tooltip_table .rt-td-select').count(),0);
   const leaving=page.locator('#tooltip_table .fm-team-label').filter({hasText:'Departing name'});
   const staying=page.locator('#tooltip_table .fm-team-label').filter({hasText:'Staying name'});
   assert.ok((await leaving.getAttribute('title')).includes('Scheduled to leave'));
   assert.ok(!(await staying.getAttribute('title')).includes('Scheduled to leave'));
   await leaving.click();await page.waitForFunction(()=>document.querySelector('#selected_row').textContent==='1');
   await page.locator('#toggle_departure').click();await page.waitForFunction(()=>!document.querySelector('#tooltip_table .fm-team-label').title.includes('Scheduled to leave'));
   assert.equal(await leaving.getAttribute('title'),'Club');
   await page.waitForFunction(()=>document.querySelector('#clock_panel details')?.hidden===false);
   assert.equal(await page.locator('#empty_dynamic details').isVisible(),false);
   assert.equal(await page.locator('#pending_panel .fm-info').count(),0);
   assert.equal(await page.locator('#settled_error_panel .fm-info').count(),0);
   await page.locator('#show_deferred').click();await page.locator('#empty_dynamic summary').waitFor({state:'visible'});
   await page.locator('#empty_dynamic summary').click();assert.equal(await page.locator('#deferred_text').innerText(),'Ready source information');
   await page.locator('#clear_deferred').click();await page.waitForFunction(()=>document.querySelector('#empty_dynamic details').hidden);
   await page.locator('#wait_deferred').click();await page.waitForTimeout(100);assert.equal(await page.locator('#pending_panel .fm-info').count(),0);
   await page.locator('#pending_panel summary').waitFor({state:'visible'});
   await page.locator('#fail_deferred').click();await page.waitForTimeout(100);assert.equal(await page.locator('#settled_error_panel .fm-info').count(),0);
   await page.locator('#settled_error_panel .fm-info-error').waitFor({state:'visible'});
   await page.locator('#wait_deferred').click();await page.waitForTimeout(100);assert.equal(await page.locator('#settled_error_panel .fm-info').count(),0);
   await page.locator('#pending_panel summary').waitFor({state:'visible'});
   assert.equal(await page.locator('.fm-info[open]:visible').count(),0);
   for(const selector of ['#neutral','#long_error','#real_offers','#real_rivals','#real_chart','#login_feedback_fixture']){
    assert(await hidden(`${selector} .fm-info-body`),`${selector} must be collapsed`);
   }
   const geometry=await page.locator('#long_error summary').evaluate(node=>{
    const icon=node.querySelector('.fm-info-icon');const b=icon.getBoundingClientRect();const s=getComputedStyle(icon);const probe=document.createElement('span');probe.style.color='var(--fm-danger)';document.body.append(probe);const danger=getComputedStyle(probe).color;probe.remove();
    return {width:b.width,height:b.height,radius:s.borderRadius,color:s.color,danger,trigger:node.getBoundingClientRect().width};
   });
   assert.equal(geometry.width,18);assert.equal(geometry.height,18);assert.equal(geometry.radius,'50%');assert.equal(geometry.color,geometry.danger);assert.equal(geometry.trigger,28);
   const loginBox=await page.locator('#login_feedback_fixture details').evaluate(node=>({padding:getComputedStyle(node).padding,border:getComputedStyle(node).borderWidth}));
   assert.equal(loginBox.padding,'0px');assert.equal(loginBox.border,'0px');
   await noOverflow();
   // Click and native keyboard toggles. They do not submit an action.
   const requestCount=requests.length;
   await page.locator('#neutral summary').click();assert(await isOpen('#neutral details'));
   await page.locator('#neutral summary').focus();await page.keyboard.press('Enter');assert(!(await isOpen('#neutral details')));
   await page.keyboard.press('Space');assert(await isOpen('#neutral details'));
   await page.locator('#long_error summary').click();assert(!(await hidden('#long_error .fm-info-body')));await noOverflow();
   // Nested renderText updates must not replace their open parent panel.
   await page.locator('#clock_panel summary').click();
   await page.waitForFunction(()=>document.querySelector('#source_clock').textContent==='fixture-clock 1');
   await page.evaluate(()=>{window.fixtureClock=document.querySelector('#clock_panel details');window.fixtureClock.marker='retained';});
   await page.locator('#advance_stamp').click();
   await page.waitForFunction(()=>document.querySelector('#source_clock').textContent==='fixture-clock 2');
   assert(await page.evaluate(()=>document.querySelector('#clock_panel details')===window.fixtureClock&&window.fixtureClock.open));
   // Replacing an actual renderUI retains the developer's open disclosure.
   await page.locator('#replaced_panel summary').click();
   await page.locator('#replace_panel').click();
   await page.waitForFunction(()=>document.querySelector('#replaced_panel').textContent.includes('Detail version 2'));
   assert(await isOpen('#replaced_panel details'));
   // Opening error details alone must never click the Retry inside them.
   await page.locator('#retry_panel summary').click();assert.equal((await counts()).retries,0);
   await page.locator('#retry').click();await page.waitForFunction(()=>JSON.parse(document.querySelector('#counts').textContent).retries===1);
   assert.equal((await counts()).retries,1);assert.equal((await counts()).clock_panels,1);assert.equal((await counts()).io_attempts,0);
   assert.equal(requests.length,requestCount,'disclosures caused HTTP requests');
   // Real Shiny-generated notification and validation DOM are wrapped dynamically.
   await page.locator('#validate_loading').click();
   await page.waitForFunction(()=>document.querySelector('#loading_validation').classList.contains('fm-info-waiting'));
   assert.equal(await page.locator('#loading_validation .fm-info').count(),0);
   assert.equal(await page.locator('#loading_validation').isVisible(),false);
   await page.locator('#validate_error').click();await page.locator('#runtime_error').click();await page.locator('#emit_error').click();await page.locator('#emit_warning').focus();await page.keyboard.press('Enter');
   for(const selector of ['#shiny-notification-fixture-error','#shiny-notification-fixture-warning','#validation','#runtime_error_output']){
    await page.locator(`${selector} .fm-info-error`).waitFor();
    assert(await hidden(`${selector} .fm-info-body`),`${selector} error detail leaked`);
    await page.locator(`${selector} summary`).click();
    assert(await isOpen(`${selector} details`));await noOverflow();
    if(selector.startsWith('#shiny-notification'))await page.locator(`${selector} .shiny-notification-close`).click();
   }
   assert.equal(await page.locator('#runtime_error_output').evaluate(node=>getComputedStyle(node,'::before').content),'none');
   // A large mutation burst must not block the browser or the Shiny heartbeat.
   const performance=await page.evaluate(async()=>{
    let ticks=0;let previous=performance.now();let largestGap=0;
    const timer=setInterval(()=>{const now=performance.now();largestGap=Math.max(largestGap,now-previous);previous=now;ticks++;},10);
    const target=document.createElement('div');target.id='mutation-fixture';document.body.append(target);
    for(let i=0;i<150;i++){const item=document.createElement('span');item.textContent=String(i);target.append(item);}
    await new Promise(resolve=>setTimeout(resolve,120));clearInterval(timer);target.remove();return {ticks,largestGap};
   });
   assert(performance.ticks>=3,'browser heartbeat stalled');assert(performance.largestGap<500,'mutation observer blocks browser');
   const started=Date.now();await page.locator('#heartbeat').click();await page.waitForFunction(()=>document.querySelector('#heartbeat_value').textContent==='1');const heartbeatMs=Date.now()-started;
   assert(heartbeatMs<1500,'Shiny heartbeat stalled');
   assert.equal((await counts()).io_attempts,0);assert.equal(external.length,0);assert.deepEqual(errors,[]);await noOverflow();
   if(width===390){await page.screenshot({path:`/tmp/futmondo-info-disclosures-${theme}.png`,fullPage:true});}
   results.push({width,theme,heartbeatMs,browserGapMs:Math.round(performance.largestGap),ioAttempts:0});
   await page.close();
  }
  console.log('INFO DISCLOSURES BROWSER: 6 layouts passed');console.log(JSON.stringify(results));
 }catch(error){console.error(log);throw error;}
 finally{if(browser)await browser.close();if(app)app.kill('SIGTERM');}
})().catch(error=>{console.error(error);process.exitCode=1;});
