#!/usr/bin/env node
const assert = require('node:assert/strict');
const net = require('node:net');
const { spawn } = require('node:child_process');
const { chromium } = require((process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules') + '/playwright');
const delay = ms => new Promise(resolve => setTimeout(resolve, ms));
const port = () => new Promise(resolve => {const server=net.createServer();server.listen(0,'127.0.0.1',()=>{const value=server.address().port;server.close(()=>resolve(value));});});
(async()=>{
 let app,browser,log='';
 try {
  const chosen=await port();
  app=spawn('Rscript',['test/test_page_information_browser.R'],{cwd:process.cwd(),env:{...process.env,FUTMONDO_INFO_TEST_PORT:String(chosen)},stdio:['ignore','pipe','pipe']});
  app.stdout.on('data',chunk=>log+=chunk);app.stderr.on('data',chunk=>log+=chunk);
  const url=`http://127.0.0.1:${chosen}`;
  let ready=false;
  for(let i=0;i<200;i++){if(app.exitCode!==null)throw Error(log);try{const response=await fetch(url);if(response.ok){ready=true;break;}}catch{}await delay(100);}
  assert(ready,'R fixture did not start');
  browser=await chromium.launch({headless:true,executablePath:process.env.FUTMONDO_TEST_CHROMIUM||undefined,args:['--no-sandbox']});
  const results=[];
  for(const width of [320,390,1440])for(const theme of ['light','dark']){
   const page=await browser.newPage({viewport:{width,height:1050}});page.setDefaultTimeout(10000);
   const errors=[],requests=[];
   page.on('pageerror',error=>errors.push(error.message));page.on('request',request=>requests.push(request.url()));
   await page.goto(url);
   await page.evaluate(mode=>document.documentElement.setAttribute('data-fm-theme',mode),theme);
   const market=page.locator('#shiny-tab-market .fm-info-panel');
   const body=market.locator(':scope > .fm-info-body');
   const source=label=>body.locator('.fm-info-entry').filter({has:page.locator('.fm-info-source-label',{hasText:label})});
   const counts=async()=>JSON.parse(await page.locator('#counts').textContent());
   await page.waitForFunction(()=>document.querySelector('#counts')?.textContent.includes('io_attempts'));
   await page.waitForFunction(()=>document.querySelectorAll('#shiny-tab-market .fm-info-entry').length===5);
   assert.equal(await page.locator('summary:visible').count(),1,'one passive icon on the page');
   assert(await page.locator('#shiny-tab-empty .fm-info-panel').evaluate(n=>n.hidden),'empty/pending-only page has no icon');
   assert.equal(await page.locator('#shiny-tab-market h2').count(),1,'no duplicate page title');
   assert(!(await market.evaluate(n=>n.open)));
   const titleBox=await page.locator('#shiny-tab-market h2').boundingBox();const iconBox=await market.locator(':scope > summary').boundingBox();
   assert(Math.abs(titleBox.y+titleBox.height/2-iconBox.y-iconBox.height/2)<2,'icon beside title');
   await market.locator(':scope > summary').focus();await page.keyboard.press('Enter');
   assert(await market.evaluate(n=>n.open));
   assert((await body.innerText()).includes('Finance context A'));
   const original=await counts();
   await page.evaluate(()=>{window.panelRef=document.querySelector('#shiny-tab-market .fm-info-panel');window.clockRef=document.querySelector('#source_clock');});
   await page.locator('#advance_stamp').click();
   await page.waitForFunction(()=>document.querySelector('#source_clock').textContent==='Observed clock 2');
   assert(await page.evaluate(()=>window.panelRef===document.querySelector('#shiny-tab-market .fm-info-panel')&&window.panelRef.open&&window.clockRef===document.querySelector('#source_clock')));
   assert.equal((await counts()).healthy_renders,original.healthy_renders,'clock does not repaint source');
   assert.equal((await counts()).finance_renders,original.finance_renders);
   await page.locator('#toggle_error').click();
   await page.waitForFunction(()=>document.querySelector('#shiny-tab-market .fm-info-panel').classList.contains('fm-info-error'));
   assert.equal(await page.locator('#retry').count(),1,'one bound Retry control after source replacement');
   const colors=await page.evaluate(()=>{
    const panel=document.querySelector('#shiny-tab-market .fm-info-panel');
    const entries=[...panel.querySelectorAll('.fm-info-entry')];
    const probe=document.createElement('span');probe.style.color='var(--fm-danger)';document.body.append(probe);const danger=getComputedStyle(probe).color;probe.remove();
    return {danger,icon:getComputedStyle(panel.querySelector('summary')).color,
      error:getComputedStyle(entries.find(n=>n.querySelector('.fm-info-source-label').textContent==='Market prices').querySelector('.fm-info-body')).color,
      healthy:getComputedStyle(entries.find(n=>n.querySelector('.fm-info-source-label').textContent==='Background sync').querySelector('.fm-info-body')).color};
   });
   assert.equal(colors.icon,colors.danger);assert.equal(colors.error,colors.danger);assert.notEqual(colors.healthy,colors.danger);
   assert(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth+1),'expanded long error wraps on mobile');
   if(width===390)await page.screenshot({path:`/tmp/futmondo-page-information-error-${theme}.png`,fullPage:true});
   assert((await market.locator('summary').first().getAttribute('aria-label')).includes('errors'));
   await page.locator('#toggle_second').click();await source('Round awards').waitFor();
   await page.locator('#retry').click();await page.waitForFunction(()=>document.querySelector('#shiny-tab-market .fm-info-panel').textContent.includes('Source recovered.'));
   assert(await market.evaluate(n=>n.classList.contains('fm-info-error')),'remaining failed source keeps the icon red');
   await page.locator('#toggle_second').click();await page.waitForFunction(()=>!document.querySelector('#shiny-tab-market .fm-info-panel').classList.contains('fm-info-error'));
   assert.equal((await counts()).retries,1);assert(await market.evaluate(n=>n.open));
   await page.locator('#start_pending').click();
   await source('Market prices').waitFor({state:'detached'});
   assert(await market.locator(':scope > summary').isVisible());assert(await source('Background sync').isVisible());
   await source('Market prices').waitFor();assert.equal(await page.locator('#retry').count(),1);
   for(let i=0;i<6;i++){await page.locator('#toggle_error').click();await page.waitForFunction(expected=>document.querySelector('#shiny-tab-market .fm-info-panel').classList.contains('fm-info-error')===expected,i%2===0);}
   assert.equal(await page.locator('#retry').count(),1);assert.equal(await page.locator('#shiny-tab-market .fm-info-entry').count(),5);
   await market.locator(':scope > summary').click();
   await page.locator('#toggle_error').click();await page.waitForFunction(()=>document.querySelector('#shiny-tab-market .fm-info-panel').classList.contains('fm-info-error'));
   assert(!(await market.evaluate(n=>n.open)),'a failed source updates the closed icon without opening it');
   await page.locator('#toggle_error').click();await page.waitForFunction(()=>!document.querySelector('#shiny-tab-market .fm-info-panel').classList.contains('fm-info-error'));
   await market.locator(':scope > summary').click();
   await page.locator('#toggle_widget_failure').click();
   await page.waitForFunction(()=>document.querySelector('#shiny-tab-market .fm-info-panel').textContent.includes('Widget source failed.'));
   assert(await market.evaluate(n=>n.classList.contains('fm-info-error')),'generated table error participates in page severity');
   await page.locator('#toggle_widget_failure').click();
   await page.waitForFunction(()=>!document.querySelector('#shiny-tab-market .fm-info-panel').textContent.includes('Widget source failed.'));
   await page.locator('#widget_error .rt-td').filter({hasText:'One'}).waitFor();
   assert(!(await market.evaluate(n=>n.classList.contains('fm-info-error'))),'widget recovery removes its old error entry');
   await page.locator('#go_team').click();await page.locator('#shiny-tab-team .fm-info-panel > summary').waitFor();
   const team=page.locator('#shiny-tab-team .fm-info-panel');await team.locator(':scope > summary').click();
   assert((await team.innerText()).includes('Finance context A'));assert((await team.innerText()).includes('Team-specific information'));
   assert(!(await team.innerText()).includes('Market guide'));assert.equal(await page.locator('summary:visible').count(),1);
   await page.locator('#go_market').click();assert(await market.evaluate(n=>n.open));
   await page.locator('#open_dialog').click();await page.locator('.modal-content .fm-info-panel > summary').waitFor();
   const dialog=page.locator('.modal-content .fm-info-panel');await dialog.locator(':scope > summary').click();
   await page.locator('#dialog_retry').waitFor();await page.locator('#dialog_retry').click();
   await page.waitForFunction(()=>JSON.parse(document.querySelector('#counts').textContent).retries===2);
   assert.equal(await page.locator('.modal-content summary:visible').count(),1);
   await page.locator('.modal-footer button').click();await page.locator('.modal-content').waitFor({state:'detached'});
   await page.locator('#open_dialog').click();await page.locator('.modal-content .fm-info-panel > summary').click();
   await page.waitForFunction(()=>document.querySelector('.modal-content .fm-info-panel').textContent.includes('Dialog generation 2'));
   assert.equal(await page.locator('#dialog_retry').count(),1);assert.equal(await page.locator('.modal-content .fm-info-entry').count(),2);
   await page.locator('#dialog_retry').click();await page.waitForFunction(()=>JSON.parse(document.querySelector('#counts').textContent).retries===3);
   await page.locator('.modal-footer button').click();await page.locator('.modal-content').waitFor({state:'detached'});
   await page.locator('#remove_source').click();await source('Optional source').waitFor({state:'detached'});
   await page.locator('#action_error').click();const notice=page.locator('#shiny-notification-action-error');
   await notice.waitFor();assert((await notice.innerText()).includes('Purchase failed'));assert.equal(await notice.locator('.fm-info').count(),0);
   assert(await page.locator('.fm-info-inline-error').isVisible(),'action validation remains visible');
   await notice.locator('.shiny-notification-close').click();
   await page.locator('#switch_context').click();
   await page.waitForFunction(()=>document.querySelector('#shiny-tab-market .fm-info-panel').textContent.includes('Finance context B'));
   assert(!(await body.innerText()).includes('Finance context A'));
   assert(!(await market.evaluate(n=>n.open)),'new context starts collapsed');
   await market.locator(':scope > summary').click();
   assert((await body.innerText()).includes('Market guide'),'static explanations survive context changes');
   assert((await body.innerText()).includes('Successful source'),'unchanged healthy source republishes for the new context');
   await page.locator('#start_pending').click();await source('Market prices').waitFor({state:'detached'});await source('Market prices').waitFor();
   assert.equal(await page.locator('#retry').count(),1);
   const requestCount=requests.length;
   await market.locator(':scope > summary').focus();await page.keyboard.press('Space');await page.keyboard.press('Enter');
   assert.equal(requests.length,requestCount,'opening panel must not acquire data');
   const perf=await page.evaluate(async()=>{
    let ticks=0,gap=0,last=performance.now();const timer=setInterval(()=>{const now=performance.now();gap=Math.max(gap,now-last);last=now;ticks++;},10);
    const div=document.createElement('div');document.body.append(div);for(let i=0;i<150;i++)div.append(document.createElement('span'));
    const transient=document.createElement('details');transient.className='fm-info';div.append(transient);transient.remove();
    await new Promise(resolve=>setTimeout(resolve,120));clearInterval(timer);div.remove();return {ticks,gap};
   });
   assert(perf.ticks>=3&&perf.gap<500,'registry must not block browser');
   const start=Date.now();await page.locator('#heartbeat').click();await page.waitForFunction(()=>document.querySelector('#heartbeat_value').textContent==='1');const heartbeatMs=Date.now()-start;
   assert(heartbeatMs<1500);assert.equal((await counts()).io_attempts,0);assert.deepEqual(errors,[]);
   assert(requests.every(request=>request.startsWith(url)),'no external requests');
   assert(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth+1),JSON.stringify(await page.evaluate(()=>[...document.querySelectorAll('body *')].filter(n=>n.getBoundingClientRect().right>innerWidth+1).slice(0,6).map(n=>({tag:n.tagName,id:n.id,class:n.className,right:n.getBoundingClientRect().right})))));
   if(width===390)await page.screenshot({path:`/tmp/futmondo-page-information-${theme}.png`,fullPage:true});
   results.push({width,theme,heartbeatMs,browserGapMs:Math.round(perf.gap),ioAttempts:0});
   await page.close();
  }
  console.log('PAGE INFORMATION BROWSER: 6 layouts passed');console.log(JSON.stringify(results));
 }catch(error){console.error(log);throw error;}
 finally{if(browser)await browser.close();if(app)app.kill('SIGTERM');}
})().catch(error=>{console.error(error);process.exitCode=1;});
