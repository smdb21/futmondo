// Start test/test_account_summary_browser_app.R first. All data is synthetic.
const assert=require('node:assert/strict');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
(async()=>{
 const browser=await chromium.launch({headless:true,args:['--no-sandbox'],
  ...(process.env.FUTMONDO_TEST_CHROMIUM?{executablePath:process.env.FUTMONDO_TEST_CHROMIUM}:{})});
 const url=`http://127.0.0.1:${process.env.FUTMONDO_SUMMARY_TEST_PORT||3892}/`;
 try{
  for(const width of [320,390,1440])for(const theme of ['light','dark']){
   const page=await browser.newPage({viewport:{width,height:1000}});
   const errors=[];page.on('pageerror',e=>errors.push(e.message));
   await page.route('**/*',r=>r.request().url().startsWith(url)?r.continue():r.abort());
   await page.goto(url);
   await page.waitForFunction(()=>window.Shiny?.shinyapp?.$socket?.readyState===1);
   await page.evaluate(theme=>document.documentElement.dataset.fmTheme=theme,theme);
   await page.locator('#login-user_name').fill('offline@example.invalid');
   await page.locator('#login-password').fill('fixture');
   await page.locator('#login-login_button').click();
   await page.waitForFunction(()=>Shiny.shinyapp.$inputValues.selected_league==='champ');
   const navigate=async tab=>{
    await page.locator(`.sidebar-menu a[data-value="${tab}"]`).evaluate(el=>el.click());
    await page.waitForFunction(tab=>Shiny.shinyapp.$inputValues.tabs===tab,tab);
   };
   await navigate('today');
   const summary=page.locator('#account_finance_summary .fm-account-finance');
   await summary.getByText('Balance: 10.000.000 EUR',{exact:true}).waitFor().catch(async e=>{
    await page.screenshot({path:'/tmp/futmondo-summary-failure.png'});
    console.log(await page.evaluate(()=>({tab:Shiny.shinyapp.$inputValues.tabs,html:document.querySelector('#account_finance_summary').outerHTML,text:document.body.innerText.slice(0,900)})));throw e;
   });
   await summary.getByText('Active offers: 200.000 EUR (1)',{exact:true}).waitFor();
   // Financial outputs are never moved/hidden by the information registry.
   for (const id of ['account_finance_summary','round_finance_amounts','account_squad']) {
    const node=page.locator('#'+id);
    await node.evaluate(n=>n.classList.add('shiny-output-error','shiny-output-error-fm-loading','fm-info-waiting'));
    await summary.getByText('Balance: 10.000.000 EUR',{exact:true}).waitFor();
    await page.waitForFunction(id=>!document.getElementById(id).classList.contains('fm-info-waiting'),id);
    assert.equal(await node.evaluate(n=>Boolean(n.closest('.fm-info-panel'))),false);
    await node.evaluate(n=>n.classList.remove('shiny-output-error','shiny-output-error-fm-loading'));
   }
   // A pending/recovered schedule cannot hide financial figures.
   const schedule=page.locator('#round_countdown');
   await schedule.evaluate(n=>n.classList.add('shiny-output-error','shiny-output-error-fm-loading'));
   await schedule.waitFor({state:'hidden'});
   await summary.getByText('Balance: 10.000.000 EUR',{exact:true}).waitFor();
   await schedule.evaluate(n=>n.classList.remove('shiny-output-error','shiny-output-error-fm-loading'));
   await schedule.waitFor({state:'visible'});
   await summary.getByText('Players: 17',{exact:true}).waitFor();
   await summary.getByText('Effective players: 17 / 18',{exact:true}).waitFor();
   await summary.getByText('No squad slots free for new offers.',{exact:true}).waitFor();
   // Keep one shell and financial node across navigation and clock updates.
   await summary.evaluate(n=>n.dataset.fixtureIdentity='retained');
   await page.locator('#round_finance_amounts').evaluate(n=>n.dataset.fixtureIdentity='retained');
   const stats=async()=>{
    await page.evaluate(()=>{window.fixtureStats=null;Shiny.addCustomMessageHandler('fixture-stats',x=>window.fixtureStats=x);Shiny.setInputValue('fixture_stats',Math.random(),{priority:'event'});});
    await page.waitForFunction(()=>window.fixtureStats!==null);return page.evaluate(()=>fixtureStats);
   };
   // Allow the existing page-specific resources to mount before measuring reuse.
   for(const tab of ['yourteam','players_in_championship','today']){
    await navigate(tab);await page.waitForTimeout(150);
   }
   const before=await stats();const timings=[];
   for(const tab of ['yourteam','players_in_championship','today']){
    const started=Date.now();await navigate(tab);
    await summary.getByText('After offers: 9.800.000 EUR',{exact:true}).waitFor();
    await summary.getByText('Can spend: 15.000.000 EUR',{exact:true}).waitFor();
    timings.push(Date.now()-started);
    assert.equal(await page.locator('#account_finance_summary').count(),1);
    assert.equal(await summary.getAttribute('data-fixture-identity'),'retained');
    assert.equal(await page.locator('#round_finance_amounts').getAttribute('data-fixture-identity'),'retained');
    await summary.getByText('No squad slots free for new offers.',{exact:true}).waitFor();
    const panel=page.locator(`#shiny-tab-${tab} .fm-info-panel`).first();
    if(await panel.isVisible() && await panel.evaluate(n=>n.open))await panel.locator(':scope > summary').click();
    assert.equal(await summary.isVisible(),true);
    const bounds=await summary.boundingBox();
    const content=await page.locator(`#shiny-tab-${tab}`).boundingBox();
    assert.ok(bounds && bounds.y>=0 && bounds.y+bounds.height<=content.y+1);
    assert.ok(bounds.x>=0 && bounds.x+bounds.width<=width+1);
    assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+1),false);
   }
   const after=await stats();
   const primary=reads=>reads.filter(x=>['get_user_team_info','get_market_players','get_players_from_team','get_championship_pressroom','get_clause_transfer_summary'].includes(x)).length;
   assert.equal(primary(after.reads),primary(before.reads),`navigation repeated shared reads: ${JSON.stringify({before:before.reads,after:after.reads})}`);
   assert.equal(after.http,0);assert.equal(after.mutations,0);
   await page.screenshot({path:`/tmp/futmondo-account-summary-${width}-${theme}.png`});
   for(const tab of ['login','notifications','lineup']){
    await navigate(tab);await summary.waitFor({state:'hidden'});
   }
   assert.deepEqual(errors,[]);
   console.log(`PASS ${width}px/${theme}: stable finance/squad summary first on all three pages, other pages hidden, no overflow, repeated shared reads or live I/O; max navigation ${Math.max(...timings)}ms`);
   await page.close();
  }
 }finally{await browser.close()}
})().catch(e=>{console.error(e.stack);process.exitCode=1});
