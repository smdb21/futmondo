const assert=require('node:assert/strict');
const {chromium}=require((process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules')+'/playwright');
(async()=>{const browser=await chromium.launch({headless:true,args:['--no-sandbox']});try{
 for(const width of [320,390,1440]){
  const page=await browser.newPage({viewport:{width,height:1000}});const errors=[];page.on('pageerror',e=>errors.push(e.message));
  await page.route('**/*',r=>new URL(r.request().url()).hostname==='127.0.0.1'?r.continue():r.abort());
  await page.goto('http://127.0.0.1:3919');await page.waitForFunction(()=>Boolean(document.querySelector('#fixture_diag')?.textContent));const priorRenewals=JSON.parse(await page.locator('#fixture_diag').textContent()).renewals;await page.locator('#login-open_login').click();
  await page.locator('#login-user_name').fill('offline@example.invalid');await page.locator('#login-password').fill('fixture');await page.locator('#login-login_button').click();
  const navigate=async tab=>{
   const link=page.locator(`a[data-value="${tab}"]`);
   if(width<768&&!await page.locator('body').evaluate(el=>el.classList.contains('sidebar-open')))await page.locator('.sidebar-toggle').click();
   await link.click();
  };
  await page.locator('a[data-value="admin"]').waitFor({state:'attached'});await navigate('login');
  await page.waitForFunction(()=>{const d=document.querySelector('#fixture_diag');if(!d?.textContent)return false;const v=JSON.parse(d.textContent);return v.tab==='login'&&v.startup==='ready'&&v.renewal_pending;},{timeout:5000});
  assert.equal(await page.locator('#startup_status').innerText(),'');
  const started=Date.now();await navigate('admin');await page.locator('#admin-league_settings .fm-admin-settings-card').first().waitFor({timeout:1500});const settingsMs=Date.now()-started;
  assert.equal(await page.locator('#admin-league_settings .fm-admin-settings-card').count(),6);
  const cash=page.locator('.fm-admin-setting').filter({hasText:'Starting cash'});assert.equal(await cash.locator('strong').innerText(),'30.000.000 EUR');
  let diag=JSON.parse(await page.locator('#fixture_diag').textContent());assert.ok(diag.renewal_pending,'test must hold renewal while settings render');
  const pulse=Date.now();await page.evaluate(()=>Shiny.setInputValue('fixture_heartbeat',1,{priority:'event'}));await page.waitForFunction(()=>document.querySelector('#fixture_beats').textContent==='1');const heartbeatMs=Date.now()-pulse;assert.ok(heartbeatMs<700);
  await page.waitForFunction(()=>{const t=document.querySelector('#fixture_diag')?.textContent;return t&&!JSON.parse(t).renewal_pending;},{timeout:7000});
  diag=JSON.parse(await page.locator('#fixture_diag').textContent());assert.equal(diag.io,0);assert.equal(diag.configuration_fields,9);assert.equal(diag.renewals,priorRenewals+1);
  await navigate('login');await page.waitForFunction(()=>JSON.parse(document.querySelector('#fixture_diag').textContent).tab==='login');
  assert.equal(await page.locator('#startup_status').innerText(),'');
  assert.equal(await page.locator('.shiny-output-error:visible').count(),0);assert.deepEqual(errors,[]);
  await navigate('admin');await page.locator('.fm-admin-setting').first().waitFor();
  assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+2),false);
  await page.locator('.fm-admin').screenshot({path:`/tmp/startup-admin-renew-${width}.png`});
  console.log(`PASS ${width}px full root: settings ${settingsMs}ms and heartbeat ${heartbeatMs}ms during 5s renewal; Welcome settles; one renewal, zero live I/O`);
  await page.close();
 }
}finally{await browser.close();}})().catch(e=>{console.error(e.stack);process.exitCode=1});
