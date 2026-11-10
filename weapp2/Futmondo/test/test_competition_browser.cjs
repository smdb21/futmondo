// Run Rscript test/test_competition_browser_app.R first. Synthetic accounts only.
const assert=require('node:assert/strict');
const path=require('node:path');
const {chromium}=require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules','playwright'));
const url=`http://127.0.0.1:${process.env.FUTMONDO_COMPETITION_TEST_PORT||'38862'}/`;
(async()=>{
 const browser=await chromium.launch({headless:true,args:['--no-sandbox']});
 try{
  const openSession=async width=>{
   const page=await browser.newPage({viewport:{width,height:1000}});const errors=[];
   page.on('pageerror',e=>errors.push(e.message));
   await page.route('**/*',route=>route.request().url().startsWith(url)?route.continue():route.abort());
   await page.goto(url);await page.waitForFunction(()=>window.Shiny&&Shiny.shinyapp?.$socket?.readyState===1);
   await page.waitForFunction(()=>document.querySelector('#login-login_button')?.dataset.futmondoLoginBound==='true');
   await page.locator('#login-user_name').fill('offline@example.invalid');await page.locator('#login-password').fill('fixture');
   await page.locator('#login-password').blur();await page.waitForTimeout(250);await page.locator('#login-login_button').click();
   await page.locator('#today-market_players-players_table .rt-select-input').first().waitFor();
   return {page,errors};
  };
  // Report first-use R compilation separately; the original card also incurs
  // this initialization CPU cost. Do not represent it as a <500ms result.
  const cold=await openSession(1440);const coldStarted=Date.now();
  await cold.page.locator('#today-market_players-players_table .rt-tr-group').filter({hasText:'Domestic Player 1'}).locator('.rt-select-input').check();
  await cold.page.locator('.modal:visible [id$="-player_identity_summary"]').filter({hasText:'Domestic Player 1'}).waitFor();
  const coldIdentityMs=Date.now()-coldStarted;const token=String(Math.random());const coldBeatStarted=Date.now();
  await cold.page.evaluate(token=>Shiny.setInputValue('fixture_heartbeat',token,{priority:'event'}),token);
  await cold.page.waitForFunction(token=>document.querySelector('#fixture_heartbeat')?.textContent===token,token);
  console.log(`DIAGNOSTIC cold card identity=${coldIdentityMs}ms, following heartbeat=${Date.now()-coldBeatStarted}ms; first-use compilation is a separately reported baseline limitation`);
  await cold.page.waitForFunction(()=>[...document.querySelectorAll('.modal .player-round-box')].some(x=>x.textContent.includes('-2 pts')&&x.textContent.includes('Domestic Home Two')));
  await cold.page.waitForTimeout(3000);assert.deepEqual(cold.errors,[]);await cold.page.close();
  if(process.env.FUTMONDO_COMPETITION_COLD_ONLY==='true')return;
  // The following assertions retain the strict held-I/O responsiveness budget
  // after interpreter initialization; every session still starts with empty data caches.
  for(const width of [1440,390,360]){
   const {page,errors}=await openSession(width);
   const labels=await page.locator('#selected_league').evaluate(el=>el.selectize?Object.values(el.selectize.options).map(x=>x.label):[...el.options].map(x=>x.text));
   assert.deepEqual(labels,['World Friends (Domestic Football)','World Friends (European Football)']);
   const heartbeat=async()=>{
    const token=String(Math.random());const started=Date.now();
    await page.evaluate(token=>Shiny.setInputValue('fixture_heartbeat',token,{priority:'event'}),token);
    await page.waitForFunction(token=>document.querySelector('#fixture_heartbeat')?.textContent===token,token);
    const ms=Date.now()-started;assert.ok(ms<500,`${width}px delayed-read heartbeat ${ms}ms`);return ms;
   };
   await page.locator('#today-market_players-players_table .rt-tr-group').filter({hasText:'Domestic Player 1'}).locator('.rt-select-input').check();
   const modal=page.locator('.modal:visible');await modal.locator('[id$="-player_identity_summary"]').filter({hasText:'Domestic Player 1'}).waitFor();
   const domesticBeat=await heartbeat();
   await page.waitForFunction(()=>[...document.querySelectorAll('.modal .player-round-box')].some(x=>x.textContent.includes('-2 pts')&&x.textContent.includes('Domestic Home Two')));
   const assertNoOverflow=async()=>{
    assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+2),false,`${width}px document overflow`);
    assert.equal(await modal.evaluate(el=>el.scrollWidth>el.clientWidth+2),false,`${width}px modal overflow`);
   };
   await modal.locator('.player-recent-rounds').evaluate(el=>el.open=true);
   await assertNoOverflow();await page.screenshot({path:`/tmp/futmondo-competition-domestic-${width}.png`,fullPage:true});
   await modal.getByRole('button',{name:'Close player card',exact:true}).click();await modal.waitFor({state:'hidden'});
   // The selector belongs to Welcome; retain its real Shiny input binding.
   await page.evaluate(()=>document.querySelector('.sidebar-menu a[data-value="login"]').click());
   await page.locator('#selected_league').evaluate(el=>el.selectize.setValue('game-european'));
   await page.evaluate(()=>document.querySelector('.sidebar-menu a[data-value="today"]').click());
   await page.locator('#today-market_players-players_table').filter({hasText:'European Player 1'}).waitFor();
   await page.locator('#today-market_players-players_table .rt-select-input').first().check();
   await modal.locator('[id$="-player_identity_summary"]').filter({hasText:'European Player 1'}).waitFor();
   const europeanBeat=await heartbeat();
   await page.waitForFunction(()=>[...document.querySelectorAll('.modal .player-round-box')].some(x=>x.textContent.includes('17 pts')));
   await page.waitForTimeout(1200);
   const rounds=await modal.locator('.player-round-box').allTextContents();
   await page.screenshot({path:`/tmp/futmondo-competition-european-${width}.png`,fullPage:true});
   const scored=rounds.filter(text=>text.includes(' pts'));
   assert.match(await modal.locator('.player-recent-rounds').textContent(),/Match records · points association unavailable/);
   assert.equal(scored.length,1,JSON.stringify(rounds));assert.match(scored[0],/17 pts/);assert.doesNotMatch(scored[0],/Domestic Home|Domestic Away|0–0|2–1/);
   await modal.locator('.player-recent-rounds').evaluate(el=>el.open=true);
   await assertNoOverflow();await page.screenshot({path:`/tmp/futmondo-competition-european-${width}.png`,fullPage:true});
   await modal.getByRole('button',{name:'Close player card',exact:true}).click();await modal.waitFor({state:'hidden'});
   await page.evaluate(()=>document.querySelector('.sidebar-menu a[data-value="yourteam"]').click());
   await page.waitForTimeout(1300);
   const teamText=await page.locator('#players_in_teams-players_table_in_teams-players_table').innerText();
   assert.doesNotMatch(teamText,/Domestic Player|European Player/);
   assert.match(teamText,/No rows found|No players|No data|empty/i,`${width}px empty roster must settle visibly: ${teamText}`);
   await page.screenshot({path:`/tmp/futmondo-competition-empty-roster-${width}.png`,fullPage:true});
   assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+2),false,`${width}px empty roster document overflow`);
   await page.evaluate(()=>{window.fixtureStats=null;Shiny.addCustomMessageHandler('competition-fixture-stats',x=>window.fixtureStats=x);Shiny.setInputValue('fixture_snapshot',Math.random(),{priority:'event'});});
   await page.waitForFunction(()=>window.fixtureStats!==null);const stats=await page.evaluate(()=>window.fixtureStats);
   assert.equal(stats.http,0);assert.deepEqual(errors,[]);
   console.log(`PASS ${width}px duplicate league labels; isolated domestic/European card scores; empty roster; no horizontal overflow; delayed-read heartbeat=${domesticBeat}/${europeanBeat}ms; external HTTP=0`);
   await page.close();
  }
 }finally{await browser.close();}
})().catch(error=>{console.error(error.stack);process.exitCode=1;});
