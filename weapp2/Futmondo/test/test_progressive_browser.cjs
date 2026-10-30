const assert = require('node:assert/strict');
const path = require('node:path');
const { chromium } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules','playwright'));
(async () => {
  const browser = await chromium.launch({headless:true,args:['--no-sandbox']});
  try {
    for (const width of [1440,390,360]) {
      const page = await browser.newPage({viewport:{width,height:1000}});
      await page.route('**/*',r => r.request().url().startsWith('http://127.0.0.1:38766/') ? r.continue() : r.abort());
      await page.goto('http://127.0.0.1:38766/');
      await page.waitForFunction(()=>window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket && Shiny.shinyapp.$socket.readyState===1);
      await page.evaluate(()=>{window.fixtureStats=null;Shiny.addCustomMessageHandler('fixture-stats',x=>window.fixtureStats=x);});
      const snapshot=async()=>{
        await page.evaluate(()=>{window.fixtureStats=null;Shiny.setInputValue('fixture_snapshot',Math.random(),{priority:'event'});});
        await page.waitForFunction(()=>window.fixtureStats!==null);return page.evaluate(()=>window.fixtureStats);
      };
      await page.waitForFunction(()=>document.getElementById('login-login_button')?.dataset.futmondoLoginBound==='true');
      await page.locator('#login-user_name').fill('offline@example.invalid');
      await page.locator('#login-password').fill('fixture');
      await page.locator('#login-password').blur();
      await page.waitForTimeout(350);
      const feedbackMs=await page.evaluate(()=>new Promise(resolve=>{
        const started=performance.now();document.getElementById('login-login_button').click();
        requestAnimationFrame(()=>resolve({ms:performance.now()-started,visible:getComputedStyle(document.getElementById('login-login_progress')).display!=='none'}));
      }));
      assert.ok(feedbackMs.visible && feedbackMs.ms<500,JSON.stringify(feedbackMs));
      await page.locator('#startup_status').filter({hasText:'Signed in. Loading your leagues…'}).waitFor();
      await page.locator('#startup_status').filter({hasText:'Loading Today…'}).waitFor();
      await page.locator('#today-kpi_cash_box').getByText('Current balance',{exact:true}).waitFor();
      await page.locator('#today-market_players-players_table .rt-select-input').first().waitFor();
      assert.match(await page.locator('#today-recent_transfers').innerText(),/Loading/);
      await page.evaluate(()=>{
        window.cardIdentityMs=null;const started=performance.now();
        const observer=new MutationObserver(()=>{
          const el=document.querySelector('.modal [id$="-player_identity_summary"]');
          if(el && el.textContent.includes('Market Player 1') && el.getClientRects().length){
            observer.disconnect();requestAnimationFrame(()=>{window.cardIdentityMs=performance.now()-started;});
          }
        });
        observer.observe(document.body,{childList:true,subtree:true});
        document.querySelector('#today-market_players-players_table .rt-select-input').click();
      });
      const modal=page.locator('.modal:visible');
      await modal.locator('[id$="-player_identity_summary"]').filter({hasText:'Market Player 1'}).waitFor();
      await page.waitForFunction(()=>window.cardIdentityMs!==null);
      const identityMs=Math.round(await page.evaluate(()=>window.cardIdentityMs));
      assert.ok(identityMs<500,`${width}px card identity ${identityMs}ms`);
      assert.match(await modal.innerText(),/Loading/);
      await modal.locator('[id$="-player_trend_plot"]').waitFor();
      const boxes=modal.locator('.player-round-box');
      await page.waitForFunction(()=>Array.from(document.querySelectorAll('.modal .player-round-box')).some(x=>x.textContent.includes('-2 pts')));
      assert.match(await modal.locator('[id$="-player_external_details"]').innerText(),/Loading profile/);
      await page.waitForFunction(()=>Array.from(document.querySelectorAll('.modal .player-round-box')).some(x=>x.textContent.includes('-2 pts') && x.textContent.includes('Club C')));
      await modal.locator('[id$="-player_external_details"]').filter({hasText:'Synthetic'}).waitFor();
      const texts=await boxes.allTextContents();
      assert.equal(texts.length,2);assert.ok(texts.some(x=>x.includes('0 pts')&&x.includes('0–0')));
      assert.ok(texts.some(x=>x.includes('-2 pts')&&x.includes('2–1')));
      assert.equal(await modal.locator('.player-round-status').count(),0);
      assert.doesNotMatch(await modal.innerText(),/0 cm|0 kg|provisional/);
      assert.equal(await modal.evaluate(el=>el.scrollWidth>el.clientWidth+2),false,`${width}px modal overflow`);
      for(const b of await boxes.all())assert.equal(await b.evaluate(el=>el.scrollWidth>el.clientWidth+2),false,`${width}px round overflow`);
      await page.screenshot({path:`/tmp/futmondo-progressive-${width}.png`,fullPage:true});
      await page.waitForTimeout(1200);
      const before=await snapshot();
      await modal.getByRole('button',{name:'Close',exact:true}).click();
      await modal.waitFor({state:'hidden'});
      await page.locator('#today-market_players-players_table .rt-select-input').first().check();
      await page.locator('.modal:visible .player-round-box').first().waitFor();
      await page.waitForTimeout(250);
      const after=await snapshot();
      for(const name of ['get_player_summary','get_player_matches','get_player_full_profile','read_card_round_history','read_card_price_history','get_acquisition_capacity'])
        assert.equal(after.reads[name],before.reads[name],`${width}px warm ${name}`);
      assert.equal(after.http,0);
      console.log(`PASS ${width}px login feedback=${feedbackMs.ms.toFixed(1)}ms card identity=${identityMs}ms; progressive matched rounds, mobile wrapping, warm reads=0, external HTTP=0`);
      await page.close();
    }
  } finally {await browser.close();}
})().catch(e=>{console.error(e.stack);process.exitCode=1;});
