// Run against Rscript test/test_live_substitution.R --browser.
const assert = require('node:assert/strict');
const path = require('node:path');
const { chromium } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules', 'playwright'));
(async () => {
  const browser = await chromium.launch({headless:true,executablePath:process.env.FUTMONDO_TEST_CHROMIUM || undefined,args:['--no-sandbox']});
  try {
    const url = 'http://127.0.0.1:' + (process.env.FUTMONDO_LIVE_TEST_PORT || '8597');
    for (const width of [390,1440]) {
      const page = await browser.newPage({ viewport:{width,height:900} });
      await page.route('**/*',r => new URL(r.request().url()).hostname==='127.0.0.1' ? r.continue() : r.abort());
      await page.goto(url);
      await page.getByText('Current round',{exact:true}).click();
      const live = page.locator('#lineup-live-pitch');
      await live.locator('.fm-lineup-layout').waitFor();
      assert.ok(await live.evaluate(n=>n.scrollWidth <= n.clientWidth+1));
      if(width===390){
        assert.equal((await page.locator('#xi_checks').innerText()).trim(),'0');
        await live.getByRole('button',{name:'Check club XI · one vs other'}).click();
        await page.waitForFunction(()=>document.querySelector('#xi_checks').textContent.trim()==='1');
        await live.getByText('On club bench',{exact:true}).waitFor();
        await live.getByRole('button',{name:'Substitute Forward'}).click();
        await live.getByRole('button',{name:'Reserve',exact:true}).click();
        const confirm = live.getByRole('button',{name:'Confirm substitution'});
        await confirm.waitFor();
        assert.match(await live.innerText(),/Round 8 · Forward out, Reserve in. Locks/);
        assert.match(await page.locator('#counts').innerText(),/^0 0 0 /);
        await confirm.evaluate(n=>{n.click();n.click()});
        await live.getByRole('button',{name:'Check official lineup'}).click();
        await live.getByText('Substitution verified.').waitFor();
        await live.getByRole('button',{name:'Retry saving the observed lineup'}).click();
        await page.waitForFunction(()=>/^5 1 2 /.test(document.querySelector('#counts').textContent));
      }
      await live.locator('.fm-lineup-pitch [data-player-id="Reserve"]').waitFor();
      await page.getByRole('tab',{name:'Classification',exact:true}).click();
      await page.getByRole('button',{name:'My team',exact:true}).click();
      const modal = page.locator('.modal');
      await modal.locator('.fm-lineup-pitch [data-player-id="Reserve"]').waitFor();
      await modal.getByText('Confirmed starter',{exact:true}).waitFor();
      await modal.getByRole('button',{name:'Substitute Reserve'}).waitFor();
      assert.ok(await modal.locator('.fm-lineup-layout').evaluate(n=>n.scrollWidth<=n.clientWidth+1));
      assert.match(await page.locator('#counts').innerText(),/^5 1 2 /);
      if(width===1440){
        await modal.getByRole('button',{name:'Substitute Reserve'}).click();
        await modal.getByRole('button',{name:'Forward',exact:true}).click();
        await modal.getByRole('button',{name:'Confirm substitution'}).click();
        await modal.getByText('Substitution verified.').waitFor();
        await modal.getByRole('button',{name:'Close lineup'}).click();
        await page.getByRole('tab',{name:'Lineup',exact:true}).click();
        await live.locator('.fm-lineup-pitch [data-player-id="Forward"]').waitFor();
        assert.match(await page.locator('#counts').innerText(),/^9 2 3 /);
      }
      console.log('PASS',width,'px, both views, source reads/writes/store attempts/DB reads',await page.locator('#counts').innerText());
      assert.equal((await page.locator('#xi_checks').innerText()).trim(),'1');
      await page.close();
    }
  } finally { await browser.close(); }
})().catch(e=>{console.error(e);process.exitCode=1});
