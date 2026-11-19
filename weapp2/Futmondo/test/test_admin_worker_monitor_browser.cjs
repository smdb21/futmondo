// Real Admin UI and server with synthetic delayed promises, never live accounts.
const assert = require('node:assert/strict');
const {chromium} = require((process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules') + '/playwright');
const url = 'http://127.0.0.1:' + (process.env.FUTMONDO_MONITOR_TEST_PORT || '3928');
const chromiumPath = process.env.FUTMONDO_TEST_CHROMIUM || undefined;
const monitor = '#admin-worker_monitor';
const refresh = '#admin-worker_monitor_refresh';
const count = async (page, id) => Number(await page.locator(id).innerText());
async function connect(browser, width=1440, theme='dark') {
  const page = await browser.newPage({viewport:{width,height:1000}}), errors=[];
  page.on('pageerror', error => errors.push(error.message));
  await page.route('**/*', route => new URL(route.request().url()).hostname === '127.0.0.1' ? route.continue() : route.abort());
  await page.goto(url);
  await page.waitForFunction(() => document.querySelector('#reads').textContent === '0' && !!window.Shiny);
  await page.evaluate(mode => document.documentElement.dataset.fmTheme = mode, theme);
  return {page,errors};
}
async function matrix(page) {
  await page.locator('.fm-worker-request-table').waitFor();
  for (const [route,expected] of [['/1/userteam/roster',['2','1']],['/1/player/summary',['0','1']]]) {
    const row = page.locator('.fm-worker-request-table tbody tr').filter({has:page.locator('code').filter({hasText:route})});
    assert.deepEqual(await row.locator('td:not(:first-child) span').allTextContents(),expected);
  }
}
async function noErrors(page, errors) {
  assert.deepEqual(errors,[]);
  assert.equal(await page.locator('.shiny-output-error').count(),0);
  assert.ok(!(await page.locator('body').innerText()).includes('PRIVATE'));
}
(async()=>{
  const browser = await chromium.launch({headless:true,executablePath:chromiumPath,args:['--no-sandbox']});
  try {
    for (const width of [320,390,1440]) for (const theme of ['dark','light']) {
      const {page,errors}=await connect(browser,width,theme);
      await page.selectOption('#request_delay','2');
      await page.locator('#toggle_admin').click();
      await page.locator(monitor).getByText('Checking worker activity…').waitFor();
      assert.equal(await count(page,'#reads'),1);
      const start=Date.now();
      await page.locator('#heartbeat').click();
      await page.waitForFunction(()=>document.querySelector('#beats').textContent==='1');
      const elapsed=Date.now()-start;
      assert.ok(elapsed<1000,`heartbeat blocked ${elapsed}ms during delayed monitor read`);
      assert.equal(await count(page,'#completed'),0);
      assert.equal(await page.locator(refresh).isDisabled(),true);
      await matrix(page);
      assert.deepEqual(await page.locator('.fm-worker-metric strong').allTextContents(),['2','2','4']);
      const colors=await page.locator('.fm-worker-request-table span[class^="fm-worker-count"]').evaluateAll(nodes=>nodes.map(n=>({
        text:n.textContent,kind:n.className,color:getComputedStyle(n).color,weight:getComputedStyle(n).fontWeight
      })));
      for(const c of colors){
        if(c.text==='0'){
          assert.equal(c.kind,'fm-worker-count-zero');assert.equal(c.weight,'400');
          assert.equal(c.color,theme==='dark'?'rgb(155, 191, 167)':'rgb(72, 96, 82)');
        }else{
          assert.equal(c.kind,'fm-worker-count-active');assert.equal(c.weight,'700');
          assert.equal(c.color,theme==='dark'?'rgb(124, 255, 155)':'rgb(20, 83, 45)');
        }
      }
      const metricRows=await page.locator('.fm-worker-metric').evaluateAll(nodes=>new Set(nodes.map(n=>n.getBoundingClientRect().top)).size);
      assert.equal(metricRows,width<480?3:1);
      const splitWords=await page.locator('.fm-worker-request-table th:not(:first-child)').evaluateAll(nodes=>nodes.flatMap(node=>{
        const split=[],walker=document.createTreeWalker(node,NodeFilter.SHOW_TEXT);let text;
        while(text=walker.nextNode())for(const match of text.textContent.matchAll(/\S+/g)){
          const range=document.createRange();range.setStart(text,match.index);range.setEnd(text,match.index+match[0].length);
          if(range.getClientRects().length>1)split.push(match[0]);
        }
        return split;
      }));
      assert.deepEqual(splitWords,[],'account headers must wrap between words on narrow phones');
      assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+2),false);
      const details=page.locator('.fm-worker-process-details');
      assert.equal(await details.getAttribute('open'),null);
      assert.ok((await page.locator('.fm-worker-request-table').boundingBox()).y<(await details.boundingBox()).y,
        'request matrix must precede optional worker details');
      assert.equal(await details.locator('.fm-worker-card').first().isVisible(),false);
      await details.locator('summary').focus();await page.keyboard.press('Enter');
      assert.equal(await details.locator('.fm-worker-card').first().isVisible(),true);
      await page.keyboard.press('Enter');
      await page.locator(monitor).screenshot({path:`/tmp/admin-worker-monitor-${width}-${theme}.png`});
      await page.selectOption('#request_delay','0.05');await page.selectOption('#response_mode','failure');
      await page.locator(refresh).click();
      await page.locator(monitor).getByText(/Showing the previous check/).waitFor();
      assert.equal(await page.locator('.fm-worker-count-active').count(),0);
      assert.equal(await page.locator('.fm-worker-count-stale').count(),4);
      assert.deepEqual(await page.locator('.fm-worker-metric strong').allTextContents(),['2','2','4']);
      await noErrors(page,errors);
      console.log(`PASS ${width}px ${theme}: route matrix 2/1 and 0/1; green positives/neutral zero; responsive ${elapsed}ms heartbeat during 2s read; labelled stale failure`);
      await page.close();
    }
    // Concurrent isolated sessions keep the real 15-second polling contract fast to check.
    await Promise.all(['hidden','navigation','permission'].map(async gate=>{
      const {page,errors}=await connect(browser);
      try{
        await page.locator('#toggle_admin').click();await matrix(page);
        await page.selectOption('#request_delay','2');await page.locator(refresh).click();
        await page.locator(monitor).getByText('Updating worker activity…').waitFor();
        const before=await count(page,'#reads'),completed=await count(page,'#completed');
        if(gate==='hidden')await page.evaluate(()=>{
          Object.defineProperty(document,'hidden',{configurable:true,get:()=>true});
          document.dispatchEvent(new Event('visibilitychange'));
        });
        else await page.locator(gate==='navigation'?'#toggle_admin':'#toggle_permission').click();
        await page.waitForFunction(()=>!document.querySelector('.fm-worker-monitor'));
        // Simulate a forged/stale browser refresh event after eligibility is lost.
        await page.evaluate(()=>Shiny.setInputValue('admin-worker_monitor_refresh',99,{priority:'event'}));
        await page.waitForFunction(n=>Number(document.querySelector('#completed').textContent)>n,completed);
        assert.equal(await page.locator('.fm-worker-monitor').count(),0,'late completion reappeared');
        await page.waitForTimeout(16000);
        assert.equal(await count(page,'#reads'),before,`${gate} continued monitor requests`);
        await noErrors(page,errors);
        console.log(`PASS ${gate}: ignores delayed response and forged refresh; no additional reads across real 15s poll`);
      }finally{await page.close();}
    }));
  }finally{await browser.close();}
})().catch(error=>{console.error(error.stack);process.exitCode=1;});
