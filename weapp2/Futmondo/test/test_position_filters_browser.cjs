const assert=require('node:assert/strict');
const path=require('node:path');
const {chromium}=require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules','playwright'));
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:process.env.FUTMONDO_TEST_CHROMIUM||undefined,args:['--no-sandbox']});
 try{
  for(const width of [320,390,1440])for(const theme of ['dark','light']){
   const page=await browser.newPage({viewport:{width,height:1000}});
   const errors=[];page.on('pageerror',error=>errors.push(error.message));
   await page.route('**/*',route=>route.request().url().startsWith('http://127.0.0.1:3933/')?route.continue():route.abort());
   await page.goto('http://127.0.0.1:3933/');
   await page.evaluate(theme=>document.documentElement.dataset.fmTheme=theme,theme);
   await page.locator('#plain-players_table .fm-player-name').first().waitFor();
   for(const id of ['plain','squad']){
    const table=page.locator(`#${id}-players_table`);
    const names=()=>table.locator('.rt-tbody .fm-player-name').allTextContents();
    const expectRows=async expected=>{
     await page.waitForFunction(({id,expected})=>Array.from(document.querySelectorAll(`#${id}-players_table .rt-tbody .fm-player-name`))
      .map(el=>el.textContent).sort().join('|')===expected.sort().join('|'),{id,expected});
    };
    const select=page.locator(`#${id}-position_filter`);
    const wrapper=select.locator('..').locator('.selectize-control');
    const items=()=>select.evaluate(el=>el.selectize.items.slice());
    const choose=async value=>{
     await wrapper.locator('.selectize-input').click();
     await wrapper.locator(`.selectize-dropdown [data-value="${value}"]`).click();
    };
    await page.locator(`#${id}-filters summary`).click();
    assert.deepEqual(await items(),['All']);
    await expectRows(['Keeper','Defender','Midfielder','Forward','Dual','Unknown']);
    await choose('Defender');assert.deepEqual(await items(),['Defender']);
    await expectRows(['Defender','Midfielder']);
    await choose('Forward');assert.deepEqual(await items(),['Defender','Forward']);
    await expectRows(['Defender','Midfielder','Forward','Dual']);
    await choose('All');assert.deepEqual(await items(),['All']);
    await expectRows(['Keeper','Defender','Midfielder','Forward','Dual','Unknown']);
    // Exercise actual rapid additions and the server's quiet period.
    await choose('Goalkeeper');await choose('Defender');await choose('Forward');
    assert.deepEqual(await items(),['Goalkeeper','Defender','Forward']);
    await page.waitForTimeout(100);
    assert.deepEqual((await names()).sort(),['Keeper','Defender','Midfielder','Forward','Dual','Unknown'].sort());
    await expectRows(['Keeper','Defender','Midfielder','Forward','Dual']);
    // Removing the final chip restores All immediately.
    await select.evaluate(el=>el.selectize.clear());assert.deepEqual(await items(),['All']);
    await expectRows(['Keeper','Defender','Midfielder','Forward','Dual','Unknown']);
    await choose('Forward');await expectRows(['Forward','Dual']);
    await wrapper.locator('.selectize-input input').press('Backspace');
    assert.deepEqual(await items(),['All']);
    await expectRows(['Keeper','Defender','Midfielder','Forward','Dual','Unknown']);
    // All four concrete positions must wrap within the responsive control.
    await choose('Goalkeeper');await choose('Defender');await choose('Midfielder');await choose('Forward');
    assert.equal((await items()).length,4);
    const overflow=await wrapper.evaluate(el=>{const r=el.getBoundingClientRect();return r.right>innerWidth+1||r.left<0||el.scrollWidth>el.clientWidth+1;});
    assert.equal(overflow,false,'multiselect chips fit their control');
    if(width===390)await page.screenshot({path:`/tmp/futmondo-position-multiple-${id}-${theme}.png`,fullPage:true});
    await page.locator(`#${id}-btn_clear_filters`).click();
    await page.waitForFunction(id=>document.getElementById(`${id}-position_filter`).selectize.items.join()==='All',id);
    await expectRows(['Keeper','Defender','Midfielder','Forward','Dual','Unknown']);
    // The second table keeps its own selection throughout the first's changes.
    const other=id==='plain'?'squad':'plain';
    assert.deepEqual(await page.locator(`#${other}-position_filter`).evaluate(el=>el.selectize.items.slice()),['All']);
    if(width===390)await page.screenshot({path:`/tmp/futmondo-position-${id}-${theme}.png`,fullPage:true});
   }
   const begin=Date.now();await page.locator('#heartbeat').click();
   await page.waitForFunction(()=>document.querySelector('#heartbeat_value').textContent==='1');
   const heartbeat=Date.now()-begin;assert.ok(heartbeat<1500);
   assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+1),false,'no horizontal page overflow');
   assert.deepEqual(errors,[]);assert.deepEqual(await page.locator('.shiny-output-error:visible').allTextContents(),[]);
   console.log(`PASS ${width}px ${theme}: both layouts, All exclusivity, role union, debounce, clear/reset, isolation, wrapping; heartbeat ${heartbeat}ms`);
   await page.close();
  }
 }finally{await browser.close();}
})().catch(error=>{console.error(error);process.exit(1);});
