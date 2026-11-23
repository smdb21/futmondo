const assert = require('node:assert/strict');
const path = require('node:path');
const { chromium } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules','playwright'));
(async () => {
  const browser = await chromium.launch({headless:true,args:['--no-sandbox']});
  try {
    for (const width of [320,390,1440]) for (const theme of ['dark','light']) {
      const page = await browser.newPage({viewport:{width,height:1000}});
      const errors=[];page.on('pageerror',error=>errors.push(error.message));
      await page.route('**/*',route=>route.request().url().startsWith('http://127.0.0.1:3917/') ? route.continue() :
        route.fulfill({contentType:'image/svg+xml',body:'<svg xmlns="http://www.w3.org/2000/svg" width="22" height="22"><path fill="red" d="M0 0h22v22H0z"/></svg>'}));
      await page.goto('http://127.0.0.1:3917/');
      await page.evaluate(theme=>document.documentElement.dataset.fmTheme=theme,theme);
      const table=page.locator('#fixture-players_table');
      await table.locator('.rt-tbody .fm-player-name').first().waitFor();
      const marked=table.locator('.rt-tbody .fm-player-name').filter({hasText:/^Synthetic player 1$/});
      const badge=marked.locator('.fm-player-status');
      assert.equal(await badge.getAttribute('aria-label'),'Injured');
      assert.equal(await marked.innerText(),'Synthetic player 1');
      assert.equal(await table.locator('.rt-tbody .fm-player-name').filter({hasText:/^Synthetic player 2$/}).locator('.fm-player-status').count(),0);
      assert.equal(await table.locator('.rt-th').filter({hasText:/^Status$/}).count(),0);
      const placement=await marked.evaluate(cell=>{
        const label=cell.querySelector('.fm-player-name-with-status > span:first-child').getBoundingClientRect();
        const badge=cell.querySelector('.fm-player-status').getBoundingClientRect();
        return badge.left >= label.right && badge.right <= cell.getBoundingClientRect().right;
      });
      assert.ok(placement,'status icon sits after the name inside its cell');
      const geometry=()=>table.evaluate(el=>{
        const scroll=el.querySelector('.rt-table');
        const bounds=e=>{const r=e.getBoundingClientRect();return {left:r.left,right:r.right,width:r.width};};
        const row=el.querySelector('.rt-tbody .rt-tr');
        return {scroll:bounds(scroll),scrollLeft:scroll.scrollLeft,crestCell:bounds(row.querySelector('.fm-player-crest')),name:bounds(row.querySelector('.fm-player-name')),
          position:bounds(row.querySelector('.fm-player-position')),average:bounds(row.querySelector('.fm-player-average')),crest:bounds(row.querySelector('.fm-team-crest')),
          header:bounds(el.querySelector('.rt-th.fm-player-average')),
          filter:bounds(el.querySelector('.rt-td-filter.fm-player-average')),
          cells:Array.from(row.children).map(e=>({text:e.textContent,...bounds(e)}))};
      });
      const trendHeader=table.locator('.rt-th').filter({hasText:'× Trend'});
      const signed=await trendHeader.evaluate(header=>{
        const index=Array.from(header.parentElement.children).indexOf(header);
        const canvas=document.createElement('canvas');canvas.width=canvas.height=1;
        const context=canvas.getContext('2d');
        const rgb=color=>{context.clearRect(0,0,1,1);context.fillStyle=color;context.fillRect(0,0,1,1);return Array.from(context.getImageData(0,0,1,1).data).slice(0,3);};
        return {width:header.getBoundingClientRect().width,header:header.textContent,
          surface:rgb(getComputedStyle(document.documentElement).getPropertyValue('--fm-surface').trim()),
          rows:Array.from(header.closest('#fixture-players_table').querySelectorAll('.rt-tbody .rt-tr')).slice(0,4).map(row=>{
            const badge=row.children[index].querySelector('.fm-metric-badge');
            return {text:badge.textContent,color:rgb(getComputedStyle(badge).backgroundColor)};
          })};
      });
      assert.equal(signed.width,104,'trend column is narrower than its former 145px width');
      assert.match(signed.header,/Avg 5.*× Trend.*€M/);
      assert.deepEqual(signed.rows.map(row=>row.text).sort((a,b)=>Number(a)-Number(b)),['-0.148','0.000','0.074','0.222']);
      for(const [index,row] of signed.rows.entries()){
        const value=Number(row.text);
        const target=value<0?[255,107,107]:[124,255,155],t=Math.abs(value)/0.222;
        const expected=signed.surface.map((channel,i)=>channel+(target[i]-channel)*t);
        assert.ok(expected.every((channel,i)=>Math.abs(channel-signed.rows[index].color[i])<2),'signed trend uses neutral zero and symmetric red/green intensity');
      }
      if(width===390){
        await trendHeader.evaluate(el=>el.scrollIntoView({block:'nearest',inline:'center'}));
        await page.screenshot({path:`/tmp/futmondo-trend-${theme}.png`,fullPage:true});
        await table.locator('.rt-table').evaluate(el=>el.scrollLeft=0);
      }
      const initial=await geometry();
      const join=await table.locator('.rt-tbody .rt-tr').first().evaluate(row=>{
        const crest=getComputedStyle(row.querySelector('.fm-player-crest'));
        const name=getComputedStyle(row.querySelector('.fm-player-name'));
        return {crestBorder:crest.borderRightWidth,nameBorder:name.borderLeftWidth,crestBg:crest.backgroundColor,nameBg:name.backgroundColor};
      });
      assert.equal(join.crestBorder,'0px');assert.equal(join.nameBorder,'0px');
      assert.equal(join.crestBg,join.nameBg,'seamless crest/name background');
      if(width===390)await page.screenshot({path:`/tmp/futmondo-sticky-initial-${theme}.png`,fullPage:true});
      await table.locator('.rt-table').evaluate(el=>{el.scrollLeft=el.scrollWidth;});
      await page.waitForTimeout(100);
      let scrolled=await geometry();
      assert.ok(scrolled.scrollLeft>100,'fixture must scroll');
      for(const key of ['crestCell','crest']) {
        assert.ok(Math.abs(initial[key].left-scrolled[key].left)<2,`${width}/${theme} pinned ${key}`);
        assert.ok(scrolled[key].right<=scrolled.scroll.right+1,`${width}/${theme} ${key} visible`);
      }
      assert.ok(scrolled.cells.some((cell,i)=>!['crestCell','average'].some(key=>Math.abs(initial[key].left-cell.left)<1)&&Math.abs(cell.left-initial.cells[i].left)>100),'stats move');
      // Cells must paint opaque backgrounds even on the departure/striped rows.
      const backgrounds=await table.locator('.rt-tbody .rt-tr').evaluateAll(rows=>rows.slice(0,3).map(row=>
        ['.fm-player-crest','.fm-player-average'].map(selector=>getComputedStyle(row.querySelector(selector)).backgroundColor)));
      assert.ok(backgrounds.flat().every(color=>color!=='rgba(0, 0, 0, 0)'&&color!=='transparent'));
      assert.notEqual(backgrounds[0][0],backgrounds[1][0],'striping stays visible');
      const departing=await table.locator('.fm-departing-before-round .fm-player-crest').evaluate(el=>getComputedStyle(el).backgroundColor);
      assert.ok(!backgrounds.flat().includes(departing),'departure tint stays visible');
      // A data rerender retains sticky definitions when scrolled again.
      await page.locator('#change_rows').click();
      await page.waitForTimeout(300);
      await table.locator('.rt-table').evaluate(el=>{el.scrollLeft=el.scrollWidth;});
      scrolled=await geometry();assert.ok(scrolled.scrollLeft>100);
      assert.ok(Math.abs(scrolled.average.left-scrolled.crestCell.right)<2,'average sticks beside crest');
      assert.ok(scrolled.name.right<=scrolled.crestCell.right,'name scrolls away');
      assert.ok(Math.abs((initial.position.left-scrolled.position.left)-scrolled.scrollLeft)<2,'position follows native horizontal scrolling');
      assert.equal(scrolled.average.width,84,'average is narrower than its former 110px width');
      for(const key of ['header','filter'])assert.ok(Math.abs(scrolled[key].left-scrolled.average.left)<2);
      // A player can still be selected from a pinned cell; heartbeat stays responsive.
      await table.locator('.rt-tbody .fm-player-crest').nth(1).click();
      await page.locator('#shiny-modal').waitFor();
      await page.locator('#shiny-modal [data-dismiss=modal]').last().click();
      await page.locator('#shiny-modal').waitFor({state:'hidden'});
      const start=Date.now();await page.locator('#heartbeat').click();
      await page.waitForFunction(()=>document.querySelector('#heartbeat_value').textContent==='1');
      const latency=Date.now()-start;assert.ok(latency<2000);
      // Changing N rebuilds the header while retaining the narrow pinned column.
      await page.locator('#fixture-recent_games_n').fill('20');
      await page.locator('#fixture-recent_games_n').press('Tab');
      await page.waitForFunction(()=>document.querySelector('.rt-th.fm-player-average')?.textContent.includes('20'));
      await table.locator('.rt-table').evaluate(el=>{el.scrollLeft=el.scrollWidth;});
      const changedN=await geometry();
      assert.ok(Math.abs(changedN.average.left-changedN.crestCell.right)<2,'N20 retains pinning');
      assert.equal(changedN.average.width,84,'N20 retains compact width');
      if(width===390)await page.screenshot({path:`/tmp/futmondo-average-N20-${theme}.png`,fullPage:true});
      await page.locator('#fixture-recent_games_n').fill('5');
      await page.locator('#fixture-recent_games_n').press('Tab');
      await page.waitForFunction(()=>document.querySelector('.rt-th.fm-player-average')?.textContent.includes('5'));
      await table.locator('.rt-table').evaluate(el=>{el.scrollLeft=el.scrollWidth;});
      if(width===1440){
        const before=await geometry();
        const handle=table.locator('.rt-th.fm-player-average .rt-resizer');
        const box=await handle.boundingBox();assert.ok(box);
        await page.mouse.move(box.x+box.width/2,box.y+box.height/2);await page.mouse.down();
        await page.mouse.move(box.x+box.width/2+40,box.y+box.height/2,{steps:5});
        await page.waitForTimeout(100);await page.mouse.up();
        const resized=await geometry();
        assert.ok(resized.average.width>before.average.width+20,'average resizes');
        assert.ok(Math.abs(resized.average.left-resized.crestCell.right)<2,'resized sticky offset');
      }
      assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+1),false,'no page overflow');
      assert.deepEqual(errors,[]);assert.deepEqual(await page.locator('.shiny-output-error:visible').allTextContents(),[]);
      await page.screenshot({path:`/tmp/futmondo-sticky-${width}-${theme}.png`,fullPage:true});
      console.log(`PASS ${width}px ${theme}: 104px signed trend, pinned crest/average/header/filter, scrolling position, N20, row states, update, selection; heartbeat ${latency}ms`);
      await page.close();
    }
  } finally {await browser.close();}
})().catch(error=>{console.error(error);process.exit(1);});
