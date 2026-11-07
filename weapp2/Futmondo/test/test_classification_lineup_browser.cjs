// Start: Rscript test/test_classification_lineup_browser.R (from app directory).
const assert = require('node:assert/strict');
const path = require('node:path');
const {chromium} = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules', 'playwright'));
(async () => {
  const browser = await chromium.launch({headless:true,args:['--no-sandbox']});
  try {
    for (const [width,height] of [[390,640],[360,640],[568,320],[1440,1000]]) {
      const phone=width<600;
      const page = await browser.newPage({viewport:{width,height},isMobile:phone,hasTouch:phone}), errors=[];
      page.on('pageerror', e => errors.push(e.message));
      await page.route('**/*', route => new URL(route.request().url()).hostname === '127.0.0.1' ? route.continue() : route.abort());
      await page.goto(`http://127.0.0.1:${process.env.FUTMONDO_LINEUP_TEST_PORT || '8501'}`);
      const table = page.locator('#classification-classification_table');
      await table.getByRole('row').filter({hasText:'Your Team'}).waitFor();
      assert.equal(await page.locator('#classification-single_round_select').count(),0);
      const rounds = page.locator('.fm-classification-rounds');
      await rounds.getByRole('button',{name:'Round 11',exact:true}).waitFor();
      assert.deepEqual(await rounds.locator('button').evaluateAll(nodes=>nodes.map(n=>Number(n.dataset.round))),[11,10,9,8,7,6,5,4,3,2,1]);
      assert.equal(await rounds.getByRole('button',{name:'Round 12',exact:true}).count(),0);
      assert.equal(await page.locator('#classification-round_range_slider').isVisible(),false);
      assert.equal(await rounds.locator('[aria-pressed="true"]').getAttribute('data-round'),'11');
      if(width<600){
        assert.ok(await rounds.evaluate(n=>n.scrollWidth>n.clientWidth));
        await rounds.evaluate(n=>n.scrollLeft=n.scrollWidth);
        assert.ok(await rounds.evaluate(n=>n.scrollLeft>0));
        await rounds.evaluate(n=>n.scrollLeft=0);
      }
      await page.screenshot({path:`/tmp/futmondo-round-buttons-${width}.png`,fullPage:true});
      await rounds.getByRole('button',{name:'Round 1',exact:true}).click();
      await page.waitForFunction(()=>document.querySelector('.fm-round-button[data-round="1"]').getAttribute('aria-pressed')==='true');
      if(width<600)assert.ok(await rounds.evaluate(n=>n.scrollLeft>0),'Selecting an older round must retain horizontal scroll');
      await rounds.getByRole('button',{name:'Round 11',exact:true}).click();
      await table.getByRole('button',{name:'Your Team',exact:true}).waitFor();
      await table.getByRole('row').filter({hasText:'Your Team'}).getByRole('cell',{name:'105',exact:true}).click();
      const modal = page.locator('.modal');
      await modal.getByText('Formation 3-6-1',{exact:true}).waitFor();
      assert.equal(await modal.locator('.fm-lineup-pitch .fm-lineup-player').count(),11);
      assert.equal(await modal.locator('.fm-lineup-row').nth(1).locator('.fm-lineup-player').count(),6);
      assert.equal(await modal.locator('.fm-lineup-pitch .fm-lineup-empty').count(),1);
      assert.equal(await modal.locator('.fm-lineup-bench-row .fm-lineup-player').count(),4);
      assert.deepEqual(await modal.locator('.fm-lineup-bench-row .fm-lineup-player').evaluateAll(nodes=>nodes.map(n=>n.dataset.playerId)),['bench-0','','bench-2','bench-3']);
      assert.equal(await modal.locator('.fm-lineup-pitch .fm-lineup-in').count(),1);
      assert.equal(await modal.locator('.fm-lineup-bench-row .fm-lineup-out').count(),1);
      const pitch = await modal.locator('.fm-lineup-pitch').boundingBox();
      assert.ok(pitch.width <= width && pitch.x>=0 && pitch.x+pitch.width<=width);
      const overflow = await modal.locator('.fm-lineup-player').evaluateAll(nodes => nodes.some(n=>n.scrollWidth>n.clientWidth+1));
      assert.equal(overflow,false);
      await page.screenshot({path:`/tmp/futmondo-round-roster-${width}.png`,fullPage:true});
      if(phone){
        const close=modal.getByRole('button',{name:'Close lineup',exact:true});
        const reachable=async()=>{
          // Reopened cached content can arrive before Bootstrap's opening transition ends.
          await page.waitForFunction(()=>{
            const n=document.querySelector('.fm-lineup-close-shortcut .btn');
            if(!n)return false;const b=n.getBoundingClientRect();
            return b.y>=0&&b.bottom<=window.innerHeight;
          },null,{timeout:5000});
          const box=await close.boundingBox();
          assert.ok(box&&box.y>=0&&box.y+box.height<=height,'Close must remain inside the phone viewport');
          assert.ok(box.width>=44&&box.height>=44,'Close must have a usable touch target');
          assert.ok(await close.evaluate(n=>{const b=n.getBoundingClientRect();return n.contains(document.elementFromPoint(b.x+b.width/2,b.y+b.height/2));}),'Close must receive touches');
        };
        await reachable();
        await close.tap();
        await modal.waitFor({state:'detached'});
        await table.getByRole('button',{name:'Your Team',exact:true}).tap();
        await modal.getByText('Formation 3-6-1',{exact:true}).waitFor();
        await modal.evaluate(n=>n.scrollTop=(n.scrollHeight-n.clientHeight)/2);
        await reachable();
        await page.screenshot({path:`/tmp/futmondo-lineup-close-${width}x${height}.png`});
        await close.tap();
      }else await modal.getByRole('button',{name:'Close',exact:true}).click();
      await modal.waitFor({state:'detached'});
      // Sorting must preserve team identity; the keyboard path opens the rival.
      await table.getByRole('columnheader',{name:'Sort Team',exact:true}).click();
      const rival = table.getByRole('button',{name:'Rival Team',exact:true});
      await rival.focus(); await page.keyboard.press('Enter');
      await modal.getByText('Formation 3-6-1',{exact:true}).waitFor();
      assert.match(await modal.locator('.modal-title').innerText(),/Rival Team/);
      await modal.locator('.fm-lineup-pitch [data-player-id="rival-0"]').waitFor();
      await modal.getByRole('button',{name:'Close',exact:true}).click();
      await page.getByRole('tab',{name:'Partial classifications',exact:true}).click();
      await page.locator('#classification-round_range_slider').waitFor({state:'visible'});
      assert.equal(await page.locator('#classification-dreamteam_box_ui').isVisible(),false);
      await page.evaluate(()=>Shiny.setInputValue('classification-round_range_slider',[9,9],{priority:'event'}));
      await table.getByRole('row').filter({hasText:'Your Team'}).getByRole('cell',{name:'72',exact:true}).waitFor();
      await page.waitForFunction(()=>document.querySelectorAll('#classification-classification_table button.fm-lineup-open').length===0);
      assert.equal(await modal.count(),0);
      await page.getByRole('tab',{name:'Round classifications',exact:true}).click();
      assert.equal(await rounds.locator('[aria-pressed="true"]').getAttribute('data-round'),'11');
      await rounds.getByRole('button',{name:'Round 9',exact:true}).click();
      await rounds.locator('[data-round="9"][aria-pressed="true"]').waitFor();
      await table.getByRole('row').filter({hasText:'Your Team'}).getByRole('cell',{name:'72',exact:true}).waitFor();
      await page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve))));
      await table.getByRole('button',{name:'Your Team',exact:true}).click();
      await modal.getByText('Formation 4-3-3',{exact:true}).waitFor();
      assert.deepEqual(await modal.locator('.fm-lineup-row').evaluateAll(nodes=>nodes.map(n=>n.querySelectorAll('.fm-lineup-player').length)),[3,3,4,1]);
      assert.equal(await page.locator('.shiny-output-error:visible').count(),0);
      assert.deepEqual(errors,[]);
      console.log(`PASS ${width}x${height}: ${phone?'touch close before/after scroll':'desktop footer close'}, own/rival click, sorted identity, keyboard, 11+4 slots, holes, substitutions, 6 midfielders, selected formation and range guard`);
      await page.close();
    }
  } finally {await browser.close();}
})().catch(error=>{console.error(error.stack);process.exitCode=1;});
