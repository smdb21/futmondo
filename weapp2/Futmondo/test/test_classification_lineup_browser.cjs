// Start: Rscript test/test_classification_lineup_browser.R (from app directory).
const assert = require('node:assert/strict');
const path = require('node:path');
const {chromium} = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules', 'playwright'));
(async () => {
  const browser = await chromium.launch({headless:true,args:['--no-sandbox']});
  try {
    for (const width of [390,1440]) {
      const page = await browser.newPage({viewport:{width,height:1000}}), errors=[];
      page.on('pageerror', e => errors.push(e.message));
      await page.route('**/*', route => new URL(route.request().url()).hostname === '127.0.0.1' ? route.continue() : route.abort());
      await page.goto(`http://127.0.0.1:${process.env.FUTMONDO_LINEUP_TEST_PORT || '8501'}`);
      const table = page.locator('#classification-classification_table');
      await table.getByRole('row').filter({hasText:'Your Team'}).waitFor();
      assert.equal(await page.locator('#classification-single_round_select').count(),0);
      await page.getByRole('button',{name:'Round 11',exact:true}).click();
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
      await modal.getByRole('button',{name:'Close',exact:true}).click();
      // Sorting must preserve team identity; the keyboard path opens the rival.
      await table.getByRole('columnheader',{name:'Sort Team',exact:true}).click();
      const rival = table.getByRole('button',{name:'Rival Team',exact:true});
      await rival.focus(); await page.keyboard.press('Enter');
      await modal.getByText('Formation 3-6-1',{exact:true}).waitFor();
      assert.match(await modal.locator('.modal-title').innerText(),/Rival Team/);
      await modal.locator('.fm-lineup-pitch [data-player-id="rival-0"]').waitFor();
      await modal.getByRole('button',{name:'Close',exact:true}).click();
      await page.getByRole('button',{name:'Multiple rounds',exact:true}).click();
      await page.waitForFunction(()=>document.querySelectorAll('#classification-classification_table button.fm-lineup-open').length===0);
      assert.equal(await modal.count(),0);
      await page.getByRole('button',{name:'Round 9',exact:true}).click();
      await table.getByRole('button',{name:'Your Team',exact:true}).click();
      await modal.getByText('Formation 4-3-3',{exact:true}).waitFor();
      assert.deepEqual(await modal.locator('.fm-lineup-row').evaluateAll(nodes=>nodes.map(n=>n.querySelectorAll('.fm-lineup-player').length)),[3,3,4,1]);
      assert.equal(await page.locator('.shiny-output-error:visible').count(),0);
      assert.deepEqual(errors,[]);
      console.log(`PASS ${width}px: own/rival click, sorted identity, keyboard, 11+4 slots, holes, substitutions, 6 midfielders, selected formation and range guard`);
      await page.close();
    }
  } finally {await browser.close();}
})().catch(error=>{console.error(error.stack);process.exitCode=1;});
