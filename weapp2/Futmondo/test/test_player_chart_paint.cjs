const assert = require('node:assert/strict');
const { execFileSync } = require('node:child_process');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const { chromium } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules', 'playwright'));

(async () => {
  const temporary = fs.mkdtempSync(path.join(os.tmpdir(), 'futmondo-card-paint-'));
  let browser;
  try {
    const fixturePath = path.join(temporary, 'fixture.json');
    execFileSync('Rscript', ['-e', `
      options(futmondo.offline=TRUE)
      suppressPackageStartupMessages(source('global.R'))
      ui <- selected_player_UI('paint')
      jsonlite::write_json(list(script=as.character(ui[[1]]$children[[1]]),
        jquery=system.file('www/shared/jquery.min.js',package='shiny')),
        Sys.getenv('FUTMONDO_PAINT_FIXTURE'),auto_unbox=TRUE)
    `], { env: { ...process.env, FUTMONDO_PAINT_FIXTURE: fixturePath }, stdio: 'pipe' });
    const fixture = JSON.parse(fs.readFileSync(fixturePath, 'utf8'));
    browser = await chromium.launch({ headless: true, args: ['--no-sandbox'] });
    for (const width of [1440, 390]) {
      const page = await browser.newPage({ viewport: { width, height: 900 } });
      await page.setContent('<div id="paint-player_identity_summary"></div>');
      await page.addScriptTag({ path: fixture.jquery });
      await page.evaluate(() => {
        window.paints = [];
        window.Shiny = { setInputValue: (name, value) => window.paints.push({ name, ...value }) };
        window.identity = (player, generation) => {
          const node = document.createElement('div');
          node.className = 'player-card-identity';
          node.dataset.playerId = player;
          node.dataset.paintGeneration = generation;
          node.textContent = 'De Jong';
          document.getElementById('paint-player_identity_summary').replaceChildren(node);
        };
      });
      await page.addScriptTag({ content: fixture.script });
      // shiny:value precedes renderUI's asynchronous DOM insertion.
      await page.evaluate(() => {
        $(document).trigger({ type: 'shiny:value', name: 'paint-player_identity_summary' });
        setTimeout(() => identity('de-jong', '1'), 60);
      });
      await page.waitForFunction(() => paints.some(x => x.player === 'de-jong' && x.generation === '1'), null, { timeout: 3000 });
      // The same mounted output can receive a new context/paint generation.
      await page.evaluate(() => identity('de-jong', '2'));
      await page.waitForFunction(() => paints.some(x => x.generation === '2'), null, { timeout: 3000 });
      // Reopening can initially expose Shiny's old cached identity HTML.
      await page.addScriptTag({ content: fixture.script });
      await page.evaluate(() => {
        $(document).trigger({ type: 'shiny:value', name: 'paint-player_identity_summary' });
        setTimeout(() => identity('de-jong', '3'), 60);
      });
      await page.waitForFunction(() => paints.some(x => x.generation === '3'), null, { timeout: 3000 });
      // A card replaced between animation frames must not acknowledge old HTML.
      await page.evaluate(() => {
        identity('obsolete-player', '4');
        requestAnimationFrame(() => identity('de-jong', '5'));
      });
      await page.waitForFunction(() => paints.some(x => x.generation === '5'), null, { timeout: 3000 });
      await page.evaluate(() => {
        for (let i = 0; i < 5; i++) $(document).trigger({ type: 'shiny:value', name: 'paint-player_identity_summary' });
      });
      await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));
      const paints = await page.evaluate(() => window.paints);
      assert.ok(paints.every(x => x.name === 'paint-identity_painted'));
      assert.ok(!paints.some(x => x.player === 'obsolete-player'));
      assert.equal(paints.filter(x => x.generation === '3').length, 1);
      assert.equal(paints.filter(x => x.generation === '5').length, 1);
      console.log(`PASS ${width}px delayed identity, context change, reopen, obsolete paint rejection, duplicate suppression`);
      await page.close();
    }
  } finally {
    if (browser) await browser.close();
    fs.rmSync(temporary, { recursive: true, force: true });
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
