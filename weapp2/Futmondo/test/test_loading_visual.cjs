// Start test_loading_visual_app.R, then run this file. Add --full-app to both
// commands for the real offline application startup acceptance checks.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const zlib = require('node:zlib');
const { chromium } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules', 'playwright'));
const fullApp = process.argv.includes('--full-app');
const origin = `http://127.0.0.1:${fullApp ? 38768 : 38767}`;
const screenshotDir = process.env.FUTMONDO_LOADING_SCREENSHOTS || '/tmp/futmondo-loading-visual';
fs.mkdirSync(screenshotDir, { recursive: true });

function instrument() {
  const stats = window.loadingFixture = {draws: {}, samples: [], stageSamples: [], callbacks: 0, pending: new Set(), maxPending: 0, colors: {}, capture: false, geometry: []};
  const nativeRequest = window.requestAnimationFrame.bind(window);
  const nativeCancel = window.cancelAnimationFrame.bind(window);
  const nativeClear = CanvasRenderingContext2D.prototype.clearRect;
  const nativeStroke = CanvasRenderingContext2D.prototype.stroke;
  const nativeFill = CanvasRenderingContext2D.prototype.fill;
  const paths = new WeakMap();
  function id(context) { return context.canvas.closest('[data-fm-loading]')?.id; }
  function trace(context) { return stats.capture && id(context) === 'fixture-hero'; }
  for (const name of ['beginPath', 'moveTo', 'lineTo', 'arc', 'ellipse']) {
    const original = CanvasRenderingContext2D.prototype[name];
    CanvasRenderingContext2D.prototype[name] = function(...args) {
      if (trace(this)) {
        if (name === 'beginPath') paths.set(this, []);
        else if (paths.has(this)) paths.get(this).push({op: name, args});
      }
      return original.apply(this, args);
    };
  }
  function record(context, kind) {
    if (trace(context) && stats.geometry.length < 500)
      stats.geometry.push({kind, alpha: context.globalAlpha,
        color: kind === 'fill' ? context.fillStyle : context.strokeStyle,
        path: paths.get(context) || []});
  }
  CanvasRenderingContext2D.prototype.clearRect = function(...args) {
    const key = id(this);
    if (key) {
      stats.draws[key] = (stats.draws[key] || 0) + 1;
      if (stats.firstDrawAt === undefined) stats.firstDrawAt = performance.now();
      if (stats.capture && key === 'fixture-hero') stats.geometry = [];
    }
    return nativeClear.apply(this, args);
  };
  CanvasRenderingContext2D.prototype.stroke = function(...args) {
    const key = id(this);
    if (key) stats.colors[key] = this.strokeStyle;
    record(this, 'stroke');
    return nativeStroke.apply(this, args);
  };
  CanvasRenderingContext2D.prototype.fill = function(...args) {
    record(this, 'fill');
    return nativeFill.apply(this, args);
  };
  window.requestAnimationFrame = callback => {
    const handle = nativeRequest(timestamp => {
      stats.pending.delete(handle);
      stats.callbacks++;
      const before = Object.values(stats.draws).reduce((sum, n) => sum + n, 0);
      const started = performance.now();
      try { return callback(timestamp); }
      finally {
        if (Object.values(stats.draws).reduce((sum, n) => sum + n, 0) > before && !stats.capture && stats.samples.length < 10000) {
          const duration = performance.now() - started;
          stats.samples.push(duration);
          const phase = (timestamp - stats.firstDrawAt + 15000) % 15000;
          if (phase >= 1800 && phase <= 5000) stats.stageSamples.push(duration);
        }
      }
    });
    stats.pending.add(handle);
    stats.maxPending = Math.max(stats.maxPending, stats.pending.size);
    return handle;
  };
  window.cancelAnimationFrame = handle => { stats.pending.delete(handle); nativeCancel(handle); };
}

async function createPage(browser, width = 1440, options = {}) {
  const page = await browser.newPage({viewport: {width, height: 900}, colorScheme: 'dark', ...options});
  page.setDefaultTimeout(20000);
  const errors = [], external = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.route('**/*', route => {
    // shinyApp object fixtures do not mount the project's www directory; serve
    // this exact unchanged local asset as the deployed application does.
    if (route.request().url() === origin + '/remember-login.js')
      return route.fulfill({path: path.resolve('www/remember-login.js'), contentType: 'application/javascript'});
    if (route.request().url().startsWith(origin + '/')) return route.continue();
    external.push(route.request().url());
    return route.abort();
  });
  await page.addInitScript(instrument);
  return {page, errors, external};
}
async function draws(page, key) {
  return page.evaluate(key => window.loadingFixture.draws[key] || 0, key);
}
async function waitDraw(page, key) {
  const before = await draws(page, key);
  await page.waitForFunction(({key, before}) => (window.loadingFixture.draws[key] || 0) > before, {key, before});
}
async function stopped(page, keys) {
  await page.waitForTimeout(350); // Permit observer delivery and the last in-flight frame.
  const before = await page.evaluate(keys => keys.map(key => window.loadingFixture.draws[key] || 0), keys);
  await page.waitForTimeout(300);
  const after = await page.evaluate(keys => keys.map(key => window.loadingFixture.draws[key] || 0), keys);
  assert.deepEqual(after, before, 'Hidden/removed loading graphics stop drawing');
}
async function noOverflow(page) {
  assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 2), false, 'No horizontal page overflow');
}
function p95(values) {
  const sorted = [...values].sort((a, b) => a - b);
  return sorted[Math.floor((sorted.length - 1) * 0.95)] || 0;
}
async function captureGeometry(page) {
  await page.evaluate(() => { window.loadingFixture.capture = true; });
  await waitDraw(page, 'fixture-hero');
  return page.evaluate(() => {
    window.loadingFixture.capture = false;
    return window.loadingFixture.geometry;
  });
}
function pitchCorners(geometry) {
  const pitch = geometry.find(mark => mark.kind === 'stroke' && mark.path.length > 50);
  assert.ok(pitch, 'Pitch markings remain a connected projected field');
  return pitch.path.slice(0, 4).map(command => command.args.slice(0, 2));
}
function polygonArea(points) {
  return Math.abs(points.reduce((sum, point, i) => {
    const next = points[(i + 1) % points.length];
    return sum + point[0] * next[1] - next[0] * point[1];
  }, 0)) / 2;
}
function checkGeometry(phases) {
  const flat = pitchCorners(phases.pitch);
  const expected = [[50, 40], [370, 40], [370, 240], [50, 240]];
  for (let i = 0; i < 4; i++) for (let axis = 0; axis < 2; axis++)
    assert.ok(Math.abs(flat[i][axis] - expected[i][axis]) < 1, 'Flat pitch retains its known corner positions');
  const tilted = pitchCorners(phases.tilted);
  assert.ok(Math.abs(tilted[0][1] - tilted[1][1]) > 5, 'The rotated pitch edge is visibly sloped');
  assert.ok(polygonArea(tilted) < polygonArea(flat) * 0.85, 'The tilted ground plane is foreshortened');
  for (const geometry of Object.values(phases)) for (const mark of geometry) for (const command of mark.path)
    assert.ok(command.args.every(value => typeof value === 'boolean' || Number.isFinite(value)), 'Canvas coordinates stay finite');
  for (const name of ['pitch', 'tilted', 'rising', 'morph', 'flock']) for (const mark of phases[name]) for (const command of mark.path) {
    const [x, y] = command.args;
    assert.ok(x >= 0 && x <= 420 && y >= 0 && y <= 280, `Projected marks stay inside the scene (${x}, ${y})`);
  }
  const heads = phases.rising.filter(mark => mark.kind === 'fill' && mark.path.length === 1 &&
    mark.path[0].op === 'arc' && mark.path[0].args[2] > 1.5).map(mark => mark.path[0].args);
  const feet = phases.rising.filter(mark => mark.kind === 'fill' && mark.path.length === 1 &&
    mark.path[0].op === 'ellipse' && mark.path[0].args[2] > 2 && mark.path[0].args[2] < 15).map(mark => mark.path[0].args);
  assert.equal(heads.length, 22, 'Both teams have eleven raised player heads');
  assert.equal(feet.length, 22, 'Every raised player retains one ground anchor');
  for (let i = 0; i < 22; i++) assert.ok(heads[i][1] < feet[i][1] - 4, 'A player head rises visibly above its ground anchor');
}
function checkTeams(phases, teamColors) {
  assert.notEqual(teamColors[0], teamColors[1], 'The two teams use different theme-derived tones');
  function countTeams(marks, expected, label) {
    assert.equal(marks.length, expected * 2, label + ' has one mark per player');
    for (const color of teamColors)
      assert.equal(marks.filter(mark => mark.color === color).length, expected, label + ' retains each team tone');
  }
  const heads = phases.rising.filter(mark => mark.kind === 'fill' && mark.path.length === 1 &&
    mark.path[0].op === 'arc' && mark.path[0].args[2] > 1.5);
  countTeams(heads, 11, 'Raised heads');
  const bodies = phases.rising.filter(mark => mark.kind === 'fill' && mark.alpha > 0.65 &&
    teamColors.includes(mark.color) && mark.path.length >= 8 &&
    mark.path.every(command => command.op === 'moveTo' || command.op === 'lineTo'));
  countTeams(bodies, 11, 'Solid foosball bodies');
  for (const body of bodies) {
    const points = body.path.map(command => command.args);
    const xs = points.map(point => point[0]), ys = points.map(point => point[1]);
    const width = Math.max(...xs) - Math.min(...xs), height = Math.max(...ys) - Math.min(...ys);
    assert.ok(width >= 5 && width <= 25 && height >= 10 && height <= 40, 'Foosball bodies have a substantial filled silhouette');
    assert.ok(polygonArea(points) > width * height * 0.3, 'The player is a solid molded body, not a thin stick figure');
    const foot = points.filter(point => point[1] >= Math.max(...ys) - height * 0.2);
    const footWidth = Math.max(...foot.map(point => point[0])) - Math.min(...foot.map(point => point[0]));
    assert.ok(footWidth >= width * 0.6, 'Every foosball figure has a broad kicking foot');
  }
  const radarDots = phases.players.filter(mark => mark.path.length === 1 && mark.path[0].op === 'arc' &&
    mark.path[0].args[2] >= 2.3 && mark.path[0].args[2] <= 3.2 && teamColors.includes(mark.color));
  countTeams(radarDots, 11, 'Radar player dots');
  for (const name of ['pitch', 'morph', 'flock']) {
    const particles = phases[name].filter(mark => mark.kind === 'fill' && mark.alpha > 0.4 &&
      mark.path.length === 1 && mark.path[0].op === 'ellipse');
    assert.ok(particles.length >= 180, name + ' reuses the bounded particle pool');
    assert.deepEqual([...new Set(particles.map(mark => mark.color))].sort(), [...teamColors].sort(),
      name + ' keeps both team tones through the transformation');
  }
}

async function visualChecks(browser) {
  for (const width of [1440, 390, 320]) {
    const {page, errors, external} = await createPage(browser, width);
    await page.goto(origin + '/');
    await waitDraw(page, 'fixture-hero');
    const geometry = await page.locator('#fixture-hero canvas').evaluate(canvas => ({width: canvas.width, css: canvas.getBoundingClientRect().width}));
    assert.ok(geometry.width <= geometry.css * 1.5 + 2, 'Canvas device pixel ratio is bounded');
    await noOverflow(page);
    assert.equal(await page.locator('#fixture-hero').getAttribute('aria-hidden'), 'true');
    for (const theme of ['dark', 'light']) {
      await page.evaluate(theme => localStorage.setItem('futmondo.color-theme', theme), theme);
      // Reload resets the shared animation epoch as well as the real markup.
      await page.reload();
      await waitDraw(page, 'fixture-hero');
      const teamColors = await page.evaluate(() => {
        const style = getComputedStyle(document.documentElement);
        const context = document.createElement('canvas').getContext('2d');
        return ['text', 'warning'].map(name => {
          context.fillStyle = style.getPropertyValue('--fm-' + name).trim();
          return context.fillStyle;
        });
      });
      const pictures = [], phases = {};
      for (const [name, at] of [['pitch', 700], ['tilted', 2400], ['rising', 4200], ['morph', 5400], ['flock', 7000], ['radar', 9300], ['players', 11500], ['return', 14300]]) {
        const elapsed = await page.evaluate(() => performance.now() - window.loadingFixture.firstDrawAt);
        await page.waitForTimeout(Math.max(0, at - elapsed));
        phases[name] = await captureGeometry(page);
        pictures.push(await page.locator('#fixture-hero canvas').evaluate(canvas => canvas.toDataURL()));
        await page.locator('#fixture-parent').screenshot({path: path.join(screenshotDir, `${width}-${theme}-${name}.png`)});
        if (['tilted', 'rising', 'morph'].includes(name))
          await page.locator('#fixture-compact-parent').screenshot({path: path.join(screenshotDir, `${width}-${theme}-${name}-compact.png`)});
      }
      assert.equal(new Set(pictures).size, pictures.length, 'Each loop phase renders changing graphics');
      checkGeometry(phases);
      checkTeams(phases, teamColors);
      const seamElapsed = await page.evaluate(() => performance.now() - window.loadingFixture.firstDrawAt);
      await page.waitForTimeout(Math.max(0, 15100 - seamElapsed));
      const seam = await captureGeometry(page);
      assert.deepEqual(seam, phases.pitch, 'The loop returns to identical field and particle geometry, including mark orientation');
      assert.equal(await page.locator('#fixture-hero canvas').evaluate(canvas => canvas.toDataURL()), pictures[0], 'The complete canvas is identical across the flat loop seam');
      await noOverflow(page);
      console.log(`PASS ${width}px ${theme}: eight loop phases, 22 solid foosball figures, broad feet, 11 green/11 gold players, persistent team tones, exact seam`);
    }
    const responseMs = await page.evaluate(() => new Promise(resolve => {
      const start = performance.now();
      document.getElementById('fixture-ping').click();
      requestAnimationFrame(() => resolve(performance.now() - start));
    }));
    assert.ok(responseMs < 500, `Control responds while drawing (${responseMs}ms)`);
    await page.locator('#fixture-input').fill('Keyboard remains responsive');
    assert.equal(await page.locator('#fixture-input').inputValue(), 'Keyboard remains responsive');
    const normalSamples = await page.evaluate(() => window.loadingFixture.samples.splice(0));
    const normalStageSamples = await page.evaluate(() => window.loadingFixture.stageSamples.splice(0));
    const cdp = await page.context().newCDPSession(page);
    const untilStage = await page.evaluate(() => (2000 - ((performance.now() - window.loadingFixture.firstDrawAt) % 15000) + 15000) % 15000);
    await page.waitForTimeout(untilStage);
    await page.evaluate(() => { window.loadingFixture.samples = []; window.loadingFixture.stageSamples = []; });
    await cdp.send('Emulation.setCPUThrottlingRate', {rate: 4});
    await page.waitForTimeout(2500);
    const throttledSamples = await page.evaluate(() => window.loadingFixture.samples.splice(0));
    const throttledStageSamples = await page.evaluate(() => window.loadingFixture.stageSamples.splice(0));
    await cdp.send('Emulation.setCPUThrottlingRate', {rate: 1});
    await cdp.detach();
    assert.ok(p95(normalSamples) < 16, 'Normal p95 draw work fits within frame budget');
    assert.ok(p95(throttledSamples) < 33.4, '4× throttled p95 draw work fits 30 FPS budget');
    assert.ok(normalStageSamples.length >= 30 && throttledStageSamples.length >= 20, 'Performance samples include the rotating pitch and rising players');
    assert.ok(p95(normalStageSamples) < 16 && p95(throttledStageSamples) < 33.4, 'The 3D stage stays within the frame budget');

    await page.locator('#fixture-dark').click();
    await waitDraw(page, 'fixture-hero');
    const darkColor = await page.evaluate(() => window.loadingFixture.colors['fixture-hero']);
    await page.locator('#fixture-light').click();
    await waitDraw(page, 'fixture-hero');
    assert.notEqual(await page.evaluate(() => window.loadingFixture.colors['fixture-hero']), darkColor, 'Live theme change refreshes canvas colors');
    await page.evaluate(() => document.getElementById('fixture-parent').style.width = '0px');
    await stopped(page, ['fixture-hero']);
    await page.evaluate(() => document.getElementById('fixture-parent').style.width = '');
    await waitDraw(page, 'fixture-hero');
    await page.locator('#fixture-hero canvas').evaluate(canvas => canvas.dispatchEvent(new Event('contextlost')));
    await stopped(page, ['fixture-hero']);
    assert.equal(await page.locator('#fixture-hero .fm-loading-fallback').isVisible(), true);
    await page.locator('#fixture-hero canvas').evaluate(canvas => canvas.dispatchEvent(new Event('contextrestored')));
    await waitDraw(page, 'fixture-hero');

    await page.evaluate(() => { document.getElementById('fixture-parent').style.display = 'none'; document.getElementById('fixture-compact-parent').style.display = 'none'; });
    await stopped(page, ['fixture-hero', 'fixture-compact']);
    const callbacksBefore = await page.evaluate(() => window.loadingFixture.callbacks);
    await page.waitForTimeout(300);
    assert.ok(await page.evaluate(() => window.loadingFixture.callbacks) - callbacksBefore <= 2, 'No recurring animation callbacks while all instances are hidden');
    await page.evaluate(() => { document.getElementById('fixture-parent').style.display = ''; document.getElementById('fixture-compact-parent').style.display = ''; });
    await waitDraw(page, 'fixture-hero');
    await page.evaluate(() => {
      Object.defineProperty(document, 'hidden', {configurable: true, get: () => window.fixtureHidden === true});
      window.fixtureHidden = true;
      document.dispatchEvent(new Event('visibilitychange'));
    });
    await stopped(page, ['fixture-hero', 'fixture-compact']);
    await page.evaluate(() => { window.fixtureHidden = false; document.dispatchEvent(new Event('visibilitychange')); });
    await waitDraw(page, 'fixture-hero');
    await page.emulateMedia({reducedMotion: 'reduce'});
    await stopped(page, ['fixture-hero', 'fixture-compact']);
    assert.equal(await page.locator('#fixture-hero .fm-loading-fallback').isVisible(), true);
    await page.emulateMedia({reducedMotion: 'no-preference'});
    await waitDraw(page, 'fixture-hero');
    for (let i = 0; i < 20; i++) {
      await page.evaluate(() => {
        const current = document.getElementById('fixture-hero');
        current.replaceWith(current.cloneNode(true));
      });
      await waitDraw(page, 'fixture-hero');
    }
    const countBefore = await draws(page, 'fixture-hero');
    await page.waitForTimeout(1000);
    const rate = await draws(page, 'fixture-hero') - countBefore;
    assert.ok(rate >= 10 && rate <= 33, `Remounts retain one capped renderer (${rate} frames/s)`);
    await page.evaluate(() => { document.getElementById('fixture-hero').remove(); document.getElementById('fixture-compact').remove(); });
    await stopped(page, ['fixture-hero', 'fixture-compact']);
    assert.deepEqual(errors, []);
    assert.deepEqual(external, []);
    console.log(`PASS ${width}px: response=${responseMs.toFixed(1)}ms, p95 draw=${p95(normalSamples).toFixed(2)}ms, 3D=${p95(normalStageSamples).toFixed(2)}ms, 3D 4x=${p95(throttledStageSamples).toFixed(2)}ms; hide/resume, visibility, motion preference, repeated mounts, removal, external requests=0`);
    await page.close();
  }
  for (const fallback of ['reduced-motion', 'canvas-unavailable']) {
    const {page, errors, external} = await createPage(browser, 320, fallback === 'reduced-motion' ? {reducedMotion: 'reduce'} : {});
    if (fallback === 'canvas-unavailable') await page.addInitScript(() => {
      const original = HTMLCanvasElement.prototype.getContext;
      HTMLCanvasElement.prototype.getContext = function(type, ...args) {
        return type === '2d' && this.closest('[data-fm-loading]') ? null : original.call(this, type, ...args);
      };
    });
    await page.goto(origin + '/');
    await page.locator('#fixture-hero .fm-loading-fallback').waitFor();
    await stopped(page, ['fixture-hero', 'fixture-compact']);
    assert.equal(await draws(page, 'fixture-hero'), 0);
    await noOverflow(page);
    await page.screenshot({path: path.join(screenshotDir, `320-${fallback}.png`)});
    assert.deepEqual(errors, []);
    assert.deepEqual(external, []);
    await page.close();
    console.log('PASS initial ' + fallback + ': static illustration, no animated draws');
  }
  const files = ['www/loading-visual.js', 'www/loading-visual.css', 'loading_visual.R'];
  const bytes = zlib.gzipSync(Buffer.concat(files.map(file => fs.readFileSync(file)))).length;
  assert.ok(bytes <= 20 * 1024, `New assets/helper gzip budget (${bytes} bytes)`);
  console.log(`PASS added assets + helper gzip=${bytes} bytes; screenshots=${screenshotDir}`);
}

async function appChecks(browser) {
  for (const width of [1440, 320]) {
    const {page, errors} = await createPage(browser, width);
    await page.goto(origin + '/');
    await page.locator('.waiter-overlay').waitFor({state: 'hidden'});
    const launch = await page.locator('.waiter-overlay [data-fm-loading]').getAttribute('id');
    await stopped(page, [launch]);
    await page.waitForFunction(() => document.getElementById('login-login_button')?.dataset.futmondoLoginBound === 'true');
    // The existing app-object fixture loses login_form_visible on the client
    // (also reproduced independently with the original HEAD spinner/UI).
    // Drive synthetic credentials through the real Shiny inputs and button;
    // this test covers startup, not the separate login-form visibility contract.
    await page.evaluate(() => {
      Shiny.setInputValue('login-user_name', 'offline@example.invalid');
      Shiny.setInputValue('login-password', 'fixture');
      document.getElementById('login-login_button').click();
    });
    const loading = page.locator('#startup_status [data-fm-loading]');
    await loading.waitFor();
    assert.equal(await loading.evaluate(el => el.closest('details') === null), true, 'Animation is outside the collapsed information disclosure');
    await page.locator('#startup_status').filter({hasText: 'Signed in. Loading your leagues…'}).waitFor();
    const startupId = await loading.getAttribute('id');
    await waitDraw(page, startupId);
    await page.screenshot({path: path.join(screenshotDir, `${width}-app-leagues.png`)});
    await page.locator('#startup_status').filter({hasText: "Loading today's market…"}).waitFor();
    await loading.waitFor();
    await page.screenshot({path: path.join(screenshotDir, `${width}-app-market.png`)});
    await loading.waitFor({state: 'detached'});
    await stopped(page, [startupId, launch]);
    for (const stage of ['partial', 'failed', 'empty']) {
      await page.evaluate(stage => Shiny.setInputValue('fixture_startup_stage', stage, {priority: 'event'}), stage);
      const text = stage === 'partial' ? 'Some sections could not load' : stage === 'failed' ? 'The first page could not load' : 'No supported leagues are available';
      await page.locator('#startup_status').filter({hasText: text}).waitFor();
      assert.equal(await page.locator('#startup_status [data-fm-loading]').count(), 0, `${stage} has no continuing animation`);
      if (stage !== 'empty') {
        if (!await page.locator('#startup_status details').evaluate(el => el.open))
          await page.locator('#startup_status summary').click();
        assert.equal(await page.locator('#retry_startup').isVisible(), true, `${stage} retains the Retry action`);
      } else assert.equal(await page.locator('#retry_startup').count(), 0);
    }
    await page.evaluate(() => {
      window.loadingAppStats = null;
      Shiny.addCustomMessageHandler('fixture-stats', stats => { window.loadingAppStats = stats; });
      Shiny.setInputValue('fixture_snapshot', Math.random(), {priority: 'event'});
    });
    await page.waitForFunction(() => window.loadingAppStats !== null);
    assert.equal(await page.evaluate(() => window.loadingAppStats.http), 0, 'Offline startup attempts no real HTTP');
    await noOverflow(page);
    assert.deepEqual(errors, []);
    console.log(`PASS actual app ${width}px: launch dismisses/stops; leagues → market → ready; partial/failed retain Retry, empty settles, no animation after completion, HTTP attempts=0`);
    await page.close();
  }
}

(async () => {
  const executablePath = process.env.FUTMONDO_TEST_CHROMIUM || '/tmp/futmondo-browsers/chromium_headless_shell-1194/chrome-linux/headless_shell';
  const browser = await chromium.launch({headless: true, executablePath, args: ['--no-sandbox']});
  try { await (fullApp ? appChecks(browser) : visualChecks(browser)); }
  finally { await browser.close(); }
})().catch(error => { console.error(error.stack); process.exitCode = 1; });
