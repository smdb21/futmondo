// Start test_loading_visual_app.R, then run this file. Add --full-app to both
// commands for the real offline application startup acceptance checks.
// --camera-only checks the orbit and close-up at three viewport widths.
// --fullscreen-only runs the portrait/landscape checks; --clubs-only checks kit variants.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const zlib = require('node:zlib');
const { chromium } = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules', 'playwright'));
const fullApp = process.argv.includes('--full-app');
const origin = `http://127.0.0.1:${fullApp ? 38768 : 38767}`;
const screenshotDir = process.env.FUTMONDO_LOADING_SCREENSHOTS || '/tmp/futmondo-loading-visual';
fs.mkdirSync(screenshotDir, { recursive: true });

// Independent expected classic club palettes; decorative kit selection is local.
const clubKits = {
  'real-madrid': {family: 'white', base: ['#f7f8fc', '#f7f8fc']},
  barcelona: {family: 'blue', base: ['#356ed1', '#2456a6'], stripe: ['#cf3659', '#b9234b']},
  atletico: {family: 'red', base: ['#e64a57', '#b92338'], stripe: ['#f7f8fc', '#f7f8fc']},
  betis: {family: 'green', base: ['#36b97b', '#087846'], stripe: ['#f7f8fc', '#f7f8fc']},
  inter: {family: 'blue', base: ['#4082df', '#1759a8'], stripe: ['#111827', '#111827']},
  milan: {family: 'red', base: ['#e74a4f', '#ba2331'], stripe: ['#111827', '#111827']},
  dortmund: {family: 'yellow', base: ['#f5d638', '#f5d638'], stripe: ['#111827', '#111827'], edge: '#806000'},
  juventus: {family: 'white', base: ['#f7f8fc', '#f7f8fc'], stripe: ['#111827', '#111827']},
  'man-city': {family: 'blue', base: ['#7dc9ef', '#7dc9ef'], edge: '#376b83'},
  liverpool: {family: 'red', base: ['#ee4c5b', '#bb1f38']}
};

function instrument() {
  const stats = window.loadingFixture = {captureId: 'fixture-hero', draws: {}, samples: [], stageSamples: [], callbacks: 0, pending: new Set(), maxPending: 0, colors: {}, capture: false, geometry: []};
  const nativeRequest = window.requestAnimationFrame.bind(window);
  const nativeCancel = window.cancelAnimationFrame.bind(window);
  const nativeClear = CanvasRenderingContext2D.prototype.clearRect;
  const nativeStroke = CanvasRenderingContext2D.prototype.stroke;
  const nativeFill = CanvasRenderingContext2D.prototype.fill;
  const paths = new WeakMap();
  const clipping = new WeakMap(), clippingStack = new WeakMap();
  for (const name of ['save', 'restore', 'clip']) {
    const original = CanvasRenderingContext2D.prototype[name];
    CanvasRenderingContext2D.prototype[name] = function(...args) {
      if (id(this)) {
        if (name === 'save') {
          if (!clippingStack.has(this)) clippingStack.set(this, []);
          clippingStack.get(this).push(clipping.get(this) || false);
        } else if (name === 'restore') clipping.set(this, clippingStack.get(this)?.pop() || false);
        else clipping.set(this, true);
      }
      return original.apply(this, args);
    };
  }
  function id(context) { return context.canvas.closest('[data-fm-loading]')?.id; }
  function trace(context) { return stats.capture && id(context) === stats.captureId; }
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
    if (trace(context) && stats.geometry.length < 1000)
      stats.geometry.push({kind, alpha: context.globalAlpha, clipped: clipping.get(context) || false,
        color: kind === 'fill' ? context.fillStyle : context.strokeStyle,
        transform: ['a', 'b', 'c', 'd', 'e', 'f'].map(key => context.getTransform()[key]),
        path: paths.get(context) || []});
  }
  CanvasRenderingContext2D.prototype.clearRect = function(...args) {
    const key = id(this);
    if (key) {
      stats.draws[key] = (stats.draws[key] || 0) + 1;
      if (stats.firstDrawAt === undefined) stats.firstDrawAt = performance.now();
      if (stats.capture && key === stats.captureId) stats.geometry = [];
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
          const phase = (timestamp - stats.firstDrawAt + 25700) % 25700;
          if (phase >= 1800 && phase <= 14400) stats.stageSamples.push(duration);
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
async function captureGeometry(page, key = 'fixture-hero') {
  await page.evaluate(key => { window.loadingFixture.captureId = key; window.loadingFixture.capture = true; }, key);
  await waitDraw(page, key);
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
  for (const name of ['pitch', 'tilted', 'rising', 'flock']) for (const mark of phases[name]) for (const command of mark.path) {
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
  assert.notEqual(teamColors[0], teamColors[1], 'The selected clubs have distinct primary colors');
  function countTeams(marks, expected, label) {
    assert.equal(marks.length, expected * 2, label + ' has one mark per player');
    for (const color of teamColors)
      assert.equal(marks.filter(mark => mark.color === color).length, expected, label + ' retains each team tone');
  }
  const heads = phases.rising.filter(mark => mark.kind === 'fill' && mark.path.length === 1 &&
    mark.path[0].op === 'arc' && mark.path[0].args[2] > 1.5);
  countTeams(heads, 11, 'Raised heads');
  let previousHead = -1;
  for (const head of heads) {
    const team = teamColors.indexOf(head.color), headIndex = phases.rising.indexOf(head);
    const shirtPanels = phases.rising.slice(previousHead + 1, headIndex).filter(mark =>
      mark.kind === 'fill' && Math.abs(mark.alpha - 0.95) < 0.01 && mark.path.length === 4);
    const stripe = teamColors.stripes[team];
    assert.equal(shirtPanels.length, stripe ? 2 : 0,
      'Each striped kit has a colored panel on its visible front/back and side');
    assert.ok(shirtPanels.every(mark => mark.color === stripe), 'Shirt panels use the selected club accent');
    if (head.color === '#f7f8fc') {
      const outline = phases.rising.find(mark => mark.kind === 'stroke' && mark.color === teamColors.edge &&
        mark.path.length === 1 && mark.path[0].op === 'arc' &&
        JSON.stringify(mark.path) === JSON.stringify(head.path));
      assert.ok(outline, 'White figures keep a visible neutral contour in both themes');
    }
    previousHead = headIndex;
  }
  const bodies = phases.rising.filter(mark => mark.kind === 'fill' && mark.alpha > 0.65 &&
    teamColors.includes(mark.color) && mark.path.length >= 8 &&
    mark.path.every(command => command.op === 'moveTo' || command.op === 'lineTo'));
  countTeams(bodies, 11, 'Solid foosball bodies');
  const sides = phases.rising.filter(mark => mark.kind === 'fill' && Math.abs(mark.alpha - 0.58) < 0.01 &&
    teamColors.includes(mark.color) && mark.path.length >= 8);
  countTeams(sides, 11, 'Solid foosball side faces');
  for (let i = 0; i < bodies.length; i++) {
    const body = bodies[i];
    const points = [...body.path, ...sides[i].path].map(command => command.args);
    const xs = points.map(point => point[0]), ys = points.map(point => point[1]);
    const width = Math.max(...xs) - Math.min(...xs), height = Math.max(...ys) - Math.min(...ys);
    assert.ok(width >= 5 && width <= 25 && height >= 10 && height <= 40, 'Goal-facing foosball bodies retain a substantial filled volume');
    assert.ok(polygonArea(body.path.map(command => command.args)) + polygonArea(sides[i].path.map(command => command.args)) > width * height * 0.3,
      'The front and side form a solid molded body, not a thin stick figure');

  }
  checkGoalFacing(phases.rising, teamColors);
  const radarDots = phases.players.filter(mark => mark.kind === 'fill' && mark.path.length === 1 && mark.path[0].op === 'arc' &&
    mark.path[0].args[2] >= 2.3 && mark.path[0].args[2] <= 3.2 && teamColors.includes(mark.color));
  countTeams(radarDots, 11, 'Radar player dots');
  for (const dot of radarDots) {
    const team = teamColors.indexOf(dot.color);
    assert.ok(phases.players.some(mark => mark.kind === 'stroke' &&
      mark.color === (teamColors.stripes[team] || teamColors.edges[team] || teamColors[team]) &&
      JSON.stringify(mark.path) === JSON.stringify(dot.path)),
    'Radar dots retain the club stripe or a neutral readable edge');
  }
  const expectedParticles = particleMarks(phases.pitch).length;
  assert.ok([180, 320].includes(expectedParticles), 'Pitch starts with exactly the configured particle budget');
  for (const name of ['pitch', 'morph', 'flock', 'lateFlock'].filter(name => phases[name])) {
    const particles = phases[name].filter(mark => mark.kind === 'fill' && mark.alpha > 0.4 &&
      mark.path.length === 1 && mark.path[0].op === 'ellipse');
    assert.equal(particles.length, expectedParticles, name + ' retains the complete bounded particle pool');
    const expectedColors = [...new Set([teamColors[0], ...[teamColors.stripes[0]].filter(Boolean)])].sort();
    assert.deepEqual([...new Set(particles.map(mark => mark.color))].sort(), expectedColors,
      name + ' carries the selected primary and shirt-stripe colors into the transformation');
    for (const particle of particles) {
      let edges = [];
      if (particle.color === '#111827') edges = [...teamColors];
      else if (teamColors.light && particle.color === '#f7f8fc') edges = [teamColors.edge];
      else if (teamColors.light) {
        const team = teamColors.indexOf(particle.color);
        if (team !== -1 && teamColors.edges[team]) edges = [teamColors.edges[team]];
      }
      if (edges.length) assert.ok(phases[name].some(mark => mark.kind === 'stroke' && edges.includes(mark.color) &&
        JSON.stringify(mark.path) === JSON.stringify(particle.path)),
      'Low-contrast kit particles retain a readable edge in the active theme');
    }
  }
}

function lineIntersection(a, b, c, d) {
  const determinant = (a[0] - b[0]) * (c[1] - d[1]) - (a[1] - b[1]) * (c[0] - d[0]);
  assert.ok(Math.abs(determinant) > 0.01, 'Perspective pitch edges establish a lateral vanishing point');
  const left = a[0] * b[1] - a[1] * b[0], right = c[0] * d[1] - c[1] * d[0];
  return [(left * (c[0] - d[0]) - (a[0] - b[0]) * right) / determinant,
    (left * (c[1] - d[1]) - (a[1] - b[1]) * right) / determinant];
}
function checkGoalFacing(geometry, teamColors) {
  const pitch = pitchCorners(geometry);
  const lateral = lineIntersection(pitch[0], pitch[3], pitch[1], pitch[2]);
  const bodies = geometry.filter(mark => mark.kind === 'fill' && mark.alpha > 0.65 &&
    teamColors.includes(mark.color) && mark.path.length >= 8 &&
    mark.path.every(command => command.op === 'moveTo' || command.op === 'lineTo'));
  assert.equal(bodies.length, 22, 'Both teams retain eleven molded body fronts');
  for (const body of bodies) {
    // The known molded-shirt shoulders share height and depth. Their lateral
    // axis must converge on the pitch's Y vanishing point, so the torso faces
    // a goal along X rather than presenting the old flat X/Z billboard.
    const a = body.path[2].args, b = body.path[11].args;
    const axis = [b[0] - a[0], b[1] - a[1]];
    const toward = [lateral[0] - (a[0] + b[0]) / 2, lateral[1] - (a[1] + b[1]) / 2];
    const alignment = Math.abs(axis[0] * toward[0] + axis[1] * toward[1]) / (Math.hypot(...axis) * Math.hypot(...toward));
    assert.ok(alignment > 0.999, 'Every shirt is physically oriented along the pitch lateral axis');
  }
  const anchors = geometry.filter(mark => mark.kind === 'fill' && Math.abs(mark.alpha - 0.14) < 0.01 &&
    mark.path.length === 1 && mark.path[0].op === 'ellipse');
  const feet = geometry.filter(mark => mark.kind === 'fill' && Math.abs(mark.alpha - 0.8) < 0.01 &&
    mark.path.length === 4 && teamColors.includes(mark.color));
  const heads = geometry.filter(mark => mark.kind === 'fill' && mark.path.length === 1 &&
    mark.path[0].op === 'arc' && mark.path[0].args[2] > 1.5);
  const noses = geometry.filter(mark => mark.kind === 'fill' && mark.path.length === 3 &&
    Math.abs(mark.alpha - 0.9) < 0.01 && teamColors.includes(mark.color));
  assert.equal(anchors.length, 22);
  assert.equal(feet.length, 22, 'Each molded player has a short forward kicking foot');
  assert.equal(heads.length, 22);
  assert.equal(noses.length, 22, 'Each head has a visible goal-facing profile');
  const sides = geometry.filter(mark => mark.kind === 'fill' && Math.abs(mark.alpha - 0.58) < 0.01 &&
    teamColors.includes(mark.color) && mark.path.length >= 8);
  assert.equal(sides.length, 22, 'Every body retains its visible volume side');
  const goal = [pitch[1][0] - pitch[0][0], pitch[1][1] - pitch[0][1]];
  const goalLength = Math.hypot(...goal);
  for (let i = 0; i < 22; i++) {
    const direction = feet[i].color === teamColors[0] ? 1 : -1;
    assert.equal(noses[i].color, feet[i].color, 'The head and foot retain the same team identity');
    const foot = feet[i].path.map(command => command.args);
    const forward = ((foot[1][0] - foot[0][0]) * goal[0] + (foot[1][1] - foot[0][1]) * goal[1]) / goalLength;
    assert.ok(direction * forward > 2, 'Each short toe points toward its opposing goal');
    // Compare like-axis dimensions on the projected model. A shoe can remain
    // directional without being wider than the shoulders or longer than the
    // torso depth, as the previous pedestal-like foot was.
    const side = sides[i].path.map(command => command.args);
    const shoulder = bodies[i].path;
    const distance = (a, b) => Math.hypot(a[0] - b[0], a[1] - b[1]);
    const toeDepth = distance(foot[0], foot[1]);
    const bodyDepth = distance(side[4], side[9]);
    const shoeWidth = distance(foot[1], foot[2]);
    const shoulderWidth = distance(shoulder[2].args, shoulder[11].args);
    assert.ok(toeDepth > bodyDepth * 0.45 && toeDepth < bodyDepth * 1.1,
      'The shoe is shorter than the torso depth rather than a projecting pedestal');
    assert.ok(shoeWidth > shoulderWidth * 0.3 && shoeWidth < shoulderWidth * 0.72,
      'The shoe is narrower than the shirt rather than a wide base');
    assert.ok(polygonArea(foot) > 5, 'The shortened foot still has a visible solid top');
    const tip = noses[i].path[1].args, head = heads[i].path[0].args;
    const facing = ((tip[0] - head[0]) * goal[0] + (tip[1] - head[1]) * goal[1]) / goalLength;
    assert.ok(direction * facing > 2, 'The two club teams face opposite goals');
  }
}
async function readTeamColors(page) {
  const state = await page.locator('#fixture-hero').evaluate(node => ({
    ids: (node.getAttribute('data-fm-teams') || '').split(' '),
    light: document.documentElement.getAttribute('data-fm-theme') === 'light'
  }));
  assert.equal(state.ids.length, 2, 'Each page chooses exactly two clubs');
  assert.ok(state.ids.every(id => clubKits[id]), 'Only known classic club kits are selected');
  assert.notEqual(clubKits[state.ids[0]].family, clubKits[state.ids[1]].family,
    'Opponents use distinguishable primary-color families');
  const mode = state.light ? 1 : 0;
  const colors = state.ids.map(id => clubKits[id].base[mode]);
  colors.ids = state.ids;
  colors.light = state.light;
  colors.stripes = state.ids.map(id => clubKits[id].stripe?.[mode] || null);
  colors.edge = state.light ? '#657286' : '#a5afbf';
  colors.edges = state.ids.map(id => clubKits[id].family === 'white' ? colors.edge : state.light && clubKits[id].edge || null);
  return colors;
}
async function checkPairIdentity(page, expected) {
  const identities = await page.locator('[data-fm-loading]').evaluateAll(nodes =>
    nodes.map(node => node.getAttribute('data-fm-teams')));
  assert.ok(identities.length >= 2, 'Hero and compact instances remain mounted');
  assert.ok(identities.every(ids => ids === expected.join(' ')),
    'All instances retain one page-level club pair through theme, loop and remount changes');
}

function particleMarks(geometry) {
  return geometry.filter(mark => mark.kind === 'fill' && mark.alpha > 0.4 &&
    mark.path.length === 1 && mark.path[0].op === 'ellipse');
}
function checkAscent({release, dissolve, rising, flyby, approach}) {
  const before = particleMarks(release), after = particleMarks(dissolve);
  assert.ok([180, 320].includes(before.length) && after.length === before.length, 'Dissolve reuses the complete existing particle pool');
  const headRise = before.slice(0, 22).map((mark, i) => mark.path[0].args[1] - after[i].path[0].args[1]);
  assert.ok(headRise.every(distance => distance > 1), 'Every head-tip particle initially travels toward the sky');
  assert.ok(headRise.reduce((sum, n) => sum + n, 0) / 22 > 2, 'The initial upward motion is visible');
  // Samples descend through the featured player. The lowest points remain
  // anchored while particles from the head are already travelling upward.
  for (let i = before.length - 22; i < before.length; i++)
    assert.ok(Math.hypot(...before[i].path[0].args.slice(0, 2).map((n, axis) => n - after[i].path[0].args[axis])) < 0.1,
      'Lower foot particles leave after the head particles');
  const heads = marks => marks.filter(mark => mark.kind === 'fill' && mark.path.length === 1 &&
    mark.path[0].op === 'arc' && mark.path[0].args[2] > 1.5);
  assert.equal(heads(rising).length, 22);
  assert.equal(heads(flyby).length, 22, 'Both teams are still drawn during the banking pass');
  const edgeX = geometry => pitchCorners(geometry)[1][0] - pitchCorners(geometry)[0][0];
  assert.ok(edgeX(rising) > 200 && edgeX(flyby) < -200, 'The pitch turns through the sweeping pass');
  const midRadius = heads(approach)[7].path[0].args[2];
  assert.ok(midRadius > 1.5 * heads(flyby)[7].path[0].args[2] &&
    midRadius < .85 * heads(release)[0].path[0].args[2], 'Zoom is still approaching the player halfway through');
  assert.equal(heads(release).length, 1, 'Camera isolates one intact player before takeoff');
  const [x, y, radius] = heads(release)[0].path[0].args;
  assert.ok(Math.abs(x - 210) < 6 && y < 140 && radius > 3 * heads(rising)[7].path[0].args[2],
    'The camera centers the enlarged midfielder');
  assert.ok(before.every(mark => Math.abs(mark.path[0].args[0] - x) < 4 * radius &&
    Math.abs(mark.path[0].args[1] - y) < 12 * radius), 'All flock particles start within one figure');
  assert.equal(heads(dissolve).length, 0, 'The head is dissolved while the lower figure remains');
}
async function checkLaunchLayout(page, selector = '#fixture-hero') {
  const layout = await page.locator(selector).evaluate(node => {
    const box = node.getBoundingClientRect(), canvas = node.querySelector('canvas');
    const launch = node.closest('.fm-loading-launch');
    const brand = launch.querySelector('.fm-loading-brand').getBoundingClientRect();
    const caption = launch.querySelector('.fm-loading-caption').getBoundingClientRect();
    return {x: box.x, y: box.y, width: box.width, height: box.height, viewportWidth: innerWidth,
      viewportHeight: innerHeight, brandTop: brand.top, captionBottom: caption.bottom,
      bitmapWidth: canvas.width, bitmapHeight: canvas.height,
      mobile: getComputedStyle(node).getPropertyValue('--fm-loading-mobile').trim()};
  });
  assert.equal(layout.mobile, '1', 'The fullscreen portrait launch uses the mobile composition');
  assert.ok(Math.abs(layout.x) <= 1 && Math.abs(layout.y) <= 1, 'Mobile artwork begins at the viewport top');
  assert.ok(Math.abs(layout.width - layout.viewportWidth) <= 1, 'Mobile artwork uses the full viewport width');
  assert.ok(Math.abs(layout.height - layout.viewportHeight / 2) <= 1, 'Artwork occupies the upper half of the visible viewport');
  assert.ok(layout.brandTop >= layout.height && layout.brandTop <= layout.height + 24, 'Brand sits just below the scene');
  assert.ok(layout.captionBottom < layout.viewportHeight, 'Loading status stays visible');
  assert.ok(layout.bitmapWidth <= layout.width * 1.5 + 2 && layout.bitmapHeight <= layout.height * 1.5 + 2, 'Both canvas dimensions respect the pixel-density budget');
  assert.ok(Math.abs(layout.bitmapWidth / layout.bitmapHeight - layout.width / layout.height) < 0.01, 'Backing canvas matches both displayed dimensions without stretching');
  return layout;
}
function normalizedPoint(mark, args, layout) {
  const [a, b, c, d, e, f] = mark.transform;
  assert.ok(Math.abs(a - d) < 0.00001 && b === 0 && c === 0, 'The scene uses uniform geometry scaling');
  return [(a * args[0] + e) / layout.bitmapWidth, (d * args[1] + f) / layout.bitmapHeight];
}
function checkFullscreenGeometry(geometry, layout, flock = false) {
  for (const mark of geometry) for (const command of mark.path) {
    const [x, y] = normalizedPoint(mark, command.args, layout);
    assert.ok(Number.isFinite(x) && Number.isFinite(y) && (mark.clipped || (x >= 0 && x <= 1 && y >= 0 && y <= 1)),
      `Projected marks stay in the visible canvas (${x}, ${y})`);
  }
  if (!flock) return;
  const points = particleMarks(geometry).map(mark => normalizedPoint(mark, mark.path[0].args, layout));
  const xs = points.map(point => point[0]), ys = points.map(point => point[1]);
  const width = Math.max(...xs) - Math.min(...xs), height = Math.max(...ys) - Math.min(...ys);
  assert.ok(width >= 0.84 && width <= 0.97, `The cloud fills roughly 90% of the scene width (${width})`);
  assert.ok(height >= 0.35 && height <= 0.72, `The cloud has a full vertical envelope (${height})`);
  assert.ok(Math.max(...ys) < 0.85, 'The assembled flock stays in the sky above the pitch position');
}
async function fullscreenChecks(browser) {
  for (const viewport of [{width: 390, height: 844}, {width: 320, height: 568}, {width: 430, height: 932}]) {
    const {page, errors, external} = await createPage(browser, viewport.width, {viewport, deviceScaleFactor: 3});
    for (const theme of ['dark', 'light']) {
      await page.goto(origin + '/?fullscreen=1');
      await page.evaluate(theme => localStorage.setItem('futmondo.color-theme', theme), theme);
      await page.reload();
      await waitDraw(page, 'fixture-hero');
      const layout = await checkLaunchLayout(page);
      const teamColors = await readTeamColors(page);
      assert.deepEqual(teamColors.ids, ['real-madrid', 'barcelona'], 'The main matrix uses a deterministic Madrid/Barcelona pair');
      await checkPairIdentity(page, teamColors.ids);
      const phases = {};
      for (const [name, at] of [['pitch', 700], ['rising', 5210], ['flyby', 9210], ['approach', 10925], ['release', 12650], ['dissolve', 13000], ['morph', 13600], ['flock', 15200], ['lateFlock', 18000], ['radar', 20000], ['players', 22200], ['return', 25000]]) {
        const elapsed = await page.evaluate(() => performance.now() - window.loadingFixture.firstDrawAt);
        await page.waitForTimeout(Math.max(0, at - elapsed));
        phases[name] = await captureGeometry(page);
        if (['rising', 'morph', 'flock'].includes(name))
          await page.screenshot({path: path.join(screenshotDir, `${viewport.width}x${viewport.height}-${theme}-${name}-fullscreen.png`)});
        if (['flyby', 'approach', 'release', 'dissolve', 'morph'].includes(name)) {
          checkFullscreenGeometry(particleMarks(phases[name]), layout);
        } else checkFullscreenGeometry(phases[name], layout, name === 'flock');
      }
      checkAscent(phases);
      checkTeams(phases, teamColors);
      const seamElapsed = await page.evaluate(() => performance.now() - window.loadingFixture.firstDrawAt);
      await page.waitForTimeout(Math.max(0, 25800 - seamElapsed));
      assert.deepEqual(await captureGeometry(page), phases.pitch, 'Fullscreen geometry returns to the exact flat loop seam');
      await checkPairIdentity(page, teamColors.ids);
      const normal = await page.evaluate(() => window.loadingFixture.samples);
      assert.ok(normal.length >= 30 && p95(normal) < 16, 'Larger fullscreen canvas keeps normal drawing within budget');
      console.log(`PASS fullscreen ${viewport.width}x${viewport.height} ${theme}: upper-half layout, full cloud, upward dissolve; p95=${p95(normal).toFixed(2)}ms`);
      await noOverflow(page);
    }
    // Toolbar-height changes keep the canvas sharp and the upper-half layout.
    await page.setViewportSize({width: viewport.width, height: viewport.height - 100});
    await waitDraw(page, 'fixture-hero');
    await checkLaunchLayout(page);
    await noOverflow(page);
    if (viewport.width === 390) {
      for (const resized of [{width: 767, height: 768}, {width: 430, height: 600}]) {
        await page.setViewportSize(resized);
        await page.reload();
        await waitDraw(page, 'fixture-hero');
        const shortLayout = await checkLaunchLayout(page);
        checkFullscreenGeometry(await captureGeometry(page), shortLayout);
        for (const [phase, at] of [['rising', 5210], ['radar', 20000], ['players', 22200], ['return', 25000]]) {
          const elapsed = await page.evaluate(() => performance.now() - window.loadingFixture.firstDrawAt);
          await page.waitForTimeout(Math.max(0, at - elapsed));
          checkFullscreenGeometry(await captureGeometry(page), shortLayout);
          if (phase === 'rising') await page.screenshot({path: path.join(screenshotDir, `${resized.width}x${resized.height}-rising-fullscreen.png`)});
        }
        await noOverflow(page);
      }
      await page.setViewportSize(viewport);
      await waitDraw(page, 'fixture-hero');
      const cdp = await page.context().newCDPSession(page);
      await cdp.send('Emulation.setCPUThrottlingRate', {rate: 4});
      await page.reload();
      await waitDraw(page, 'fixture-hero');
      await page.waitForTimeout(6500);
      const samples = await page.evaluate(() => window.loadingFixture.stageSamples);
      assert.ok(samples.length >= 40 && p95(samples) < 33.4, 'Mobile pitch/dissolve drawing fits 30 FPS at 4× CPU throttling');
      console.log(`PASS fullscreen 4× CPU: stage p95=${p95(samples).toFixed(2)}ms, samples=${samples.length}`);
      await cdp.send('Emulation.setCPUThrottlingRate', {rate: 1});
      await cdp.detach();
    }
    await page.setViewportSize({width: 844, height: 390});
    await waitDraw(page, 'fixture-hero');
    const landscape = await page.locator('#fixture-hero').boundingBox();
    assert.ok(Math.abs(landscape.width - 420) <= 1 && Math.abs(landscape.height - 280) <= 1, 'Landscape retains the standard scene dimensions');
    await noOverflow(page);
    await page.setViewportSize(viewport);
    await waitDraw(page, 'fixture-hero');
    await checkLaunchLayout(page);
    await page.emulateMedia({reducedMotion: 'reduce'});
    await stopped(page, ['fixture-hero', 'fixture-compact']);
    assert.equal(await page.locator('#fixture-hero .fm-loading-fallback').isVisible(), true,
      JSON.stringify(await page.locator('#fixture-hero').evaluate(node => ({ready: node.dataset.fmDrawing, reduced: matchMedia('(prefers-reduced-motion: reduce)').matches, visibility: getComputedStyle(node.querySelector('svg')).visibility, rect: node.getBoundingClientRect().toJSON()}))));
    await page.screenshot({path: path.join(screenshotDir, `${viewport.width}-fullscreen-reduced-motion.png`)});
    assert.deepEqual(errors, []);
    assert.deepEqual(external, []);
    await page.close();
  }
}

async function clubChecks(browser) {
  const {page, errors, external} = await createPage(browser, 390, {deviceScaleFactor: 3});
  const catalog = Object.keys(clubKits);
  for (let i = 0; i < catalog.length; i++) {
    const seed = i === 0 ? '0' : i === 9 ? '0.999' : String((i + 0.5) / 10);
    await page.goto(origin + '/?kits=' + seed);
    await waitDraw(page, 'fixture-hero');
    const colors = await readTeamColors(page);
    assert.equal(colors.ids[0], catalog[i], 'Every catalog club can be selected on a new page load');
    await checkPairIdentity(page, colors.ids);
    assert.ok(await page.evaluate(() => typeof window.fixtureRestoreRandom === 'undefined' &&
      new Set([Math.random(), Math.random(), Math.random()]).size > 1),
    'Fixture restores native randomness immediately after the loader chooses kits');
  }
  console.log('PASS ten club choices: distinct primary families, shared hero/compact pair, native randomness restored');

  for (const variant of [
    {seed: '0.999', ids: ['liverpool', 'man-city']},
    {seed: '0.85', ids: ['man-city', 'juventus']},
    {seed: '0.75,0.7', ids: ['juventus', 'dortmund']}
  ]) {
    for (const theme of ['dark', 'light']) {
      await page.goto(origin + '/?kits=' + variant.seed);
      await page.evaluate(theme => localStorage.setItem('futmondo.color-theme', theme), theme);
      await page.reload();
      await waitDraw(page, 'fixture-hero');
      const colors = await readTeamColors(page);
      assert.deepEqual(colors.ids, variant.ids, 'A new document can choose another known club pair');
      const phases = {};
      for (const [name, at] of [['pitch', 700], ['rising', 5210], ['morph', 13600], ['flock', 15200], ['players', 22200]]) {
        const elapsed = await page.evaluate(() => performance.now() - window.loadingFixture.firstDrawAt);
        await page.waitForTimeout(Math.max(0, at - elapsed));
        phases[name] = await captureGeometry(page);
        if (name === 'rising') checkGoalFacing(await captureGeometry(page, 'fixture-compact'), colors);
        if (['rising', 'flock'].includes(name))
          await page.locator('#fixture-parent').screenshot({path: path.join(screenshotDir, variant.ids.join('-') + '-' + theme + '-' + name + '.png')});
      }
      checkTeams(phases, colors);
      await checkPairIdentity(page, variant.ids);
      if (theme === 'light') {
        // Force a different choice only while remounting the decorative nodes;
        // an accidental re-roll is deterministic without perturbing app widgets.
        await page.evaluate(() => {
          window.fixtureSavedRandom = Math.random;
          Math.random = () => 0;
          document.documentElement.dataset.fmTheme = 'dark';
          const hero = document.getElementById('fixture-hero');
          hero.replaceWith(hero.cloneNode(true));
        });
        await waitDraw(page, 'fixture-hero');
        await checkPairIdentity(page, variant.ids);
        await page.evaluate(() => { Math.random = window.fixtureSavedRandom; delete window.fixtureSavedRandom; });
      }
      console.log(`PASS ${variant.ids.join('/')} ${theme}: club accents, readable edges, short shoes and persistent page identity`);
    }
  }
  // Both outlined bodies and black-accent shirts, at the maximum 320-particle
  // hero budget plus the compact instance, exercise the highest draw-call cost.
  await page.setViewportSize({width: 1440, height: 900});
  await page.evaluate(() => localStorage.setItem('futmondo.color-theme', 'light'));
  const cdp = await page.context().newCDPSession(page);
  await cdp.send('Emulation.setCPUThrottlingRate', {rate: 4});
  await page.goto(origin + '/?kits=0.75,0.7');
  await waitDraw(page, 'fixture-hero');
  await page.waitForTimeout(5200);
  const worstGeometry = await captureGeometry(page);
  assert.equal(particleMarks(worstGeometry).length, 0, 'Solid formation skips particle draws before the close-up');
  checkGoalFacing(worstGeometry, await readTeamColors(page));
  await page.locator('#fixture-parent').screenshot({path: path.join(screenshotDir, 'juventus-dortmund-light-worstcase.png')});
  await page.waitForTimeout(2200);
  const samples = await page.evaluate(() => window.loadingFixture.stageSamples);
  assert.ok(samples.length >= 40 && p95(samples) < 33.4, 'Outlined Juventus/Dortmund kits keep the 30 FPS frame budget at 4× CPU');
  console.log(`PASS outlined-kit 4× CPU: stage p95=${p95(samples).toFixed(2)}ms, samples=${samples.length}`);
  await cdp.send('Emulation.setCPUThrottlingRate', {rate: 1});
  await cdp.detach();
  await noOverflow(page);
  assert.deepEqual(errors, []);
  assert.deepEqual(external, []);
  await page.close();
}

async function cameraChecks(browser) {
  for (const [width, fullscreen] of [[1440, false], [390, true], [320, false]]) {
    const {page} = await createPage(browser, width);
    const cdp = await page.context().newCDPSession(page);
    await cdp.send('Emulation.setCPUThrottlingRate', {rate: 4});
    await page.goto(origin + (fullscreen ? '/?fullscreen=1' : '/'));
    await waitDraw(page, 'fixture-hero');
    const phases = {};
    const id = width === 320 ? 'fixture-compact' : 'fixture-hero';
    for (const [name, at] of [['rising', 5210], ['overhead', 7350], ['intoTurn', 9030], ['flyby', 9210], ['outOfTurn', 9390],
      ['approach', 10925], ['release', 12650], ['dissolve', 13000]]) {
      const elapsed = await page.evaluate(() => performance.now() - window.loadingFixture.firstDrawAt);
      await page.waitForTimeout(Math.max(0, at - elapsed));
      phases[name] = await captureGeometry(page, id);
    }
    const corners = pitchCorners(phases.overhead);
    assert.ok(Math.abs(corners[1][1] - corners[0][1]) > 100, 'Camera passes over the end of the pitch mid-orbit');
    const corner = phase => pitchCorners(phases[phase])[0];
    for (const [from, to] of [['intoTurn', 'flyby'], ['flyby', 'outOfTurn']]) {
      const [x, y] = corner(from), [nextX, nextY] = corner(to);
      assert.ok(Math.hypot(nextX-x, nextY-y) > 10, 'Forward flight continues across the former pause');
    }
    checkAscent(phases);
    const samples = await page.evaluate(() => window.loadingFixture.stageSamples);
    assert.ok(samples.length > 100 && p95(samples) < 33.4, 'Camera sequence fits the 30 FPS budget at 4× CPU');
    console.log(`PASS ${width}px continuous bank, approach and release, 4× CPU p95=${p95(samples).toFixed(1)}ms`);
    await page.close();
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
      const teamColors = await readTeamColors(page);
      assert.deepEqual(teamColors.ids, ['real-madrid', 'barcelona'], 'The main matrix uses a deterministic Madrid/Barcelona pair');
      await checkPairIdentity(page, teamColors.ids);
      const pictures = [], phases = {};
      for (const [name, at] of [['pitch', 700], ['tilted', 2400], ['rising', 5210], ['flyby', 9210], ['approach', 10925], ['release', 12650], ['dissolve', 13000], ['morph', 13600], ['flock', 15200], ['lateFlock', 18000], ['radar', 20000], ['players', 22200], ['return', 25000]]) {
        const elapsed = await page.evaluate(() => performance.now() - window.loadingFixture.firstDrawAt);
        await page.waitForTimeout(Math.max(0, at - elapsed));
        phases[name] = await captureGeometry(page);
        if (name === 'rising') checkGoalFacing(await captureGeometry(page, 'fixture-compact'), teamColors);
        pictures.push(await page.locator('#fixture-hero canvas').evaluate(canvas => canvas.toDataURL()));
        await page.locator('#fixture-parent').screenshot({path: path.join(screenshotDir, `${width}-${theme}-${name}.png`)});
        if (['tilted', 'rising', 'morph'].includes(name))
          await page.locator('#fixture-compact-parent').screenshot({path: path.join(screenshotDir, `${width}-${theme}-${name}-compact.png`)});
      }
      assert.equal(new Set(pictures).size, pictures.length, 'Each loop phase renders changing graphics');
      checkGeometry(phases);
      assert.deepEqual(particleMarks(phases.flock).map(mark => mark.alpha),
        particleMarks(phases.lateFlock).map(mark => mark.alpha), 'The cloud remains fully formed near the end of its longer stage');
      assert.notDeepEqual(particleMarks(phases.flock), particleMarks(phases.lateFlock), 'The cloud keeps moving throughout the longer stage');
      checkTeams(phases, teamColors);
      checkAscent(phases);
      checkGoalFacing(phases.rising, teamColors);
      const seamElapsed = await page.evaluate(() => performance.now() - window.loadingFixture.firstDrawAt);
      await page.waitForTimeout(Math.max(0, 25800 - seamElapsed));
      const seam = await captureGeometry(page);
      assert.deepEqual(seam, phases.pitch, 'The loop returns to identical field and particle geometry, including mark orientation');
      assert.equal(await page.locator('#fixture-hero canvas').evaluate(canvas => canvas.toDataURL()), pictures[0], 'The complete canvas is identical across the flat loop seam');
      await checkPairIdentity(page, teamColors.ids);
      await noOverflow(page);
      console.log(`PASS ${width}px ${theme}: thirteen loop phases, upward head-first dissolve, 22 solid goal-facing foosball figures, opposed heads and short shoes, eleven players per club, persistent kit colors, exact seam`);
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
    const untilStage = await page.evaluate(() => (5400 - ((performance.now() - window.loadingFixture.firstDrawAt) % 25700) + 25700) % 25700);
    await page.waitForTimeout(untilStage);
    await page.evaluate(() => { window.loadingFixture.samples = []; window.loadingFixture.stageSamples = []; });
    await cdp.send('Emulation.setCPUThrottlingRate', {rate: 4});
    await page.waitForTimeout(2900);
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
    await checkPairIdentity(page, ['real-madrid', 'barcelona']);
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
  await fullscreenChecks(browser);
  await clubChecks(browser);
  const files = ['www/loading-visual.js', 'www/loading-visual.css', 'loading_visual.R'];
  const bytes = zlib.gzipSync(Buffer.concat(files.map(file => fs.readFileSync(file)))).length;
  assert.ok(bytes <= 20 * 1024, `New assets/helper gzip budget (${bytes} bytes)`);
  console.log(`PASS added assets + helper gzip=${bytes} bytes; screenshots=${screenshotDir}`);
}

async function appChecks(browser) {
  for (const width of [1440, 320]) {
    const {page, errors} = await createPage(browser, width);
    await page.goto(origin + '/', {waitUntil: 'domcontentloaded'});
    await page.locator('.waiter-overlay .fm-loading-launch').waitFor();
    if (width === 320) await checkLaunchLayout(page, '#fm-launch-visual');
    await page.screenshot({path: path.join(screenshotDir, `${width}-app-launch.png`)});
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
      // The current disclosure manager adopts settled feedback into its page
      // information panel, leaving an origin marker in #startup_status.
      const feedback = page.locator('.fm-info-panel .startup-feedback').filter({hasText: text});
      await feedback.waitFor({state: 'attached'});
      const panel = page.locator('.fm-info-panel').filter({hasText: text});
      if (!await panel.evaluate(el => el.open)) await panel.locator(':scope > summary').click();
      await feedback.waitFor({state: 'visible'});
      assert.equal(await page.locator('#startup_status [data-fm-loading]').count(), 0, `${stage} has no continuing animation`);
      if (stage !== 'empty') {
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
  try { await (fullApp ? appChecks(browser) : process.argv.includes("--camera-only") ? cameraChecks(browser) : process.argv.includes("--fullscreen-only") ? fullscreenChecks(browser) : process.argv.includes("--clubs-only") ? clubChecks(browser) : visualChecks(browser)); }
  finally { await browser.close(); }
})().catch(error => { console.error(error.stack); process.exitCode = 1; });
