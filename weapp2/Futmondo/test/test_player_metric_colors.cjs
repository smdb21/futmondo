// Execute the exact JS callback emitted by the R column definition.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const fixture = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
let surface = '#0B120D';
global.document = {documentElement: {}};
global.getComputedStyle = () => ({getPropertyValue: () => surface});
const style = Function('return (' + fixture.style + ')')();
const ranges = {recent_points_avg: {min: -10, max: 10}, recent_points_ratio: {min: 0, max: 10}};
function cell(value, id = 'recent_points_avg', meta = ranges, client = {}) {
  const state = {...client, meta: {playerMetricRanges: meta}};
  Object.defineProperty(state, 'sortedData', {get() {throw Error('Style must not scan client rows');}});
  return style({values: {[id]: value}}, {id}, state);
}
const color = value => cell(value)['--fm-metric-bg'];
const mix = percent => `color-mix(in srgb, var(--fm-surface) ${percent}%, #7cff9b)`;
assert.equal(color(-10), mix(100));
assert.equal(color(0), mix(50));
assert.equal(color(10), mix(0));
assert.equal(color(-5), mix(75));
assert.equal(color(5), mix(25));
assert.equal(color(-20), color(-10));
assert.equal(color(20), color(10));
for (const value of [null, undefined, NaN, Infinity, '5']) assert.deepEqual(cell(value), {});
assert.deepEqual(cell(1, 'recent_points_avg', {recent_points_avg: {min: null, max: null}}), {});
assert.deepEqual(style({values: {recent_points_avg: 1}}, {id: 'recent_points_avg'}, {}), {});
assert.equal(cell(0, 'recent_points_avg', {recent_points_avg: {min: 0, max: 0}})['--fm-metric-bg'], mix(100));
assert.equal(cell(-3, 'recent_points_avg', {recent_points_avg: {min: -3, max: -3}})['--fm-metric-bg'], mix(100));
const independent = {recent_points_avg: {min: 0, max: 20}, recent_points_ratio: {min: 0, max: 10}};
assert.equal(cell(10, 'recent_points_avg', independent)['--fm-metric-bg'], mix(50));
assert.equal(cell(10, 'recent_points_ratio', independent)['--fm-metric-bg'], mix(0));
assert.notEqual(cell(0)['--fm-metric-bg'], cell(0, 'recent_points_avg', {recent_points_avg: {min: -10, max: 30}})['--fm-metric-bg']);
for (const client of [{searchValue: 'Player'}, {page: 2, pageSize: 5}, {sorted: [{id: 'name', desc: true}]}]) {
  assert.deepEqual(cell(5, 'recent_points_avg', ranges, client), cell(5));
}
function luminance(rgb) {
  return rgb.map(v => v / 255).map(v => v <= 0.04045 ? v / 12.92 : ((v + 0.055) / 1.055) ** 2.4)
    .reduce((sum, value, index) => sum + value * [0.2126, 0.7152, 0.0722][index], 0);
}
for (const theme of ['#0B120D', '#ffffff', '#eeeeee']) {
  surface = theme;
  const base = theme.slice(1).match(/../g).map(v => parseInt(v, 16));
  for (let value = -10; value <= 10; value += 0.2) {
    const result = cell(value);
    assert.equal(result.backgroundColor, undefined);
    const t = (value + 10) / 20;
    const background = base.map((v,i) => v + ([124,255,155][i]-v)*t);
    const ink = result['--fm-metric-fg'] === '#000000' ? 0 : 1;
    const lum = luminance(background);
    assert.ok((Math.max(lum,ink)+0.05)/(Math.min(lum,ink)+0.05) >= 4.5);
  }
}
console.log('CLIENT METRIC COLORS: palette, missing/equal/signed values, independent domains, metadata changes, stable client state and contrast passed');
