// Execute the exact JS callback emitted by the R column definition.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const fixture = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const style = Function('return (' + fixture.style + ')')();
const ranges = {recent_points_avg: {min: -10, max: 10}, recent_points_ratio: {min: 0, max: 10}};
function cell(value, id = 'recent_points_avg', meta = ranges, client = {}) {
  const state = {...client, meta: {playerMetricRanges: meta}};
  Object.defineProperty(state, 'sortedData', {get() {throw Error('Style must not scan client rows');}});
  return style({values: {[id]: value}}, {id}, state);
}
const color = value => cell(value).backgroundColor;
assert.equal(color(-10), 'rgb(255, 107, 107)');
assert.equal(color(0), 'rgb(244, 201, 93)');
assert.equal(color(10), 'rgb(124, 255, 155)');
assert.equal(color(-5), 'rgb(250, 154, 100)');
assert.equal(color(5), 'rgb(184, 228, 124)');
assert.equal(color(-20), color(-10));
assert.equal(color(20), color(10));
for (const value of [null, undefined, NaN, Infinity, '5']) assert.deepEqual(cell(value), {});
assert.deepEqual(cell(1, 'recent_points_avg', {recent_points_avg: {min: null, max: null}}), {});
assert.deepEqual(style({values: {recent_points_avg: 1}}, {id: 'recent_points_avg'}, {}), {});
assert.equal(cell(0, 'recent_points_avg', {recent_points_avg: {min: 0, max: 0}}).backgroundColor, 'rgb(244, 201, 93)');
assert.equal(cell(-3, 'recent_points_avg', {recent_points_avg: {min: -3, max: -3}}).backgroundColor, 'rgb(244, 201, 93)');
const independent = {recent_points_avg: {min: 0, max: 20}, recent_points_ratio: {min: 0, max: 10}};
assert.equal(cell(10, 'recent_points_avg', independent).backgroundColor, 'rgb(244, 201, 93)');
assert.equal(cell(10, 'recent_points_ratio', independent).backgroundColor, 'rgb(124, 255, 155)');
assert.notEqual(cell(0).backgroundColor, cell(0, 'recent_points_avg', {recent_points_avg: {min: -10, max: 30}}).backgroundColor);
for (const client of [{searchValue: 'Player'}, {page: 2, pageSize: 5}, {sorted: [{id: 'name', desc: true}]}]) {
  assert.deepEqual(cell(5, 'recent_points_avg', ranges, client), cell(5));
}
function luminance(rgb) {
  return rgb.map(v => v / 255).map(v => v <= 0.04045 ? v / 12.92 : ((v + 0.055) / 1.055) ** 2.4)
    .reduce((sum, value, index) => sum + value * [0.2126, 0.7152, 0.0722][index], 0);
}
for (let value = -10; value <= 10; value += 0.2) {
  const result = cell(value);
  assert.equal(result.color, '#102019');
  const background = result.backgroundColor.match(/\d+/g).map(Number);
  assert.ok((luminance(background) + 0.05) / (luminance([16, 32, 25]) + 0.05) >= 4.5);
}
console.log('CLIENT METRIC COLORS: palette, missing/equal/signed values, independent domains, metadata changes, stable client state and contrast passed');
