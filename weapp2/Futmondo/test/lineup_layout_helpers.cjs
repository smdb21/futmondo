const assert = require('node:assert/strict');

// Assert rendered geometry rather than a particular CSS layout implementation.
async function assertLineupLayout(page, layout) {
  await layout.waitFor();
  await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));
  const state = await layout.evaluate(node => {
    const rect = element => {
      const r = element.getBoundingClientRect();
      return { left: r.left, right: r.right, top: r.top, bottom: r.bottom,
        width: r.width, height: r.height, centerX: r.left + r.width / 2 };
    };
    const pitch = node.querySelector('.fm-lineup-pitch');
    const bench = node.querySelector('.fm-lineup-bench');
    const markings = pitch.querySelector('.fm-lineup-markings');
    const halfway = getComputedStyle(markings, '::before');
    const players = element => Array.from(element.querySelectorAll('.fm-lineup-player'), rect);
    return {
      desktop: window.innerWidth >= 992, viewportWidth: window.innerWidth,
      formation: pitch.getAttribute('aria-label').replace('Pitch, formation ', ''),
      layout: rect(node), pitch: rect(pitch), bench: rect(bench),
      rows: Array.from(pitch.querySelectorAll('.fm-lineup-row'), row => ({ box: rect(row), players: players(row) })),
      benchPlayers: players(bench), allPlayers: players(node),
      markings: rect(markings), firstGoal: rect(markings.querySelector('.fm-lineup-goal-top')),
      secondGoal: rect(markings.querySelector('.fm-lineup-goal-bottom')),
      halfway: { width: parseFloat(halfway.width), height: parseFloat(halfway.height),
        horizontalBorder: parseFloat(halfway.borderTopWidth) + parseFloat(halfway.borderBottomWidth),
        verticalBorder: parseFloat(halfway.borderLeftWidth) + parseFloat(halfway.borderRightWidth) }
    };
  });
  const counts = state.formation.split('-').map(Number);
  const expectedRows = [counts[2], counts[1], counts[0], 1].filter(Boolean);
  assert.deepEqual(state.rows.map(row => row.players.length), expectedRows, 'Every formation keeps its official slot groups');
  assert.equal(state.allPlayers.length, 15, 'The layout retains all eleven starters and four bench slots');
  for (const box of [state.layout, state.pitch, state.bench, ...state.allPlayers]) {
    assert.ok(box.width > 0 && box.height > 0 && box.left >= -1 && box.right <= state.viewportWidth + 1,
      'The pitch, bench and player cards fit the viewport');
  }
  const contained = (box, parent) => box.left >= parent.left - 1 && box.right <= parent.right + 1 &&
    box.top >= parent.top - 1 && box.bottom <= parent.bottom + 1;
  for (const row of state.rows) for (const player of row.players)
    assert.ok(contained(player, state.pitch), 'Every starter fits inside the pitch');
  for (const player of state.benchPlayers)
    assert.ok(contained(player, state.bench), 'Every substitute fits inside the bench');
  for (let i = 0; i < state.allPlayers.length; i++) {
    for (let j = i + 1; j < state.allPlayers.length; j++) {
      const a = state.allPlayers[i], b = state.allPlayers[j];
      assert.ok(Math.min(a.right, b.right) - Math.max(a.left, b.left) <= 1 ||
        Math.min(a.bottom, b.bottom) - Math.max(a.top, b.top) <= 1, 'Player cards never overlap');
    }
  }
  if (state.desktop) {
    assert.ok(state.bench.right <= state.pitch.left + 1, 'The bench sits to the far left of the pitch');
    assert.ok(state.bench.top < state.pitch.bottom && state.bench.bottom > state.pitch.top, 'Bench and pitch share a horizontal layout');
    // DOM order stays FWD, MID, DEF, GK so existing slot identity is unchanged.
    for (let i = 1; i < state.rows.length; i++)
      assert.ok(state.rows[i].box.right <= state.rows[i - 1].box.left + 1, 'Positions run left to right from goalkeeper to forwards');
    for (const players of [...state.rows.map(row => row.players), state.benchPlayers]) {
      for (let i = 1; i < players.length; i++) {
        assert.ok(Math.abs(players[i].centerX - players[0].centerX) <= 2, 'Players in each position form one vertical column');
        assert.ok(players[i].top >= players[i - 1].bottom - 1, 'Players are ordered vertically without overlap');
      }
    }
    assert.ok(Math.abs(state.firstGoal.left - state.markings.left) <= 2 &&
      Math.abs(state.secondGoal.right - state.markings.right) <= 2, 'Desktop goal boxes sit on the left and right touchlines');
    assert.ok(state.halfway.width <= 2 && state.halfway.height >= state.markings.height * .8 &&
      state.halfway.verticalBorder > 0, 'Desktop halfway line is vertical');
  } else {
    assert.ok(state.bench.top >= state.pitch.bottom - 1, 'Mobile keeps the bench below the pitch');
    for (let i = 1; i < state.rows.length; i++)
      assert.ok(state.rows[i].box.top >= state.rows[i - 1].box.bottom - 1, 'Mobile keeps the existing forward-to-goalkeeper rows');
    for (const players of [...state.rows.map(row => row.players), state.benchPlayers]) {
      for (let i = 1; i < players.length; i++)
        assert.ok(players[i].left >= players[i - 1].right - 1, 'Mobile players stay in horizontal position rows');
    }
    assert.ok(Math.abs(state.firstGoal.top - state.markings.top) <= 2 &&
      Math.abs(state.secondGoal.bottom - state.markings.bottom) <= 2, 'Mobile goal boxes stay at the top and bottom');
    assert.ok(state.halfway.height <= 2 && state.halfway.width >= state.markings.width * .8 &&
      state.halfway.horizontalBorder > 0, 'Mobile halfway line stays horizontal');
  }
}

module.exports = { assertLineupLayout };
