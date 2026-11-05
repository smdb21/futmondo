// Loopback-only delayed transport fixture. Never accepts account credentials.
const http = require('node:http'), fs = require('node:fs');
const runs = {};
const server = http.createServer((req, res) => {
  const url = new URL(req.url, 'http://localhost');
  const reply = (status, value) => { res.writeHead(status, {'content-type': 'application/json'}); res.end(JSON.stringify(value)); };
  if (url.pathname === '/shutdown') { reply(200, {}); server.close(); return; }
  const run = url.searchParams.get('run') || 'default';
  const state = runs[run] ||= {active: 0, maxActive: 0, events: [], counts: {}};
  if (url.pathname === '/stats') { reply(200, state); return; }
  const route = url.pathname, delay = Number(url.searchParams.get('delay') || 20);
  const count = state.counts[route] = (state.counts[route] || 0) + 1;
  const event = {route, arrival: Date.now(), delay, count};
  state.events.push(event); state.active++; state.maxActive = Math.max(state.maxActive, state.active);
  let active = true;
  const release = () => { if (active) { state.active--; active = false; } };
  res.once('close', release); req.resume();
  setTimeout(() => {
    release();
    if (res.destroyed) return;
    event.finished = Date.now(); event.maxActive = state.maxActive;
    reply(url.searchParams.get('fail_first') === '1' && count === 1 ? 503 : 200, [event]);
  }, delay);
});
server.listen(0, '127.0.0.1', () => fs.writeFileSync(process.argv[2], String(server.address().port)));
setTimeout(() => server.close(() => process.exit(0)), 30000).unref();
