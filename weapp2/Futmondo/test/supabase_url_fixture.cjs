// Synthetic REST storage. Incorrect duplicated prefixes deliberately return 404.
const http = require('node:http');
const fs = require('node:fs');
const scores = [], checks = [], requests = [];
const server = http.createServer((req, res) => {
  const url = new URL(req.url, 'http://localhost');
  const send = (status, value) => {
    res.writeHead(status, {'Content-Type': 'application/json'});
    res.end(JSON.stringify(value));
  };
  if (url.pathname === '/shutdown') { send(200, {}); server.close(); return; }
  if (url.pathname === '/stats') { send(200, requests); return; }
  requests.push({method: req.method, path: url.pathname});
  let body = '';
  req.on('data', chunk => { body += chunk; });
  req.on('end', () => setTimeout(() => {
    const table = url.pathname.replace(/^\/rest\/v1\//, '');
    if (req.method === 'GET' && table === 'player_round_dataset') {
      const league = (url.searchParams.get('championship_id') || '').replace(/^eq\./, '');
      const rows = scores.filter(x => x.championship_id === league).map(x => ({
        ...x, row_kind: 'point', checked_at: null, result_status: null, point_count: null
      })).concat(checks.filter(x => x.championship_id === league).map(x => ({
        ...x, row_kind: 'check', round: null, points: null, score_status: null,
        observed_at: null, occurred_at: null, round_start_at: null
      })));
      rows.sort((a, b) => a.player_id.localeCompare(b.player_id) ||
        a.row_kind.localeCompare(b.row_kind) || (a.round || 0) - (b.round || 0));
      const offset = Number(url.searchParams.get('offset') || 0);
      const limit = Number(url.searchParams.get('limit') || 1000);
      send(200, rows.slice(offset, offset + limit));
    } else if (req.method === 'POST' && ['player_match_observations', 'player_round_history_checks'].includes(table)) {
      const rows = JSON.parse(body);
      const target = table === 'player_match_observations' ? scores : checks;
      target.push(...(Array.isArray(rows) ? rows : [rows]));
      send(201, {});
    } else send(404, {code: 'PGRST125'});
  }, 80));
});
server.listen(0, '127.0.0.1', () => fs.writeFileSync(process.argv[2], String(server.address().port)));
setTimeout(() => server.close(() => process.exit(0)), 30000).unref();
