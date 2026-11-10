#!/usr/bin/env node
'use strict';

// Build-only customization of the installed OSS Shiny Server. Keep both ends of
// its existing-session protocol at the user's requested, bounded 30 minutes.
// Upstream: rstudio/shiny-server lib/proxy/{sockjs,robust-sockjs}.js,
// R/SockJSAdapter.R; rstudio/shiny-server-client lib/decorators/reconnect.js.
const fs = require('fs');
const path = require('path');
const vm = require('vm');

const RETENTION_SECONDS = 1800;
const SERVER_FILE = 'lib/proxy/sockjs.js';
const ADAPTER_FILE = 'R/SockJSAdapter.R';
const SERVER_BEFORE = 'var robust = new RobustSockJS();';
const BROWSER_BEFORE = 'preShinyInit({reconnect:%s,disableProtocols:[%s]});';
const SERVER_AFTER = `var robust = new RobustSockJS(${RETENTION_SECONDS});`;
const BROWSER_AFTER = `preShinyInit({reconnect:%s,reconnectTimeout:${RETENTION_SECONDS * 1000},disableProtocols:[%s]});`;

function expectOnce(source, literal, file) {
  if (source.split(literal).length !== 2) {
    throw new Error(`Unsupported Shiny Server source in ${file}: expected exactly one ${JSON.stringify(literal)}. Review the upstream reconnect implementation before updating this patch.`);
  }
}

function planPatch(root) {
  const read = file => fs.readFileSync(path.join(root, file), 'utf8');
  const server = read(SERVER_FILE);
  const adapter = read(ADAPTER_FILE);
  const robust = read('lib/proxy/robust-sockjs.js');
  const clientFile = 'node_modules/shiny-server-client/lib/decorators/reconnect.js';
  const client = read(clientFile);

  // Reject changed upstream contracts instead of building with a one-sided or
  // ineffective timeout. The behavioral check also runs against this release.
  expectOnce(server, SERVER_BEFORE, SERVER_FILE);
  expectOnce(adapter, BROWSER_BEFORE, ADAPTER_FILE);
  expectOnce(robust, 'function RobustSockJSRegistry(timeout)', 'lib/proxy/robust-sockjs.js');
  expectOnce(robust, 'this._timeout = timeout * 1000;', 'lib/proxy/robust-sockjs.js');
  expectOnce(robust, 'delete self._connections[id];', 'lib/proxy/robust-sockjs.js');
  expectOnce(client, 'let timeout = options.reconnectTimeout;', clientFile);
  for (const asset of ['shiny-server-client.js', 'shiny-server-client.min.js']) {
    const file = `node_modules/shiny-server-client/dist/${asset}`;
    expectOnce(read(file), '.reconnectTimeout', file);
  }
  const updatedServer = server.replace(SERVER_BEFORE, SERVER_AFTER);
  new vm.Script(updatedServer, {filename: SERVER_FILE});
  return [
    {file: SERVER_FILE, source: updatedServer},
    {file: ADAPTER_FILE, source: adapter.replace(BROWSER_BEFORE, BROWSER_AFTER)}
  ];
}

function patch(root) {
  // Validate every target before writing either one.
  const changes = planPatch(root);
  for (const change of changes) {
    fs.writeFileSync(path.join(root, change.file), change.source);
  }
  return changes.map(change => change.file);
}

module.exports = {patch, planPatch, RETENTION_SECONDS, SERVER_FILE, ADAPTER_FILE,
  SERVER_BEFORE, BROWSER_BEFORE, SERVER_AFTER, BROWSER_AFTER};

if (require.main === module) {
  try {
    const root = process.argv[2] || '/opt/shiny-server';
    const files = patch(root);
    console.log(`Shiny existing-session reconnect: ${RETENTION_SECONDS}s server / ${RETENTION_SECONDS * 1000}ms browser (${files.join(', ')}).`);
  } catch (error) {
    console.error(`Shiny reconnect build patch failed: ${error.message}`);
    process.exitCode = 1;
  }
}
