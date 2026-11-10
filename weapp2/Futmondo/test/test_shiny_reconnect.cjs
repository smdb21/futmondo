#!/usr/bin/env node
'use strict';

// Run against a patched installed/extracted official Shiny Server, not a copy of
// its protocol: node test/test_shiny_reconnect.cjs /opt/shiny-server
const assert = require('assert/strict');
const fs = require('fs');
const os = require('os');
const path = require('path');
const vm = require('vm');
const {EventEmitter} = require('events');
const patcher = require('../scripts/patch-shiny-reconnect.cjs');
const root = path.resolve(process.argv[2] || '/opt/shiny-server');
const read = file => fs.readFileSync(path.join(root, file), 'utf8');
const duration = 1800 * 1000;
let passed = 0;
function check(name, action) {
  action();
  console.log(`PASS ${name}`);
  passed++;
}

// Only time and transport are fake; protocol/registry/buffer code is upstream.
class Clock {
  constructor() { this.now = 0; this.nextId = 1; this.pending = new Map(); }
  setTimeout = (fn, delay = 0) => {
    const id = this.nextId++;
    this.pending.set(id, {fn, due: this.now + Math.max(0, Number(delay) || 0)});
    return id;
  };
  clearTimeout = id => { this.pending.delete(id); };
  advance(ms) {
    const end = this.now + ms;
    let steps = 0;
    for (;;) {
      const next = [...this.pending.entries()].filter(([, t]) => t.due <= end)
        .sort((a, b) => a[1].due - b[1].due || a[0] - b[0])[0];
      if (!next) break;
      assert(++steps < 100000, 'timer loop must terminate');
      this.now = next[1].due;
      this.pending.delete(next[0]);
      next[1].fn();
    }
    this.now = end;
  }
}
class Socket extends EventEmitter {
  constructor(url) {
    super(); this.url = url; this.readyState = 1; this.writes = [];
    this.headers = {}; this.address = {}; this.closeCode = null;
  }
  write(message) { this.writes.push(message); }
  close(code) {
    this.closeCode = code;
    if (this.readyState === 3) return;
    this.readyState = 3;
    this.emit('close'); this.emit('end');
  }
}

const clock = new Clock();
const original = {setTimeout, clearTimeout, setImmediate, now: Date.now, logger: global.logger};
global.setTimeout = clock.setTimeout;
global.clearTimeout = clock.clearTimeout;
global.setImmediate = fn => clock.setTimeout(fn, 0);
Date.now = () => clock.now;
global.logger = {debug() {}, warn() {}, trace() {}};
try {
  const Registry = require(path.join(root, 'lib/proxy/robust-sockjs.js'));
  const errorcode = require(path.join(root, 'lib/proxy/errorcode.js'));
  const constructor = read(patcher.SERVER_FILE).match(/new RobustSockJS\(\d+\)/g);
  assert.equal(constructor?.length, 1);
  const registry = () => vm.runInNewContext(constructor[0], {RobustSockJS: Registry});

  check('matching 1800-second server and injected browser windows', () => {
    assert.equal(registry()._timeout, duration);
    const adapter = read(patcher.ADAPTER_FILE);
    const template = adapter.match(/preShinyInit\(\{reconnect:%s,[^"\n]+/g);
    assert.equal(template?.length, 1);
    let options;
    vm.runInNewContext(template[0].replace('%s', 'true').replace('%s', ''),
      {preShinyInit: value => { options = value; }});
    assert.equal(options.reconnect, true);
    assert.equal(options.reconnectTimeout, duration);
    const client = require(path.join(root, 'node_modules/shiny-server-client/lib/decorators/reconnect.js'));
    require(path.join(root, 'node_modules/shiny-server-client/lib/log.js')).suppress = true;
    require(path.join(root, 'node_modules/shiny-server-client/lib/debug.js')).suppress = true;
    let connection;
    client.decorate(() => {}, options)('/__sockjs__/', new EventEmitter(), (_, conn) => { connection = conn; });
    assert.equal(connection._conn._timeout, duration, 'real client consumes the injected option');
    clock.advance(0);
    connection.close();
    assert.equal(clock.pending.size, 0);
  });

  for (const seconds of [16, 1799]) {
    check(`same logical session resumes after ${seconds} seconds, replay and ACK release its buffer`, () => {
      const reg = registry();
      const physical = new Socket('/__sockjs__/n=same_session');
      const logical = reg.robustify(physical);
      logical.state = {unsavedSelection: 'kept'};
      physical.emit('data', '0#initial');
      clock.advance(2000); // Finish the upstream message-receiver ACK timer.
      physical.close();
      logical.write('while disconnected');
      clock.advance(seconds * 1000);
      assert.equal(logical.readyState, 1);
      const resumed = new Socket('/__sockjs__/o=same_session');
      assert.equal(reg.robustify(resumed), undefined, 'existing channel is reused');
      assert.equal(reg._connections.same_session, logical);
      assert.equal(logical.state.unsavedSelection, 'kept');
      assert.equal(clock.pending.size, 0, 'original expiry timer is canceled');
      assert.equal(resumed.writes[0], 'CONTINUE 1');
      resumed.emit('data', 'CONTINUE 0');
      assert.equal(resumed.writes[1], '0#while disconnected');
      resumed.emit('data', 'ACK 1');
      assert.equal(logical._messageBuffer._messages.length, 0);
      clock.advance(duration);
      assert.equal(reg._connections.same_session, logical, 'canceled expiry cannot kill a restored session');
      resumed.close();
      clock.advance(duration);
      assert.equal(Object.keys(reg._connections).length, 0);
      assert.equal(clock.pending.size, 0);
    });
  }

  check('deadline expires at 1800 seconds, releases registry and closes channel once', () => {
    const reg = registry();
    const physical = new Socket('/__sockjs__/n=expires');
    const logical = reg.robustify(physical);
    let closes = 0;
    let ends = 0;
    logical.on('close', () => { closes++; });
    logical.on('end', () => { ends++; });
    physical.close();
    // Model the app's existing one-second countdown while the phone is asleep.
    for (let i = 0; i < 1799; i++) {
      logical.write(`countdown ${i}`);
      clock.advance(1000);
    }
    assert.equal(logical._messageBuffer._messages.length, 1799);
    clock.advance(999);
    assert.equal(reg._connections.expires, logical);
    assert.equal(closes, 0);
    clock.advance(1);
    assert.equal(logical.readyState, 3);
    assert.equal(Object.keys(reg._connections).length, 0, 'registry no longer retains the buffer/session');
    assert.equal(logical._withheld.events.length, 0);
    assert.equal(closes, 1);
    assert.equal(ends, 1);
    assert.equal(clock.pending.size, 0);
    const late = new Socket('/__sockjs__/o=expires');
    assert.equal(reg.robustify(late), undefined);
    assert.equal(late.closeCode, errorcode.BAD_IDENTIFIER);
    assert.equal(Object.keys(reg._connections).length, 0, 'expired reconnect cannot create a replacement');
    console.log('INFO upstream buffers one entry per unacknowledged output; expiry removes registry ownership, ACK discards replayed entries.');
  });

  check('upstream shape changes fail before modifying either target', () => {
    const fixture = fs.mkdtempSync(path.join(os.tmpdir(), 'futmondo-reconnect-shape-'));
    const files = [patcher.SERVER_FILE, patcher.ADAPTER_FILE, 'lib/proxy/robust-sockjs.js',
      'node_modules/shiny-server-client/lib/decorators/reconnect.js',
      'node_modules/shiny-server-client/dist/shiny-server-client.js',
      'node_modules/shiny-server-client/dist/shiny-server-client.min.js'];
    try {
      for (const file of files) {
        fs.mkdirSync(path.dirname(path.join(fixture, file)), {recursive: true});
        fs.writeFileSync(path.join(fixture, file), read(file)
          .replace(patcher.SERVER_AFTER, patcher.SERVER_BEFORE)
          .replace(patcher.BROWSER_AFTER, patcher.BROWSER_BEFORE));
      }
      assert.equal(patcher.planPatch(fixture).length, 2);
      const adapterPath = path.join(fixture, patcher.ADAPTER_FILE);
      const changed = fs.readFileSync(adapterPath, 'utf8').replace(patcher.BROWSER_BEFORE, 'changedUpstream();');
      fs.writeFileSync(adapterPath, changed);
      const beforeServer = fs.readFileSync(path.join(fixture, patcher.SERVER_FILE), 'utf8');
      assert.throws(() => patcher.patch(fixture), /Unsupported Shiny Server source/);
      assert.equal(fs.readFileSync(path.join(fixture, patcher.SERVER_FILE), 'utf8'), beforeServer);
      assert.equal(fs.readFileSync(adapterPath, 'utf8'), changed);
    } finally { fs.rmSync(fixture, {recursive: true, force: true}); }
  });
  console.log(`SHINY RECONNECT: ${passed} passed / 0 failed`);
} finally {
  global.setTimeout = original.setTimeout;
  global.clearTimeout = original.clearTimeout;
  global.setImmediate = original.setImmediate;
  Date.now = original.now;
  if (original.logger === undefined) delete global.logger;
  else global.logger = original.logger;
}
