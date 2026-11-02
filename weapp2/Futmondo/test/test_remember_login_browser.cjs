const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const http = require('node:http');
const https = require('node:https');
const {spawn, spawnSync} = require('node:child_process');
const {randomBytes} = require('node:crypto');
const {once} = require('node:events');
const proxyWithoutOrigin = process.env.FUTMONDO_TEST_PROXY_NO_WS_ORIGIN === 'true';
const {chromium} = require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES || '/tmp/futmondo-review-tools/node_modules', 'playwright'));
(async () => {
  const tmp = fs.mkdtempSync(path.join(os.tmpdir(), 'fm-remember-browser-'));
  const backend = 18761;
  assert.equal(spawnSync('openssl', ['req', '-x509', '-newkey', 'rsa:2048', '-nodes', '-keyout', path.join(tmp, 'key.pem'), '-out', path.join(tmp, 'cert.pem'), '-subj', '/CN=localhost', '-days', '1'], {stdio: 'ignore'}).status, 0);
  let log = '';
  const fixtureKey = randomBytes(32).toString('hex');
  let app;
  async function startApp() {
    log = '';
    app = spawn('Rscript', ['test/test_remember_browser_app.R', String(backend), path.join(tmp,'records.json')],
      {env:{...process.env,FUTMONDO_TEST_REMEMBER_KEY:fixtureKey}});
    app.stdout.on('data', d => {log += d;}); app.stderr.on('data', d => {log += d;});
    for (let i=0;i<100 && !log.includes('Listening on');i++) await new Promise(resolve => setTimeout(resolve,100));
    assert.ok(log.includes('Listening on'), log);
  }
  let workerPrefix = '/_w_fixture_a/';
  const upstreamPath = url => url.replace(/^\/_w_fixture_[ab]\//, '/');
  const sockets = new Set();
  const proxy = https.createServer({key: fs.readFileSync(path.join(tmp, 'key.pem')), cert: fs.readFileSync(path.join(tmp, 'cert.pem'))}, (req, res) => {
    const upstream = http.request({hostname: '127.0.0.1', port: backend, path: upstreamPath(req.url), method: req.method, headers: {...req.headers,'accept-encoding':'identity'}}, reply => {
      const headers = {...reply.headers};
      if (req.url === '/' && /text\/html/.test(headers['content-type'] || '')) {
        let html = ''; reply.on('data', chunk => {html += chunk;});
        reply.on('end', () => {delete headers['content-length']; res.writeHead(reply.statusCode, headers); res.end(html.replace('<head>', `<head><base href="${workerPrefix}">`));});
      } else {res.writeHead(reply.statusCode, headers); reply.pipe(res);}
    });
    upstream.on('error', () => {res.writeHead(503); res.end();}); req.pipe(upstream);
  });
  proxy.on('connection', socket => {sockets.add(socket); socket.on('close', () => sockets.delete(socket));});
  proxy.on('upgrade', (req, downstream, head) => {
    const headers = {...req.headers};
    if (proxyWithoutOrigin) delete headers.origin;
    const upstream = http.request({hostname: '127.0.0.1', port: backend, path: upstreamPath(req.url), method: req.method, headers});
    upstream.on('upgrade', (reply, socket, upstreamHead) => {
      downstream.write(`HTTP/1.1 ${reply.statusCode} Switching Protocols\r\n` + Object.entries(reply.headers).map(([k,v]) => `${k}: ${v}\r\n`).join('') + '\r\n');
      if (head.length) socket.write(head); if (upstreamHead.length) downstream.write(upstreamHead);
      socket.on('error', () => downstream.destroy()); downstream.on('error', () => socket.destroy());
      socket.pipe(downstream).pipe(socket);
    });
    upstream.on('error', () => downstream.destroy()); upstream.end();
  });
  await new Promise(resolve => proxy.listen(0, '127.0.0.1', resolve));
  let context;
  try {
    await startApp();
    const openBrowser = () => chromium.launchPersistentContext(path.join(tmp,'browser-profile'),
      {headless:true,args:['--no-sandbox'],ignoreHTTPSErrors:true,viewport:{width:390,height:800}});
    context = await openBrowser();
    let page = await context.newPage(); page.setDefaultTimeout(10000);
    const endpointStatuses = [];
    function observePage(page) {
    page.on('pageerror', e => {log += '\nBROWSER: ' + e.message;});
    page.on('console', m => {if(m.type()==='error') log += '\nCONSOLE: ' + m.text();});
    page.on('response', async response => {if(response.url().includes('/dataobj/')) endpointStatuses.push(await response.text());});
    }
    observePage(page);
    const appUrl = `https://127.0.0.1:${proxy.address().port}/`;
    await page.goto(appUrl);
    await page.waitForFunction(() => window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected());
    await page.locator('#login-user_name').fill('fixture@example.invalid');
    await page.locator('#login-password').fill('synthetic-password');
    await page.locator('#login-remember_login').check();
    await page.locator('#login-login_button').click();
    await page.locator('#shiny-tab-today').waitFor({state: 'visible'});
    await page.locator('#login-remember_feedback').waitFor({state: 'visible'});
    await page.waitForFunction(() => document.querySelector('#login-remember_feedback').textContent.includes('remembered for 7 days'));
    assert.equal(await page.locator('#login-login_button').isVisible(), false);
    assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 2), false);
    for(let i=0;i<100 && !(await context.cookies()).some(c=>c.name.startsWith('__Secure-fm_remember_'));i++) await page.waitForTimeout(30);
    const cookie = (await context.cookies()).find(c => c.name.startsWith('__Secure-fm_remember_'));
    assert.ok(cookie, JSON.stringify(endpointStatuses));
    assert.equal(cookie.httpOnly, true); assert.equal(cookie.secure, true); assert.equal(cookie.sameSite, 'Lax'); assert.equal(cookie.path, '/');
    assert.ok(cookie.expires > Date.now()/1000 + 604700 && cookie.expires <= Date.now()/1000 + 604801);
    assert.equal(await page.evaluate(() => document.cookie.includes('fm_remember_')), false);
    assert.equal(await page.evaluate(() => Object.values(localStorage).some(v => /synthetic-browser-token|synthetic-password/.test(v))), false);
    assert.ok(endpointStatuses.some(x => JSON.parse(x).status === 'confirmed'));
    assert.ok((await page.evaluate(() => document.baseURI)).includes(workerPrefix));
    console.log('PASS HTTPS login confirms the browser returned its protected cookie on Today at mobile width');
    await page.reload();
    await page.locator('#shiny-tab-today').waitFor({state: 'visible'});
    await page.locator('.sidebar-menu a[data-value="login"]').evaluate(el => el.click());
    await page.waitForFunction(() => document.querySelector('#login-login_feedback').textContent.includes('Session restored'));
    assert.equal((await context.cookies()).find(c => c.name===cookie.name).expires, cookie.expires);
    assert.equal(await page.locator('#login-password').inputValue(), '');
    console.log('PASS new Shiny session restores login without extending cookie expiry');
    // Close the browser and restart the R process, with persisted synthetic storage
    // and a different Posit-style worker URL. The public cookie path must stay '/'.
    await context.close();context = null;
    const exited = once(app,'exit');app.kill('SIGTERM');await exited;
    workerPrefix = '/_w_fixture_b/';
    await startApp();
    context = await openBrowser();page = await context.newPage();page.setDefaultTimeout(10000);observePage(page);
    await page.setViewportSize({width:1440,height:900});
    await page.goto(appUrl);
    await page.locator('#shiny-tab-today').waitFor({state:'visible'});
    assert.ok((await page.evaluate(() => document.baseURI)).includes(workerPrefix));
    assert.equal((await context.cookies()).find(c => c.name===cookie.name).expires,cookie.expires);
    await page.locator('.sidebar-menu a[data-value="login"]').evaluate(el => el.click());
    await page.waitForFunction(() => document.querySelector('#login-login_feedback').textContent.includes('Session restored'));
    console.log('PASS browser closure and server restart restore through a new worker path with the same expiry');
    await page.locator('#login-logout_button').click();
    await page.waitForFunction(() => !document.querySelector('#login-login_feedback').textContent.includes('Session restored'));
    for(let i=0;i<100 && (await context.cookies()).some(c=>c.name===cookie.name);i++) await page.waitForTimeout(30);
    assert.equal((await context.cookies()).some(c=>c.name===cookie.name), false);
    await page.reload();
    await page.waitForFunction(() => window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected());
    await page.waitForTimeout(150);
    assert.equal(await page.locator('#login-login_feedback').textContent(), '');
    console.log('PASS logout expires cookie and next page load remains signed out');
    await page.locator('#login-user_name').fill('remember-failure@example.invalid');
    await page.locator('#login-password').fill('synthetic-password');
    await page.locator('#login-remember_login').check();
    await page.locator('#login-login_button').click();
    await page.locator('#shiny-tab-today').waitFor({state: 'visible'});
    await page.locator('#login-remember_feedback').waitFor({state: 'visible'});
    await page.waitForFunction(() => document.querySelector('#login-remember_feedback').textContent.includes('Remembering this device failed'));
    assert.equal((await context.cookies()).some(c => c.name.startsWith('__Secure-fm_remember_')), false);
    assert.equal(await page.locator('#login-login_button').isVisible(), false);
    assert.equal(await page.evaluate(() => document.documentElement.scrollWidth > innerWidth + 2), false);
    console.log('PASS failed persistence remains visible on Today without issuing a remembered cookie');
    console.log(`REMEMBER LOGIN BROWSER (${proxyWithoutOrigin ? 'proxied without WebSocket Origin' : 'original WebSocket Origin'}): 5 passed / 0 failed`);
  } catch(error) {
    throw new Error(error.stack + '\nAPP LOG: ' + log);
  } finally {
    if (context) await context.close();
    if (app) app.kill('SIGTERM'); for(const socket of sockets) socket.destroy(); proxy.closeAllConnections(); await new Promise(resolve => proxy.close(resolve)); fs.rmSync(tmp, {recursive: true, force: true});
  }
})().catch(error => {console.error(error.message); process.exitCode=1;});
