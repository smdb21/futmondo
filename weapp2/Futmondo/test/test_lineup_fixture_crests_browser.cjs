const assert=require('node:assert/strict');
const path=require('node:path');
const {chromium}=require(path.join(process.env.FUTMONDO_TEST_NODE_MODULES||'/tmp/futmondo-review-tools/node_modules','playwright'));
(async()=>{
 const browser=await chromium.launch({headless:true,args:['--no-sandbox'],executablePath:process.env.FUTMONDO_TEST_CHROMIUM||undefined});
 try{
  for(const width of [320,390,1440])for(const theme of ['light','dark']){
   const page=await browser.newPage({viewport:{width,height:900},colorScheme:theme});
   const errors=[];page.on('pageerror',e=>errors.push(e.message));
   await page.route('**/*',route=>{
    if(new URL(route.request().url()).hostname==='127.0.0.1')return route.continue();
    if(/\/fixture-(home|away|club)\.png$/.test(route.request().url()))return route.fulfill({contentType:'image/svg+xml',
     body:'<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24"><circle cx="12" cy="12" r="11" fill="orange"/></svg>'});
    return route.abort();
   });
   await page.goto(`http://127.0.0.1:${process.env.FUTMONDO_EDITOR_TEST_PORT||8558}`);
   await page.evaluate(t=>document.documentElement.setAttribute('data-fm-theme',t),theme);
   assert.equal(await page.evaluate(()=>document.documentElement.getAttribute('data-fm-theme')),theme);
   await page.locator('.fm-lineup-fixture').nth(9).waitFor();
   await page.locator('.fm-lineup-player').nth(14).waitFor();
   assert.equal(await page.locator('.fm-lineup-fixture-crest').count(),0,'Matches render before the catalogue arrives');
   assert.match(await page.locator('.fm-lineup-fixtures h3').innerText(),/Round 6/);
   await page.locator('.fm-lineup-fixtures').scrollIntoViewIfNeeded();
   await page.waitForFunction(()=>{
    const counts=document.querySelector('#fixture_counts').textContent.split('/').map(Number);
    return counts[0]>0&&counts[0]===counts[3];
   });
   const counts=await page.locator('#fixture_counts').innerText();
   const draft=await page.locator('.fm-lineup-player').evaluateAll(nodes=>nodes.map(n=>n.dataset.playerId));
   await page.evaluate(()=>Shiny.setInputValue('publish_clubs',1,{priority:'event'}));
   await page.locator('.fm-lineup-fixture-crest').nth(19).waitFor();
   const images=page.locator('.fm-lineup-fixture-crest');
   const urls=await images.evaluateAll(nodes=>nodes.map(n=>n.src));
   assert.ok(urls.every((url,i)=>url.endsWith(i%2?'/fixture-away.png':'/fixture-home.png')));
   for(let i=0;i<await images.count();i++){
    await images.nth(i).scrollIntoViewIfNeeded();
    await images.nth(i).evaluate(img=>img.complete&&img.naturalWidth>0?null:new Promise((resolve,reject)=>{
     img.addEventListener('load',resolve,{once:true});img.addEventListener('error',reject,{once:true});
    }));
   }
   assert.ok(await page.locator('.fm-lineup-fixture-team').evaluateAll(nodes=>nodes.every(node=>{
    const crest=node.querySelector('img').getBoundingClientRect(),name=node.querySelector('.fm-lineup-fixture-team-name').getBoundingClientRect();
    return (node.classList.contains('fm-lineup-fixture-away')?name.right<=crest.left+1:crest.right<=name.left+1)&&crest.width===24;
   })));
   assert.deepEqual(await page.locator('.fm-lineup-player').evaluateAll(nodes=>nodes.map(n=>n.dataset.playerId)),draft);
   assert.equal(await page.locator('#fixture_counts').innerText(),counts,'Crest publication does not reload/save the lineup');
   assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth>innerWidth+2),false);
   assert.deepEqual(errors,[]);
   await page.locator('.fm-lineup-fixtures').screenshot({path:`/tmp/futmondo-matchup-crests-${width}-${theme}.png`});
   console.log(`PASS ${width}px ${theme}: ten visible games, twenty loaded crests, outer-edge placement, no overflow or lineup I/O`);
   await page.close();
  }
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
