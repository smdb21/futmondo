const {chromium}=require('playwright');
(async()=>{const b=await chromium.launch({headless:true,args:['--no-sandbox']});try{
const p=await b.newPage();let errors=[];p.on('pageerror',e=>errors.push(e.message));await p.goto(process.env.FUTMONDO_TEST_URL||'http://127.0.0.1:8492');await p.getByText('Beta',{exact:true}).waitFor();
await p.getByRole('row').filter({hasText:'Beta'}).getByRole('radio').click();await p.waitForFunction(()=>document.querySelector('#chosen').textContent==='b');
await p.getByRole('button',{name:'Update balances'}).click();await p.waitForFunction(()=>Reactable.getState('tbl').selected?.[0]===0);
if(await p.locator('#chosen').textContent()!=='b')throw Error('Selection lost after balance update');
const headers=await p.locator('[role=columnheader]').allTextContents();if(headers.some(x=>/Status|Confidence/.test(x)))throw Error('Removed columns still present');
await p.getByRole('button',{name:'Change league'}).click();await p.waitForFunction(()=>document.querySelector('#chosen').textContent==='none');
if(errors.length)throw Error(errors.join(';'));
console.log('PASS selected team ID and radio selection survive reordering and balance update; league change clears; Status/Confidence absent');
}finally{await b.close()}})().catch(e=>{console.error(e);process.exitCode=1});
