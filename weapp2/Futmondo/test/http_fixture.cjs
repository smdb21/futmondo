const http=require('node:http'),fs=require('node:fs');
const counts={};
const server=http.createServer((req,res)=>{
 const url=new URL(req.url,'http://localhost');
 const send=(code,data)=>{res.writeHead(code,{'Content-Type':'application/json'});res.end(JSON.stringify(data));};
 if(url.pathname==='/shutdown'){send(200,{});server.close();return;}
 if(url.pathname==='/stats'){send(200,counts);return;}
 counts[url.pathname]=(counts[url.pathname]||0)+1;req.resume();
 setTimeout(()=>{
  if(url.pathname==='/1/player/summary')send(200,{answer:{data:{id:'p',name:'Player',slug:'player'},championship:{season:'2026'},points:[{round:1,points:0},{round:2,points:-2},{round:3}],prices:[]}});
  else if(url.pathname==='/1/userteam/lineup')send(200,{answer:{pointSystem:'factual',custom:{},players:[],bench:{}}});
  else if(url.pathname==='/rest/v1/player_match_observations')send(503,{code:'offline'});
  else if(url.pathname==='/rest/v1/player_history')send(200,[]);
  else send(200,{ok:true});
 },url.pathname==='/slow'||url.pathname==='/cancel'?200:20);
});
server.listen(0,'127.0.0.1',()=>fs.writeFileSync(process.argv[2],String(server.address().port)));
setTimeout(()=>server.close(()=>process.exit(0)),25000).unref();
