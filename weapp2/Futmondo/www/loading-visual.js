(function () {
  'use strict';
  var selector = '[data-fm-loading]';
  var instances = new Map();
  var reduced = window.matchMedia('(prefers-reduced-motion: reduce)');
  var frame = 0;
  var epoch = null;
  var lastFrame = -Infinity;
  var tau = Math.PI * 2;
  // Time to rise and read the figures, then watch the cloud in motion.
  var riseStart = 2.2, riseDuration = 2.6;
  var takeoffStart = 6.7, takeoffDuration = 1.7;
  var flockStart = takeoffStart + takeoffDuration;
  var radarStart = 12.7, radarDuration = 2;
  var detectionStart = radarStart + radarDuration;
  var returnStart = detectionStart + 3, returnDuration = 2;
  var loopDuration = returnStart + returnDuration;
  var root = document.documentElement;
  var palette;
  var pendingSync = false;
  // Classic club color schemes; select once per page, never per frame or theme.
  // Keep different primary-color families so both teams remain distinguishable.
  var clubs = [
    { id: 'real-madrid', family: 'white', base: ['#f7f8fc','#f7f8fc'] },
    { id: 'barcelona', family: 'blue', base: ['#356ed1','#2456a6'], stripe: ['#cf3659','#b9234b'] },
    { id: 'atletico', family: 'red', base: ['#e64a57','#b92338'], stripe: ['#f7f8fc','#f7f8fc'] },
    { id: 'betis', family: 'green', base: ['#36b97b','#087846'], stripe: ['#f7f8fc','#f7f8fc'] },
    { id: 'inter', family: 'blue', base: ['#4082df','#1759a8'], stripe: ['#111827','#111827'] },
    { id: 'milan', family: 'red', base: ['#e74a4f','#ba2331'], stripe: ['#111827','#111827'] },
    { id: 'dortmund', family: 'yellow', base: ['#f5d638','#f5d638'], stripe: ['#111827','#111827'], edge: '#806000' },
    { id: 'juventus', family: 'white', base: ['#f7f8fc','#f7f8fc'], stripe: ['#111827','#111827'] },
    { id: 'man-city', family: 'blue', base: ['#7dc9ef','#7dc9ef'], edge: '#376b83' },
    { id: 'liverpool', family: 'red', base: ['#ee4c5b','#bb1f38'] }
  ];
  var firstClub = clubs[Math.floor(Math.random()*clubs.length)];
  var opponents = clubs.filter(function (club) { return club.family !== firstClub.family; });
  var selectedClubs = [firstClub,opponents[Math.floor(Math.random()*opponents.length)]];
  var teamIds = selectedClubs.map(function (club) { return club.id; }).join(' ');
  var paths = [
    [[50,40],[370,40],[370,240],[50,240],[50,40]],
    [[210,40],[210,240]],
    [[50,91],[99,91],[99,189],[50,189]],
    [[370,91],[321,91],[321,189],[370,189]],
    [[50,116],[68,116],[68,164],[50,164]],
    [[370,116],[352,116],[352,164],[370,164]],
    [[50,124],[43,124],[43,156],[50,156]],
    [[370,124],[377,124],[377,156],[370,156]]
  ];
  var circle = [];
  for (var c = 0; c <= 48; c++) circle.push([210 + 36 * Math.cos(c * tau / 48), 140 + 36 * Math.sin(c * tau / 48)]);
  paths.push(circle);
  var segments = [];
  var pathLength = 0;
  paths.forEach(function (path) {
    for (var i = 1; i < path.length; i++) {
      var a = path[i - 1], b = path[i];
      var length = Math.hypot(b[0] - a[0], b[1] - a[1]);
      segments.push({ a: a, b: b, start: pathLength, length: length });
      pathLength += length;
    }
  });
  var formation = [[.06,.5],[.22,.15],[.25,.38],[.25,.62],[.22,.85],[.44,.22],[.41,.5],[.44,.78],[.64,.18],[.72,.5],[.64,.82]];
  var actors = [];
  for (var team = 0; team < 2; team++) {
    formation.forEach(function (f, index) {
      actors.push({ x: 62 + (team ? 1-f[0] : f[0])*296, y: 55+f[1]*170,
        team: team, facing: team ? -1 : 1, delay: (index % 4)*.12 });
    });
  }
  // Molded table-football body with a short shoe, not a projecting plinth.
  var figurine = [[-2,24],[-5.5,22],[-6,16],[-3.5,14],[-2.5,3],[-3.2,3],
    [-3.2,0],[3.2,0],[3.2,3],[2.5,3],[3.5,14],[6,16],[5.5,22],[2,24]];
  // Local X is forward toward the opponent's goal; shirt width lies across Y.
  function frontDepth(width, z) { return z < 3 || (z === 3 && Math.abs(width) >= 3) ? 3.2 : z < 14 ? 2.2 : 3; }
  var frontFace = figurine.map(function (v) { return [frontDepth(v[0],v[1]),v[0],v[1]]; });
  var backFace = figurine.map(function (v) { return [v[1] <= 3 ? -1.6 : -2.5,v[0],v[1]]; });
  var sideFace = frontFace.slice(7).concat(backFace.slice(7).reverse());
  var shirtTop = [[-2.5,-2,24],[3,-2,24],[3,2,24],[-2.5,2,24]];
  var footTop = [[-1.6,-3.2,3],[3.2,-3.2,3],[3.2,3.2,3],[-1.6,3.2,3]];
  // A stripe on both shirt planes stays readable while players face the goals.
  var frontStripe = [[3,-1.6,14],[3,1.6,14],[3,1.6,23],[3,-1.6,23]];
  var backStripe = frontStripe.map(function (v) { return [-2.5,v[1],v[2]]; });
  var sideStripe = [[-.5,6,16],[1.8,6,16],[1.8,5.5,22],[-.5,5.5,22]];
  // Sample the same head, shirt and short shoe for the silhouette-to-flock morph.
  var silhouette = [[0,30],[-2.9,27],[0,24],[2.9,27],[-5.5,22],[5.5,22],
    [-6,16],[6,16],[-3.5,14],[3.5,14],[-2.5,3],[2.5,3],[-3.2,3],[3.2,3],[-3.2,0],[3.2,0]];
  function clamp(x) { return Math.max(0, Math.min(1, x)); }
  function ease(x) { x = clamp(x); return x * x * x * (x * (x * 6 - 15) + 10); }
  function mix(a, b, w) { return a + (b - a) * w; }
  function cameraAt(t, instance) {
    var turn = ease((t-1.2)/2);
    var yaw = turn*(-.32 + .54*ease((t-2.6)/1.9));
    var tilt = turn*.96;
    var camera = { cy: Math.cos(yaw), sy: Math.sin(yaw), ct: Math.cos(tilt),
      st: Math.sin(tilt), zoom: 1-.08*turn, lift: 0, turn: turn };
    camera.eyeX = 210+600*camera.sy*camera.st;
    // Fit the board/shadow in short portrait scenes before placing it below
    // the sky. Extra scaling is needed only when perspective exceeds the box.
    var top, bottom;
    for (var pass = 0; pass < 2; pass++) {
      top = Infinity; bottom = -Infinity;
      for (var i = 0; i < 4; i++) {
        project(camera,paths[0][i][0],paths[0][i][1],0);
        top = Math.min(top,camera.py); bottom = Math.max(bottom,camera.py);
        project(camera,paths[0][i][0],paths[0][i][1],-9);
        top = Math.min(top,camera.py+3); bottom = Math.max(bottom,camera.py+3);
      }
      if (!pass) camera.zoom *= Math.min(1,(instance.height-16*turn)/(bottom-top));
    }
    var low = 7*turn-instance.offset-top;
    var high = instance.height-instance.offset-7*turn-bottom;
    camera.lift = Math.max(low,Math.min(high,(9+(instance.mobile ? instance.height*.20 : 25))*turn));
    return camera;
  }
  // Perspective is a small projection of the same 2D pitch coordinates, not a
  // second render engine. Reuse the camera's output slots instead of point arrays.
  function project(camera, x, y, z) {
    x -= 210; y -= 140;
    var rx = camera.cy*x-camera.sy*y;
    var ry = camera.sy*x+camera.cy*y;
    var depth = ry*camera.st+z*camera.ct;
    camera.k = camera.zoom*600/(600-depth);
    camera.px = 210+rx*camera.k;
    camera.py = 140+(ry*camera.ct-z*camera.st)*camera.k+camera.lift;
  }
  function readPalette() {
    var style = window.getComputedStyle(root);
    function token(name) { return style.getPropertyValue('--fm-' + name).trim(); }
    palette = { ink: token('text'), muted: token('muted'), accent: token('accent'), surface: token('surface') };
    palette.light = root.getAttribute('data-fm-theme') === 'light';
    var mode = palette.light ? 1 : 0;
    palette.teams = selectedClubs.map(function (club) { return club.base[mode]; });
    palette.stripes = selectedClubs.map(function (club) { return club.stripe ? club.stripe[mode] : null; });
    palette.whiteEdge = palette.light ? '#657286' : '#a5afbf';
    palette.edges = selectedClubs.map(function (club) {
      return club.family === 'white' ? palette.whiteEdge : palette.light && club.edge || null;
    });
    palette.particleEdges = palette.stripes.map(function (color, team) {
      return color === '#111827' ? palette.teams[team] :
        color === '#f7f8fc' && palette.light ? palette.whiteEdge : null;
    });
  }
  function makeParticles(count) {
    var result = [];
    var segmentIndex = 0;
    for (var i = 0; i < count; i++) {
      var distance = (i + .5) / count * pathLength;
      while (segmentIndex < segments.length - 1 && distance > segments[segmentIndex].start + segments[segmentIndex].length) segmentIndex++;
      var segment = segments[segmentIndex];
      var f = (distance - segment.start) / segment.length;
      var angle = i * 2.399963229728653;
      // Keep longitudinal position independent of the golden-angle cross-section.
      // Correlating both sequences collapses the cloud into one thin curve.
      var u = (i + .5) / count * 2 - 1;
      var ring = i % 3;
      var actor = i % actors.length;
      var samples = Math.floor((count-1-actor)/actors.length)+1;
      var slot = Math.floor(i/actors.length);
      // Keep both shoe corners even in the compact eight-sample figures.
      var body = slot >= samples-2 ? silhouette[silhouette.length-2+slot-(samples-2)] :
        silhouette[Math.floor(slot/(samples-2)*(silhouette.length-2))];
      var stripe = body[1] >= 16 && body[1] <= 22 && body[0] > 0;
      var bodyX = body[1] >= 24 ? 0 : actors[actor].facing*(stripe ? 1 : frontDepth(body[0],body[1]));
      var bodyY = body[0];
      if (body[1] === 27 && body[0] > 0) { bodyX = actors[actor].facing*3.8; bodyY = 0; }
      result.push({ x: mix(segment.a[0], segment.b[0], f), y: mix(segment.a[1], segment.b[1], f),
        actor: actor, bodyX: bodyX, bodyY: bodyY, bodyZ: body[1], stripe: stripe,
        release: takeoffStart + actors[actor].delay*.25 + (1-body[1]/30)*.36,
        angle: angle, u: u, radius: Math.sqrt(((i * .754877666) % 1)), ring: 32 + ring * 32,
        ringAngle: Math.floor(i / 3) / Math.ceil(count / 3) * tau, depth: (Math.sin(angle) + 1) * .5 });
    }
    return result;
  }
  function pitch(ctx, opacity, markScale, camera) {
    if (opacity <= .001) return;
    ctx.globalAlpha = opacity;
    ctx.strokeStyle = palette.ink;
    ctx.lineWidth = .85 * markScale;
    ctx.beginPath();
    paths.forEach(function (path) {
      for (var i = 0; i < path.length; i++) {
        var x = path[i][0], y = path[i][1];
        if (camera) { project(camera,x,y,0); x = camera.px; y = camera.py; }
        if (i === 0) ctx.moveTo(x,y); else ctx.lineTo(x,y);
      }
    });
    ctx.stroke();
    ctx.fillStyle = palette.ink;
    [88,210,332].forEach(function (x) {
      var y = 140;
      if (camera) { project(camera,x,y,0); x = camera.px; y = camera.py; }
      ctx.beginPath(); ctx.arc(x,y,1.25,0,tau); ctx.fill();
    });
  }
  function raisedBoard(ctx, opacity, markScale, camera) {
    if (opacity <= .001 || camera.turn <= .001) return;
    var corners = paths[0];
    // A thin lower edge and a restrained shadow make the turning plane readable.
    ctx.fillStyle = palette.ink;
    ctx.globalAlpha = opacity*camera.turn*.045;
    ctx.beginPath();
    for (var i = 0; i < corners.length; i++) {
      project(camera,corners[i][0],corners[i][1],-9);
      if (i === 0) ctx.moveTo(camera.px+3,camera.py+3); else ctx.lineTo(camera.px+3,camera.py+3);
    }
    ctx.fill();
    ctx.strokeStyle = palette.ink;
    ctx.lineWidth = .75*markScale;
    ctx.globalAlpha = opacity*camera.turn*.3;
    ctx.beginPath();
    for (var j = 0; j < corners.length; j++) {
      project(camera,corners[j][0],corners[j][1],-4);
      if (j === 0) ctx.moveTo(camera.px,camera.py); else ctx.lineTo(camera.px,camera.py);
    }
    for (var k = 0; k < corners.length-1; k++) {
      project(camera,corners[k][0],corners[k][1],0); ctx.moveTo(camera.px,camera.py);
      project(camera,corners[k][0],corners[k][1],-4); ctx.lineTo(camera.px,camera.py);
    }
    ctx.stroke();
  }
  // Clip cached volume faces in local height, so the dissolve follows Z even
  // when a shoulder spans Y. Intersections reuse camera slots, without arrays.
  function playerFace(ctx, camera, actor, vertices, rise, cut, outline) {
    var started = false;
    ctx.beginPath();
    for (var i = 0; i < vertices.length; i++) {
      var v = vertices[i], previous = vertices[(i+vertices.length-1)%vertices.length];
      var inside = v[2] <= cut, previousInside = previous[2] <= cut;
      if (inside !== previousInside) {
        var w = (cut-previous[2])/(v[2]-previous[2]);
        project(camera,actor.x+actor.facing*mix(previous[0],v[0],w),
          actor.y+mix(previous[1],v[1],w),cut*rise);
        if (started) ctx.lineTo(camera.px,camera.py); else { ctx.moveTo(camera.px,camera.py); started = true; }
      }
      if (inside) {
        project(camera,actor.x+actor.facing*v[0],actor.y+v[1],v[2]*rise);
        if (started) ctx.lineTo(camera.px,camera.py); else { ctx.moveTo(camera.px,camera.py); started = true; }
      }
    }
    if (started) { ctx.closePath(); ctx.fill(); if (outline) ctx.stroke(); }
  }
  function risingPlayers(ctx, opacity, markScale, camera, lifts, t) {
    if (opacity <= .001) return;
    for (var i = 0; i < actors.length; i++) {
      var actor = actors[i], rise = lifts[i];
      if (rise <= .001) continue;
      var color = palette.teams[actor.team];
      project(camera,actor.x,actor.y,0);
      var footX = camera.px, footY = camera.py;
      ctx.fillStyle = color;
      ctx.globalAlpha = opacity*rise*.14;
      ctx.beginPath(); ctx.ellipse(footX,footY,4*camera.k,1.5*camera.k,0,0,tau); ctx.fill();
      var cut = 30*(1-clamp((t-takeoffStart-actor.delay*.25)/.36));
      if (cut <= 0) continue;
      // Select the camera-facing X surface, then shade the visible +Y side.
      // This gives opposing teams actual depth rather than rotated billboards.
      var front = (camera.eyeX-actor.x)*actor.facing >= 0;
      var outline = palette.edges[actor.team];
      ctx.strokeStyle = outline || color;
      ctx.lineWidth = .55*markScale;
      ctx.globalAlpha = opacity*rise*.9;
      playerFace(ctx,camera,actor,front ? frontFace : backFace,rise,cut,outline);
      ctx.globalAlpha = opacity*rise*.58;
      playerFace(ctx,camera,actor,sideFace,rise,cut,outline);
      ctx.globalAlpha = opacity*rise*.75;
      playerFace(ctx,camera,actor,shirtTop,rise,cut,outline);
      ctx.globalAlpha = opacity*rise*.8;
      playerFace(ctx,camera,actor,footTop,rise,cut,outline);
      if (palette.stripes[actor.team]) {
        ctx.fillStyle = palette.stripes[actor.team];
        ctx.globalAlpha = opacity*rise*.95;
        playerFace(ctx,camera,actor,front ? frontStripe : backStripe,rise,cut);
        playerFace(ctx,camera,actor,sideStripe,rise,cut);
        ctx.fillStyle = color;
      }
      if (cut > 24) {
        var clipped = cut < 30;
        if (clipped) {
          project(camera,actor.x,actor.y,cut*rise);
          ctx.save(); ctx.beginPath(); ctx.rect(0,camera.py,420,420); ctx.clip();
        }
        var headScale = Math.sqrt(markScale);
        ctx.globalAlpha = opacity*rise*.9;
        project(camera,actor.x,actor.y,27*rise);
        ctx.beginPath(); ctx.arc(camera.px,camera.py,2.9*camera.k*headScale*rise,0,tau); ctx.fill();
        if (outline) ctx.stroke();
        // A small side-profile nose makes the two goal-facing directions clear.
        ctx.beginPath();
        project(camera,actor.x+actor.facing*2.1*headScale,actor.y,28*rise); ctx.moveTo(camera.px,camera.py);
        project(camera,actor.x+actor.facing*4*headScale,actor.y,27*rise); ctx.lineTo(camera.px,camera.py);
        project(camera,actor.x+actor.facing*2.1*headScale,actor.y,26*rise); ctx.lineTo(camera.px,camera.py);
        ctx.closePath(); ctx.fill(); if (outline) ctx.stroke();
        if (clipped) ctx.restore();
      }
    }
  }
  function radar(ctx, opacity, t, markScale, fit) {
    if (opacity <= .001) return;
    ctx.strokeStyle = palette.ink;
    ctx.lineWidth = .6 * markScale;
    ctx.globalAlpha = opacity * .24;
    ctx.beginPath();
    [32,64,96].forEach(function (r) { r *= fit; ctx.moveTo(210+r,140); ctx.arc(210,140,r,0,tau); });
    ctx.moveTo(210-109*fit,140); ctx.lineTo(210+109*fit,140);
    ctx.moveTo(210,140-109*fit); ctx.lineTo(210,140+109*fit);
    ctx.stroke();
    ctx.globalAlpha = opacity * .45;
    ctx.beginPath();
    for (var i = 0; i < 48; i++) {
      var a = i * tau / 48, outer = (i % 4 ? 101 : 105)*fit;
      ctx.moveTo(210 + 98*fit*Math.cos(a), 140 + 98*fit*Math.sin(a));
      ctx.lineTo(210 + outer*Math.cos(a), 140 + outer*Math.sin(a));
    }
    ctx.stroke();
    var beam = t * tau / 4 - Math.PI / 2;
    var beamRadius = mix(96*fit,176,ease((t-detectionStart)/.8));
    ctx.save();
    ctx.beginPath(); ctx.rect(50,40,320,200); ctx.clip();
    ctx.fillStyle = palette.accent;
    for (var j = 0; j < 14; j++) {
      ctx.globalAlpha = opacity * .095 * (1-j/14);
      ctx.beginPath(); ctx.moveTo(210,140);
      ctx.arc(210,140,beamRadius,beam-(j+1)*.045,beam-j*.045); ctx.closePath(); ctx.fill();
    }
    ctx.globalAlpha = opacity * .8;
    ctx.strokeStyle = palette.accent;
    ctx.lineWidth = markScale;
    ctx.beginPath(); ctx.moveTo(210,140); ctx.lineTo(210 + beamRadius*Math.cos(beam),140 + beamRadius*Math.sin(beam)); ctx.stroke();
    ctx.restore();
    ctx.globalAlpha = opacity * .65;
    ctx.fillStyle = palette.accent;
    ctx.beginPath(); ctx.arc(210,140,2,0,tau); ctx.fill();
  }
  function players(ctx, opacity, t) {
    if (opacity <= .001) return;
    var beam = t * tau / 4 - Math.PI / 2;
    for (var team = 0; team < 2; team++) {
      for (var i = 0; i < formation.length; i++) {
        var f = formation[i], phase = i * 1.7 + team * 2.1;
        var x = 62 + (team ? 1-f[0] : f[0]) * 296 + Math.sin(t*.9+phase) * (i ? 5 : 1.5);
        var y = 55 + f[1] * 170 + Math.cos(t*.7+phase) * (i ? 6 : 2);
        var angle = Math.atan2(y-140,x-210);
        var elapsed = ((beam-angle) % tau + tau) % tau;
        var pulse = Math.exp(-elapsed*3.3);
        var color = palette.teams[team];
        ctx.strokeStyle = color;
        ctx.fillStyle = color;
        ctx.globalAlpha = opacity * pulse * .32;
        ctx.lineWidth = .7;
        ctx.beginPath(); ctx.arc(x,y,4+(1-pulse)*7,0,tau); ctx.stroke();
        ctx.globalAlpha = opacity * (.65+.35*pulse);
        ctx.beginPath(); ctx.arc(x,y,2.4+pulse*.65,0,tau);
        ctx.fill();
        ctx.strokeStyle = palette.stripes[team] || palette.edges[team] || color;
        ctx.lineWidth = palette.stripes[team] ? 1.2 : .8;
        ctx.stroke();
      }
    }
  }
  function draw(instance, t) {
    var ctx = instance.ctx;
    ctx.setTransform(instance.scale,0,0,instance.scale,0,instance.offset*instance.scale);
    ctx.clearRect(0,-instance.offset,420,instance.height);
    var leave = ease((t-takeoffStart)/takeoffDuration);
    var gather = ease((t-radarStart)/radarDuration);
    var returnPitch = ease((t-returnStart)/returnDuration);
    var assemble = ease((t-riseStart)/riseDuration);
    var radarWeight = gather * (1-returnPitch);
    var flockWeight = leave * (1-gather);
    var pitchWeight = 1-flockWeight-radarWeight;
    // The camera finishes rotating before takeoff so particles leave stable body points.
    var camera = t < flockStart ? cameraAt(t,instance) : null;
    var board = (1-leave)*.56 + ease((t-detectionStart)/1.2)*(.25+.31*returnPitch);
    if (camera) raisedBoard(ctx,1-leave,instance.markScale,camera);
    pitch(ctx,board,instance.markScale,camera);
    for (var actorIndex = 0; actorIndex < actors.length; actorIndex++)
      instance.lifts[actorIndex] = ease((t-riseStart-actors[actorIndex].delay)/riseDuration);
    if (camera) risingPlayers(ctx,1-leave,instance.markScale,camera,instance.lifts,t);
    var radarFit = Math.min(1,(instance.height-8)/218);
    radar(ctx,radarWeight,t,instance.markScale,radarFit);
    // Start the same flock motion relative to the figures' takeoff.
    var flockTime = t-takeoffStart+4.5;
    var yaw = .18*Math.sin(flockTime*.65), cos = Math.cos(yaw), sin = Math.sin(yaw);
    var particles = instance.particles;
    for (var i = 0; i < particles.length; i++) {
      var p = particles[i];
      var phi = p.angle+flockTime*.48;
      var width = (8+34*Math.sqrt(1-p.u*p.u))*p.radius;
      var fx = p.u*(instance.mobile ? 182 : 133) + Math.cos(phi)*width*.65;
      var fy = Math.sin(p.u*3.2+flockTime*.92)*22 + Math.sin(p.u*5.2-flockTime*.4)*10 + Math.sin(phi)*width;
      var depth = Math.cos(phi)*width;
      var flockX = 210 + fx*cos-fy*sin + depth*.24;
      var sky = instance.mobile ? instance.height*.38-instance.offset : 82;
      var spread = instance.mobile ? instance.height/170 : .72;
      var flockY = sky + (fx*sin+fy*cos)*spread + Math.sin(flockTime*.7)*5;
      var ringAngle = p.ringAngle + .08*Math.sin(t*.8);
      var originX = p.x, originY = p.y;
      if (camera) {
        var actor = actors[p.actor];
        project(camera,mix(p.x,actor.x+p.bodyX,assemble),mix(p.y,actor.y+p.bodyY,assemble),p.bodyZ*instance.lifts[p.actor]);
        originX = camera.px; originY = camera.py;
      }
      var flight = ease((t-p.release)/(flockStart-p.release));
      var x, y;
      if (t < flockStart) {
        // A cubic path starts upward, then opens into the moving sky cloud.
        // Ease at both ends keeps detachment and arrival continuous.
        var remaining = 1-flight;
        var a = remaining*remaining*remaining, b = 3*remaining*remaining*flight;
        var c = 3*remaining*flight*flight, d = flight*flight*flight;
        var curl = Math.sin(p.angle)*6;
        x = a*originX + b*(originX+curl) + c*(flockX-curl) + d*flockX;
        var up = Math.max(8-instance.offset,originY-48);
        y = a*originY + b*up + c*(flockY+28) + d*flockY;
      } else {
        x = pitchWeight*originX + flockWeight*flockX + radarWeight*(210+p.ring*radarFit*Math.cos(ringAngle));
        y = pitchWeight*originY + flockWeight*flockY + radarWeight*(140+p.ring*radarFit*Math.sin(ringAngle));
      }
      // Keep each particle's color identity through the silhouette/flock morph.
      var team = actors[p.actor].team;
      var stripe = p.stripe && palette.stripes[team];
      ctx.fillStyle = stripe || palette.teams[team];
      ctx.globalAlpha = (mix(.7,.9,p.depth)*flockWeight + .8*pitchWeight + .22*radarWeight);
      var size = (.75 + flockWeight*(.3+p.depth*.35)) * instance.markScale;
      ctx.beginPath(); ctx.ellipse(x,y,size*(1+flockWeight*.35),size*.6,-.25+flockWeight*Math.sin(phi)*.3,0,tau); ctx.fill();
      var edge = stripe ? palette.particleEdges[team] : palette.light ? palette.edges[team] : null;
      if (edge) {
        ctx.strokeStyle = edge;
        ctx.lineWidth = .65*instance.markScale;
        ctx.stroke();
      }
    }
    players(ctx,ease((t-detectionStart)/.9)*(1-returnPitch),t);
    ctx.globalAlpha = 1;
  }
  function visible(instance) {
    if (!instance.node.isConnected || document.hidden || reduced.matches || !instance.inView || instance.lost) return false;
    var rect = instance.node.getBoundingClientRect();
    if (rect.width <= 0 || rect.height <= 0) return false;
    var style = window.getComputedStyle(instance.node);
    return style.visibility !== 'hidden' && style.visibility !== 'collapse';
  }
  function size(instance) {
    var rect = instance.node.getBoundingClientRect();
    if (!rect.width || !rect.height) return;
    instance.mobile = window.getComputedStyle(instance.node).getPropertyValue('--fm-loading-mobile').trim() === '1';
    // A tall mobile scene has extra sky, not vertically stretched figures.
    instance.markScale = Math.max(1,Math.min(2,420/rect.width));
    var dpr = Math.min(window.devicePixelRatio || 1,1.5);
    var pixels = Math.max(1,Math.round(rect.width*dpr));
    var height = Math.max(1,Math.round(rect.height*dpr));
    instance.scale = pixels/420;
    instance.height = height/instance.scale;
    instance.offset = (instance.height-280)/2;
    if (instance.canvas.width !== pixels || instance.canvas.height !== height) {
      instance.canvas.width = pixels;
      instance.canvas.height = height;
    }
    var count = instance.mobile || window.innerWidth < 480 || instance.node.classList.contains('fm-loading-visual--compact') ? 180 : 320;
    // Resizing a toolbar/viewport does not recreate the bounded particle pool.
    if (instance.count !== count) {
      instance.count = count;
      instance.particles = makeParticles(count);
      instance.lifts = new Float32Array(actors.length);
    }
  }
  function tick(now) {
    frame = 0;
    if (document.hidden || reduced.matches) return;
    if (epoch === null) epoch = now;
    if (now-lastFrame >= 1000/30-.25) {
      lastFrame = now;
      instances.forEach(function (instance) {
        if (!instance.active) return;
        try {
          draw(instance,((now-epoch)/1000)%loopDuration);
          if (!instance.ready) {
            instance.ready = true;
            instance.node.setAttribute('data-fm-drawing','true');
          }
        } catch (error) {
          // A lost/unavailable graphics context must leave a useful static mark.
          instance.onLost();
        }
      });
    }
    if (Array.from(instances.values()).some(function (instance) { return instance.active; })) frame = window.requestAnimationFrame(tick);
  }
  function sync() {
    pendingSync = false;
    instances.forEach(function (instance,node) {
      if (!node.isConnected) {
        if (intersection) intersection.unobserve(node);
        if (resize) resize.unobserve(node);
        instance.canvas.removeEventListener('contextlost',instance.onLost);
        instance.canvas.removeEventListener('contextrestored',instance.onRestored);
        instances.delete(node);
        return;
      }
      instance.active = visible(instance);
      if (instance.active) {
        size(instance);
      } else if (reduced.matches) {
        instance.ready = false;
        node.removeAttribute('data-fm-drawing');
      }
    });
    var active = Array.from(instances.values()).some(function (instance) { return instance.active; });
    if (active && !frame) frame = window.requestAnimationFrame(tick);
    if (!active && frame) { window.cancelAnimationFrame(frame); frame = 0; }
  }
  function scheduleSync() {
    if (pendingSync) return;
    pendingSync = true;
    // Microtasks coalesce DOM changes without leaving an idle polling timer.
    Promise.resolve().then(sync);
  }
  var intersection = typeof IntersectionObserver === 'function' ? new IntersectionObserver(function (entries) {
    entries.forEach(function (entry) { var instance = instances.get(entry.target); if (instance) instance.inView = entry.isIntersecting; });
    scheduleSync();
  }) : null;
  var resize = typeof ResizeObserver === 'function' ? new ResizeObserver(function (entries) {
    entries.forEach(function (entry) { var instance = instances.get(entry.target); if (instance) size(instance); });
    scheduleSync();
  }) : null;
  function mount(node) {
    if (instances.has(node)) return;
    node.setAttribute('data-fm-teams',teamIds);
    // DOM clones copy readiness attributes, but never the canvas bitmap.
    node.removeAttribute('data-fm-drawing');
    var canvas = node.querySelector('.fm-loading-canvas');
    var ctx;
    try { ctx = canvas && canvas.getContext('2d'); } catch (error) { return; }
    if (!ctx) return;
    var instance = { node: node, canvas: canvas, ctx: ctx, inView: !intersection, active: false, count: 0, ready: false, lost: false };
    instance.onLost = function () {
      instance.lost = true;
      instance.active = false;
      instance.ready = false;
      node.removeAttribute('data-fm-drawing');
      scheduleSync();
    };
    instance.onRestored = function () { instance.lost = false; scheduleSync(); };
    canvas.addEventListener('contextlost',instance.onLost);
    canvas.addEventListener('contextrestored',instance.onRestored);
    instances.set(node,instance);
    if (intersection) intersection.observe(node);
    if (resize) resize.observe(node);
    size(instance);
  }
  function discover(node, deep) {
    if (node.nodeType !== 1) return false;
    var count = instances.size;
    if (node.matches(selector)) mount(node);
    if (deep && node.childElementCount) node.querySelectorAll(selector).forEach(mount);
    return instances.size !== count;
  }
  function start() {
    readPalette();
    discover(document.body,true);
    var observer = new MutationObserver(function (records) {
      var changed = false;
      records.forEach(function (record) {
        if (record.type === 'childList') {
          // Nested app tables are not loader hosts. Only known mount locations
          // need subtree discovery; a directly inserted loader works anywhere.
          var deep = record.target === document.body || (record.target.nodeType === 1 &&
            record.target.closest('#startup_status, [data-fm-loading-host]'));
          record.addedNodes.forEach(function (node) { if (discover(node,deep)) changed = true; });
          record.removedNodes.forEach(function (node) {
            if (node.nodeType !== 1) return;
            instances.forEach(function (instance) {
              if (node === instance.node || node.contains(instance.node)) changed = true;
            });
          });
        } else if (record.target === root && record.attributeName === 'data-fm-theme') {
          readPalette(); changed = true;
        } else if (record.type === 'attributes') {
          instances.forEach(function (instance) {
            if (record.target === instance.node || record.target.contains(instance.node)) changed = true;
          });
        }
      });
      if (changed) scheduleSync();
    });
    observer.observe(document.body,{childList:true,subtree:true,attributes:true,attributeFilter:['style','class','hidden']});
    observer.observe(root,{attributes:true,attributeFilter:['data-fm-theme','style','class','hidden']});
    document.addEventListener('visibilitychange',scheduleSync);
    window.addEventListener('resize',scheduleSync);
    if (reduced.addEventListener) reduced.addEventListener('change',scheduleSync);
    else reduced.addListener(scheduleSync);
    window.addEventListener('pagehide',function () {
      if (frame) window.cancelAnimationFrame(frame);
      frame = 0;
    });
    window.addEventListener('pageshow',scheduleSync);
    scheduleSync();
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded',start,{once:true});
  else start();
})();
