(function () {
  'use strict';
  var selector = '[data-fm-loading]';
  var instances = new Map();
  var reduced = window.matchMedia('(prefers-reduced-motion: reduce)');
  var frame = 0;
  var epoch = null;
  var lastFrame = -Infinity;
  var tau = Math.PI * 2;
  var root = document.documentElement;
  var palette;
  var pendingSync = false;
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
        team: team, delay: (index % 4)*.12 });
    });
  }
  // A few samples of the head, shoulders, arms and legs reuse the pitch particles.
  var silhouette = [[0,30],[-2.7,27],[0,24],[2.7,27],[-4,21],[4,21],
    [-6,14],[6,14],[-2,13],[2,13],[-2,7],[2,7],[-3,0],[3,0]];
  function clamp(x) { return Math.max(0, Math.min(1, x)); }
  function ease(x) { x = clamp(x); return x * x * x * (x * (x * 6 - 15) + 10); }
  function mix(a, b, w) { return a + (b - a) * w; }
  function cameraAt(t) {
    var turn = ease((t-1.2)/2);
    var yaw = turn*(-.32 + .54*ease((t-2.6)/3));
    var tilt = turn*.96;
    return { cy: Math.cos(yaw), sy: Math.sin(yaw), ct: Math.cos(tilt),
      st: Math.sin(tilt), zoom: 1-.08*turn, lift: 9*turn, turn: turn };
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
      var body = silhouette[Math.floor(Math.floor(i/actors.length)/samples*silhouette.length)];
      result.push({ x: mix(segment.a[0], segment.b[0], f), y: mix(segment.a[1], segment.b[1], f),
        actor: actor, bodyX: body[0], bodyZ: body[1],
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
  function risingPlayers(ctx, opacity, markScale, camera, lifts) {
    if (opacity <= .001) return;
    ctx.lineCap = 'round';
    for (var i = 0; i < actors.length; i++) {
      var actor = actors[i], rise = lifts[i];
      if (rise <= .001) continue;
      var color = actor.team ? palette.accent : palette.ink;
      project(camera,actor.x,actor.y,0);
      var footX = camera.px, footY = camera.py;
      ctx.fillStyle = color;
      ctx.globalAlpha = opacity*rise*.14;
      ctx.beginPath(); ctx.ellipse(footX,footY,5*camera.k,2*camera.k,0,0,tau); ctx.fill();
      ctx.strokeStyle = color;
      ctx.lineWidth = 1.25*markScale*camera.k;
      ctx.globalAlpha = opacity*rise*.8;
      ctx.beginPath();
      project(camera,actor.x,actor.y,24*rise); ctx.moveTo(camera.px,camera.py);
      project(camera,actor.x,actor.y,12*rise); ctx.lineTo(camera.px,camera.py);
      project(camera,actor.x-3,actor.y,0); ctx.lineTo(camera.px,camera.py);
      project(camera,actor.x,actor.y,12*rise); ctx.moveTo(camera.px,camera.py);
      project(camera,actor.x+3,actor.y,0); ctx.lineTo(camera.px,camera.py);
      project(camera,actor.x-6,actor.y,14*rise); ctx.moveTo(camera.px,camera.py);
      project(camera,actor.x-3,actor.y,21*rise); ctx.lineTo(camera.px,camera.py);
      project(camera,actor.x+3,actor.y,21*rise); ctx.lineTo(camera.px,camera.py);
      project(camera,actor.x+6,actor.y,14*rise); ctx.lineTo(camera.px,camera.py);
      ctx.stroke();
      project(camera,actor.x,actor.y,27*rise);
      ctx.beginPath(); ctx.arc(camera.px,camera.py,2.6*camera.k*Math.sqrt(markScale)*rise,0,tau); ctx.fill();
    }
    ctx.lineCap = 'butt';
  }
  function radar(ctx, opacity, t, markScale) {
    if (opacity <= .001) return;
    ctx.strokeStyle = palette.ink;
    ctx.lineWidth = .6 * markScale;
    ctx.globalAlpha = opacity * .24;
    ctx.beginPath();
    [32,64,96].forEach(function (r) { ctx.moveTo(210+r,140); ctx.arc(210,140,r,0,tau); });
    ctx.moveTo(101,140); ctx.lineTo(319,140); ctx.moveTo(210,31); ctx.lineTo(210,249);
    ctx.stroke();
    ctx.globalAlpha = opacity * .45;
    ctx.beginPath();
    for (var i = 0; i < 48; i++) {
      var a = i * tau / 48, outer = i % 4 ? 101 : 105;
      ctx.moveTo(210 + 98*Math.cos(a), 140 + 98*Math.sin(a));
      ctx.lineTo(210 + outer*Math.cos(a), 140 + outer*Math.sin(a));
    }
    ctx.stroke();
    var beam = t * tau / 4 - Math.PI / 2;
    var beamRadius = mix(96,176,ease((t-10)/.8));
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
        var color = team ? palette.accent : palette.ink;
        ctx.strokeStyle = color;
        ctx.fillStyle = color;
        ctx.globalAlpha = opacity * pulse * .32;
        ctx.lineWidth = .7;
        ctx.beginPath(); ctx.arc(x,y,4+(1-pulse)*7,0,tau); ctx.stroke();
        ctx.globalAlpha = opacity * (.65+.35*pulse);
        ctx.beginPath(); ctx.arc(x,y,2.4+pulse*.65,0,tau);
        if (team) { ctx.lineWidth = 1.4; ctx.stroke(); } else ctx.fill();
      }
    }
  }
  function draw(instance, t) {
    var ctx = instance.ctx;
    ctx.setTransform(instance.scale,0,0,instance.scale,0,0);
    ctx.clearRect(0,0,420,280);
    var leave = ease((t-4.5)/1.7), gather = ease((t-8)/2), returnPitch = ease((t-13)/2);
    var assemble = ease((t-2.2)/1.3);
    var radarWeight = gather * (1-returnPitch);
    var flockWeight = leave * (1-gather);
    var pitchWeight = 1-flockWeight-radarWeight;
    var camera = t < 6.2 ? cameraAt(t) : null;
    var board = (1-leave)*.56 + ease((t-10)/1.2)*(.25+.31*returnPitch);
    if (camera) raisedBoard(ctx,1-leave,instance.markScale,camera);
    pitch(ctx,board,instance.markScale,camera);
    for (var actorIndex = 0; actorIndex < actors.length; actorIndex++)
      instance.lifts[actorIndex] = ease((t-2.2-actors[actorIndex].delay)/1.4);
    if (camera) risingPlayers(ctx,1-leave,instance.markScale,camera,instance.lifts);
    radar(ctx,radarWeight,t,instance.markScale);
    var yaw = .18*Math.sin(t*.65), cos = Math.cos(yaw), sin = Math.sin(yaw);
    var particles = instance.particles;
    for (var i = 0; i < particles.length; i++) {
      var p = particles[i];
      var phi = p.angle+t*.48;
      var width = (8+34*Math.sqrt(1-p.u*p.u))*p.radius;
      var fx = p.u*133 + Math.cos(phi)*width*.65;
      var fy = Math.sin(p.u*3.2+t*.92)*22 + Math.sin(p.u*5.2-t*.4)*10 + Math.sin(phi)*width;
      var depth = Math.cos(phi)*width;
      var flockX = 210 + fx*cos-fy*sin + depth*.24;
      var flockY = 140 + fx*sin+fy*cos + Math.sin(t*.7)*8;
      var ringAngle = p.ringAngle + .08*Math.sin(t*.8);
      var originX = p.x, originY = p.y;
      if (camera) {
        var actor = actors[p.actor];
        project(camera,mix(p.x,actor.x+p.bodyX,assemble),mix(p.y,actor.y,assemble),p.bodyZ*instance.lifts[p.actor]);
        originX = camera.px; originY = camera.py;
      }
      var x = pitchWeight*originX + flockWeight*flockX + radarWeight*(210+p.ring*Math.cos(ringAngle));
      var y = pitchWeight*originY + flockWeight*flockY + radarWeight*(140+p.ring*Math.sin(ringAngle));
      // Keep each particle's color identity through the silhouette/flock morph.
      ctx.fillStyle = actors[p.actor].team ? palette.accent : palette.ink;
      ctx.globalAlpha = (mix(.7,.9,p.depth)*flockWeight + .8*pitchWeight + .22*radarWeight);
      var size = (.75 + flockWeight*(.3+p.depth*.35)) * instance.markScale;
      ctx.beginPath(); ctx.ellipse(x,y,size*(1+flockWeight*.35),size*.6,-.25+flockWeight*Math.sin(phi)*.3,0,tau); ctx.fill();
    }
    players(ctx,ease((t-10)/.9)*(1-returnPitch),t);
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
    var width = instance.node.getBoundingClientRect().width;
    if (!width) return;
    // Preserve readable dots and fine lines when the same scene is compact.
    instance.markScale = Math.max(1,Math.min(2,420/width));
    var dpr = Math.min(window.devicePixelRatio || 1,1.5);
    var pixels = Math.max(1,Math.round(width*dpr));
    var count = window.innerWidth < 480 || instance.node.classList.contains('fm-loading-visual--compact') ? 180 : 320;
    if (instance.canvas.width !== pixels || instance.count !== count) {
      instance.canvas.width = pixels;
      instance.canvas.height = Math.round(pixels*2/3);
      instance.scale = pixels/420;
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
          draw(instance,((now-epoch)/1000)%15);
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
