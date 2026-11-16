/* Show disclosures only when their contents have settled and are nonempty. */
(function () {
  'use strict';
  const pendingOpen = new WeakMap();
  const settledOutputs = new WeakSet();
  const nameTooltips = new WeakMap();
  // DOM-only presentation registry. Source elements keep their existing Shiny
  // bindings; an origin marker lets us restore them BEFORE Shiny unbinds/replaces
  // their owning output. No HTML copies, new inputs, polling or acquisition.
  const entries = new Map();
  const scopes = new Map();
  let registryFrame = null;
  const scopeSelector = '.fm-information-scope, .modal-content';
  function setClass(node, name, value) {
    if (node.classList.contains(name) !== value) node.classList.toggle(name, value);
  }
  function activePage() {
    for (const scope of scopes.keys()) {
      if (!scope.matches('.fm-information-scope')) continue;
      const tab = scope.closest('.tab-pane[id^="shiny-tab-"]');
      if (!tab || tab.classList.contains('active')) return scope;
    }
    return null;
  }
  function ensureScope(root) {
    if (scopes.has(root)) return scopes.get(root);
    let panel = root.querySelector('.fm-info-panel');
    // A nested player view is part of its enclosing dialog, not another scope.
    if (panel && panel.closest(scopeSelector) !== root) panel = null;
    if (!panel) {
      let heading = root.querySelector(':scope > .modal-header');
      if (!heading) {
        heading = document.createElement('div');
        heading.className = 'fm-dialog-information-heading';
        const title = document.createElement('h2');
        title.textContent = root.querySelector('.player-card-close-shortcut') ? 'Player details' : 'Details';
        heading.appendChild(title);
        const body = root.querySelector(':scope > .modal-body') || root;
        body.prepend(heading);
      }
      heading.classList.add('fm-information-heading');
      panel = document.createElement('details');
      panel.className = 'fm-info fm-info-panel';
      panel.hidden = true;
      panel.innerHTML = '<summary aria-label="Dialog information" title="Dialog information"><span class="fm-info-icon" aria-hidden="true">i</span></summary><div class="fm-info-body"></div>';
      heading.appendChild(panel);
    }
    scopes.set(root, panel);
    return panel;
  }
  function sourceLabel(node, origin) {
    if (node.hasAttribute('data-fm-info-source')) return node.dataset.fmInfoSource;
    const owner = origin.closest('[id].shiny-html-output, [id].shiny-text-output, [id].shiny-bound-output');
    const id = owner ? owner.id : '';
    const labels = [
      [/round_finance/, 'Balance and active offers'], [/round_countdown/, 'Round schedule'],
      [/worker|background_sync/, 'Background sync'], [/coach/, 'Coach'],
      [/startup/, 'Startup'], [/timezone/, 'Timezone'], [/league_preference/, 'League preference'],
      [/page_preference/, 'Page preference'], [/remember/, 'Remembered login'],
      [/transfer_coverage|recent_transfers/, 'League transfers'], [/incoming/, 'Incoming signings'],
      [/outgoing/, 'Departing players'], [/received_offer/, 'Received offers'],
      [/market_prices|market_status/, 'Market data'], [/history|round_points/, 'Player history'],
      [/chart|plot|standings_status|values_status|cash_status|rank_status|owned_club_status/, 'Chart details'],
      [/classification|lineup|departure/, 'Lineup and round data'], [/catalog/, 'Player catalog'],
      [/notifications.*status/, 'Official events'], [/login/, 'Login'], [/clause/, 'Scheduled purchases']
    ];
    for (const [pattern, label] of labels) if (pattern.test(id)) return label;
    const section = origin.closest('.box, section, .fm-incoming-panel');
    const title = section && section.querySelector('.box-title,h2,h3,h4');
    return title && title.textContent.trim() ? title.textContent.trim() : 'Page details';
  }
  function register(node) {
    if (!node.isConnected || entries.has(node) || node.matches('.fm-info-panel,.fm-info-inline,.fm-info-restoring') ||
        node.closest('.shiny-notification') || node.parentElement.closest('.fm-info-entry')) return;
    const home = node.closest(scopeSelector);
    const global = !home && node.closest('.content,[data-fm-info-global]');
    if (!home && !global) return; // Standalone/embedded fixtures retain native disclosures.
    if (home) ensureScope(home);
    const origin = document.createElement('span');
    origin.className = 'fm-info-origin';
    origin.setAttribute('aria-hidden', 'true');
    node.before(origin);
    const label = document.createElement('div');
    label.className = 'fm-info-source-label';
    label.textContent = sourceLabel(node, origin);
    if (label.textContent) node.insertBefore(label, node.querySelector('.fm-info-body'));
    const record = {node, origin, home, label, wasOpen: node.open};
    entries.set(node, record);
    node.classList.add('fm-info-entry');
    node.open = true;
    scheduleRegistry();
  }
  function restoreWithin(target) {
    if (!target || target.nodeType !== 1) return;
    for (const [node, entry] of entries) {
      if (!target.contains(entry.origin) && !target.contains(node)) continue;
      if (entry.origin.isConnected) entry.origin.replaceWith(node);
      entry.label.remove();
      node.classList.remove('fm-info-entry', 'fm-info-attached');
      node.classList.add('fm-info-restoring');
      node.open = entry.wasOpen;
      entries.delete(node);
    }
    scheduleRegistry();
  }
  function scheduleRegistry() {
    if (registryFrame !== null) return;
    registryFrame = requestAnimationFrame(reconcileRegistry);
  }
  function reconcileRegistry() {
    registryFrame = null;
    for (const [root] of scopes) if (!root.isConnected) scopes.delete(root);
    const page = activePage();
    const grouped = new Map(Array.from(scopes, ([root]) => [root, []]));
    for (const [node, entry] of entries) {
      if (!entry.origin.isConnected) {
        // Raw subtree removal (outside renderUI) must also release moved inputs.
        if (node.isConnected && window.Shiny) window.Shiny.unbindAll(node);
        node.remove(); entries.delete(node); continue;
      }
      if (!node.isConnected) {
        entry.origin.replaceWith(node);
        entries.delete(node);
        entry.label.remove();
        node.classList.remove('fm-info-entry', 'fm-info-attached');
        node.open = entry.wasOpen;
        register(node);
        continue;
      }
      const home = entry.home && entry.home.isConnected ? entry.home : page;
      if (!home || !grouped.has(home)) continue;
      refresh(node, entry.origin);
      // renderUI may still be binding freshly inserted descendants. Keep them
      // in their original binding scope until Shiny has bound every live child.
      const unbound = node.querySelector('.action-button:not(.shiny-bound-input), .shiny-text-output:not(.shiny-bound-output), .shiny-html-output:not(.shiny-bound-output)');
      if (!unbound) grouped.get(home).push(entry);
    }
    for (const [root, list] of grouped) {
      const panel = scopes.get(root);
      const body = panel.querySelector(':scope > .fm-info-body');
      list.sort((a,b) => {
        if (Boolean(a.home) !== Boolean(b.home)) return a.home ? 1 : -1;
        return a.origin.compareDocumentPosition(b.origin) & Node.DOCUMENT_POSITION_FOLLOWING ? -1 : 1;
      });
      let position = body.firstElementChild;
      for (const entry of list) {
        if (entry.node !== position) body.insertBefore(entry.node, position);
        setClass(entry.node, 'fm-info-attached', true);
        position = entry.node.nextElementSibling;
      }
      const ready = list.filter(entry => !entry.node.hidden);
      const failed = ready.some(entry => entry.node.classList.contains('fm-info-error') ||
        entry.node.querySelector('.shiny-output-error:not(.shiny-output-error-fm-loading)'));
      setClass(panel, 'fm-info-error', failed);
      if (panel.hidden !== !ready.length) panel.hidden = !ready.length;
      const summary = panel.querySelector(':scope > summary');
      const name = root.matches('.modal-content') ? 'Dialog information' : 'Page information';
      const accessible = failed ? name + ' (errors)' : name;
      if (summary.getAttribute('aria-label') !== accessible) summary.setAttribute('aria-label', accessible);
    }
  }
  const selector = '.shiny-output-error, .shiny-notification-warning .shiny-notification-content-text, .shiny-notification-error .shiny-notification-content-text';
  const outputSelector = '.shiny-text-output, .shiny-html-output, .shiny-bound-output';
  function updateNameTooltip(label) {
    const current = label.getAttribute('title') || '';
    const previous = nameTooltips.get(label);
    const base = previous && current === previous.applied ? previous.base : current;
    if (label.closest('.fm-departing-before-round')) {
      const applied = (base ? base + '. ' : '') + "Scheduled to leave by the next round's kickoff.";
      nameTooltips.set(label, {base, applied});
      if (current !== applied) label.setAttribute('title', applied);
    } else if (previous) {
      if (current === previous.applied) {
        if (base) label.setAttribute('title', base);
        else label.removeAttribute('title');
      }
      nameTooltips.delete(label);
    }
  }
  function scanNames(node) {
    if (node.matches('.fm-team-label')) updateNameTooltip(node);
    node.querySelectorAll('.fm-team-label').forEach(updateNameTooltip);
  }
  function meaningful(node) {
    return Boolean(node && (node.textContent.replace(/[\s\u00a0\u200b]/g, '') ||
      node.querySelector('img,svg,canvas,button,select,textarea,input:not([type=hidden])')));
  }
  function refresh(panel, origin) {
    if (!panel || !panel.matches('.fm-info') || panel.matches('.fm-info-panel,.fm-info-restoring')) return;
    const body = panel.querySelector(':scope > .fm-info-body');
    const outputs = body ? Array.from(body.querySelectorAll(outputSelector)) : [];
    const waiting = (origin || panel).closest('.recalculating,[aria-busy=true]') ||
      outputs.some(node => node.classList.contains('recalculating') ||
        (!settledOutputs.has(node) && !meaningful(node)));
    const empty = Boolean(waiting) || !meaningful(body);
    if (panel.hidden !== empty) panel.hidden = empty;
    if (panel.classList.contains('fm-info-empty') !== empty) panel.classList.toggle('fm-info-empty', empty);
  }
  function compact(node) {
    if (!node.matches(selector)) {
      // Shiny removes its error classes before publishing recovered content.
      // Do not retain a loading class that can suspend the recovered output.
      if (node.classList.contains('fm-info-waiting')) node.classList.remove('fm-info-waiting');
      return;
    }
    // Action-result notifications remain immediately readable in the scoped app.
    if (node.querySelector('.fm-offer-error') || (scopes.size && node.closest('.shiny-notification'))) {
      const notification = node.closest('.shiny-notification');
      if (notification && notification.classList.contains('fm-info-notification')) notification.classList.remove('fm-info-notification');
      return;
    }
    const loading = node.classList.contains('shiny-output-error-fm-loading');
    if (node.classList.contains('fm-info-waiting') !== loading) node.classList.toggle('fm-info-waiting', loading);
    if (loading || node.closest('.fm-info') || node.querySelector('.fm-info') || !node.textContent.trim()) return;
    const disclosure = document.createElement('details');
    disclosure.className = 'fm-info fm-info-error';
    const trigger = document.createElement('summary');
    trigger.setAttribute('aria-label', 'Error details');
    const icon = document.createElement('span');
    icon.className = 'fm-info-icon';
    icon.setAttribute('aria-hidden', 'true');
    icon.textContent = 'i';
    trigger.appendChild(icon);
    const body = document.createElement('div');
    body.className = 'fm-info-body';
    while (node.firstChild) body.appendChild(node.firstChild);
    disclosure.append(trigger, body);
    node.appendChild(disclosure);
    const notification = node.closest('.shiny-notification');
    if (notification) notification.classList.add('fm-info-notification');
    refresh(disclosure);
  }
  function scan(node) {
    if (node.nodeType !== 1 || !node.isConnected) return;
    compact(node);
    node.querySelectorAll(selector).forEach(compact);
    if (node.matches('.fm-information-scope')) ensureScope(node);
    node.querySelectorAll('.fm-information-scope').forEach(ensureScope);
    if (node.matches('.fm-info')) { refresh(node); register(node); }
    node.querySelectorAll('.fm-info').forEach(panel => { refresh(panel); register(panel); });
    scanNames(node);
  }
  function start() {
    scan(document.body);
    if (window.Shiny) window.Shiny.addCustomMessageHandler('fm-information-reset', function (message) {
      for (const [node, entry] of entries) {
        if (!entry.origin.closest(outputSelector) && !entry.home?.matches('.modal-content')) continue; // Page help is context independent.
        entry.origin.replaceWith(node);
        entry.label.remove();
        node.classList.remove('fm-info-entry', 'fm-info-attached');
        node.classList.add('fm-info-restoring');
        node.hidden = true;
        entries.delete(node);
      }
      for (const panel of scopes.values()) panel.open = false;
      scheduleRegistry();
    });
    if (window.jQuery) {
      window.jQuery(document).on('shiny:value.fmInfo shiny:error.fmInfo', function (event) {
        const target = event.target.nodeType === 1 ? event.target : document.getElementById(event.name);
        if (!target || !target.matches(outputSelector)) return;
        restoreWithin(target);
        settledOutputs.add(target);
        scheduleRegistry();
        const panel = target.closest('.fm-info');
        if (panel) requestAnimationFrame(() => refresh(panel));
        if (!target.matches('.shiny-html-output')) return;
        const indexes = [];
        target.querySelectorAll('.fm-info').forEach(function (node, index) { if (node.open) indexes.push(index); });
        if (indexes.length) pendingOpen.set(target, indexes);
        else pendingOpen.delete(target);
      });
    }
    const observer = new MutationObserver(function (records) {
      const panels = new Set();
      records.forEach(function (record) {
        const target = record.target.nodeType === 1 ? record.target : record.target.parentElement;
        if (target) {
          const panel = target.closest('.fm-info');
          if (panel) panels.add(panel);
        }
        if (record.type === 'attributes') {
          if (target.matches('.rt-tr')) scanNames(target);
          if (target.matches('.shiny-notification')) scan(target);
          else if (target.matches('.shiny-output-error, .fm-info-waiting')) compact(target);
          // Recalculation can change on a parent output without replacing nodes.
          if (target.matches(outputSelector) || target.hasAttribute('aria-busy')) target.querySelectorAll('.fm-info').forEach(node => panels.add(node));
          return;
        }
        if (target && record.type === 'childList') {
          compact(target);
          const state = pendingOpen.get(target);
          const disclosures = state && target.querySelectorAll('.fm-info');
          if (disclosures && disclosures.length) {
            state.forEach(function (index) { if (disclosures[index]) disclosures[index].open = true; });
            pendingOpen.delete(target);
          }
        }
        record.addedNodes.forEach(scan);
      });
      panels.forEach(panel => refresh(panel, entries.get(panel)?.origin));
      scheduleRegistry();
    });
    if (window.jQuery) {
      window.jQuery(document).on('shown.bs.tab.fmInfo shown.bs.modal.fmInfo hidden.bs.modal.fmInfo', scheduleRegistry);
      window.jQuery(document).on('shiny:bound.fmInfo', scheduleRegistry);
      window.jQuery(document).on('shiny:unbound.fmInfo', event => restoreWithin(event.target));
    }
    scheduleRegistry();
    observer.observe(document.body, { childList: true, characterData: true, subtree: true, attributes: true, attributeFilter: ['class','aria-busy'] });
  }
  document.addEventListener('click', function (event) {
    if (event.target.closest('.fm-info > summary')) event.stopPropagation();
  }, true);
  document.addEventListener('toggle', function (event) {
    if (!event.target.matches('.fm-info') || event.target.matches('.fm-info-entry')) return;
    if (window.jQuery) window.jQuery(event.target).trigger(event.target.open ? 'shown' : 'hidden');
  }, true);
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start, { once: true });
  else start();
}());
