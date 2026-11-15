/* Show disclosures only when their contents have settled and are nonempty. */
(function () {
  'use strict';
  const pendingOpen = new WeakMap();
  const settledOutputs = new WeakSet();
  const nameTooltips = new WeakMap();
  const selector = '.shiny-output-error, .shiny-notification-warning .shiny-notification-content-text, .shiny-notification-error .shiny-notification-content-text';
  const outputSelector = '.shiny-text-output, .shiny-html-output';
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
  function refresh(panel) {
    if (!panel || !panel.matches('.fm-info')) return;
    const body = panel.querySelector(':scope > .fm-info-body');
    const outputs = body ? Array.from(body.querySelectorAll(outputSelector)) : [];
    const waiting = panel.closest('.recalculating,[aria-busy=true]') ||
      outputs.some(node => node.classList.contains('recalculating') ||
        (!settledOutputs.has(node) && !meaningful(node)));
    const empty = Boolean(waiting) || !meaningful(body);
    if (panel.hidden !== empty) panel.hidden = empty;
    if (panel.classList.contains('fm-info-empty') !== empty) panel.classList.toggle('fm-info-empty', empty);
  }
  function compact(node) {
    if (!node.matches(selector)) return;
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
    if (node.nodeType !== 1) return;
    compact(node);
    node.querySelectorAll(selector).forEach(compact);
    if (node.matches('.fm-info')) refresh(node);
    node.querySelectorAll('.fm-info').forEach(refresh);
    scanNames(node);
  }
  function start() {
    scan(document.body);
    if (window.jQuery) {
      window.jQuery(document).on('shiny:value.fmInfo shiny:error.fmInfo', function (event) {
        const target = event.target.nodeType === 1 ? event.target : document.getElementById(event.name);
        if (!target || !target.matches(outputSelector)) return;
        settledOutputs.add(target);
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
          else if (target.matches('.shiny-output-error')) compact(target);
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
      panels.forEach(refresh);
    });
    observer.observe(document.body, { childList: true, characterData: true, subtree: true, attributes: true, attributeFilter: ['class','aria-busy'] });
  }
  document.addEventListener('click', function (event) {
    if (event.target.closest('.fm-info > summary')) event.stopPropagation();
  }, true);
  document.addEventListener('toggle', function (event) {
    if (!event.target.matches('.fm-info')) return;
    if (window.jQuery) window.jQuery(event.target).trigger(event.target.open ? 'shown' : 'hidden');
  }, true);
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start, { once: true });
  else start();
}());
