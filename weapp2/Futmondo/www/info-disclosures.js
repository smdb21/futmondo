/* Compact Shiny-generated errors; ordinary status content uses fm_info() in R. */
(function () {
  'use strict';
  const pendingOpen = new WeakMap();
  const selector = '.shiny-output-error, .shiny-notification-warning .shiny-notification-content-text, .shiny-notification-error .shiny-notification-content-text';
  function compact(node) {
    if (!node.matches(selector) || node.closest('.fm-info') || node.querySelector('.fm-info') || !node.textContent.trim()) return;
    const disclosure = document.createElement('details');
    const loading = node.classList.contains('shiny-output-error-fm-loading');
    disclosure.className = loading ? 'fm-info' : 'fm-info fm-info-error';
    const trigger = document.createElement('summary');
    trigger.setAttribute('aria-label', loading ? 'Information' : 'Error details');
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
  }
  function scan(node) {
    if (node.nodeType !== 1) return;
    compact(node);
    node.querySelectorAll(selector).forEach(compact);
  }
  function start() {
    scan(document.body);
    if (window.jQuery) window.jQuery(document).on('shiny:value.fmInfo', function (event) {
      const target = event.target;
      if (!target.matches('.shiny-html-output')) return;
      const indexes = [];
      target.querySelectorAll('.fm-info').forEach(function (panel, index) { if (panel.open) indexes.push(index); });
      if (indexes.length) pendingOpen.set(target, indexes);
      else pendingOpen.delete(target);
    });
    // Inspect only inserted subtrees/changed error containers, never rescan the
    // full document on a clock tick or an ordinary text-output update.
    const observer = new MutationObserver(function (records) {
      records.forEach(function (record) {
        // Shiny assigns notification severity after awaiting content rendering.
        // Watch that late class change without scanning unrelated UI subtrees.
        if (record.type === 'attributes') {
          if (record.target.matches('.shiny-notification, .shiny-output-error')) scan(record.target);
          return;
        }
        if (record.target.nodeType === 1) {
          compact(record.target);
          const state = pendingOpen.get(record.target);
          const panels = state && record.target.querySelectorAll('.fm-info');
          if (panels && panels.length) {
            state.forEach(function (index) { if (panels[index]) panels[index].open = true; });
            pendingOpen.delete(record.target);
          }
        }
        record.addedNodes.forEach(scan);
      });
    });
    observer.observe(document.body, { childList: true, subtree: true, attributes: true, attributeFilter: ['class'] });
  }
  document.addEventListener('click', function (event) {
    // Reactable row selection must not intercept a disclosure's native toggle.
    if (event.target.closest('.fm-info > summary')) event.stopPropagation();
  }, true);
  document.addEventListener('toggle', function (event) {
    if (!event.target.matches('.fm-info')) return;
    // Let Shiny resume outputs revealed by native details, without an input,
    // refresh, API request, or persistence write.
    if (window.jQuery) window.jQuery(event.target).trigger(event.target.open ? 'shown' : 'hidden');
  }, true);
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start, { once: true });
  else start();
}());
