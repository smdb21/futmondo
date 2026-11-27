/* Browser visibility/timezone and a clock that sends no per-second Shiny traffic. */
(function () {
  'use strict';
  function publish() {
    if (!window.Shiny || !Shiny.setInputValue) return;
    Shiny.setInputValue('worker_visible', !document.hidden);
    try { Shiny.setInputValue('worker_timezone_detected', Intl.DateTimeFormat().resolvedOptions().timeZone || 'Europe/Madrid'); }
    catch (_) { Shiny.setInputValue('worker_timezone_detected', 'Europe/Madrid'); }
  }
  function clock() {
    if (document.hidden) return;
    document.querySelectorAll('.fm-browser-countdown').forEach(function (node) {
      if (!node.dataset.offset) node.dataset.offset = String(Number(node.dataset.serverNow) * 1000 - Date.now());
      if (!node.getClientRects().length) return;
      var seconds = Math.max(0, Math.floor(Number(node.dataset.deadline) - (Date.now() + Number(node.dataset.offset)) / 1000));
      if (!Number.isFinite(seconds)) return;
      var days = Math.floor(seconds / 86400), hours = Math.floor(seconds / 3600) % 24;
      var mins = Math.floor(seconds / 60) % 60, secs = seconds % 60;
      var label = (days ? days + 'd ' : '') + hours + 'h ' + String(mins).padStart(2, '0') + 'm ' + String(secs).padStart(2, '0') + 's';
      if (node.textContent !== label) node.textContent = label;
    });
  }
  document.addEventListener('toggle', function (event) {
    if (event.target.id === 'worker_settings_details' && window.Shiny && Shiny.setInputValue)
      Shiny.setInputValue('worker_settings_open', event.target.open);
  }, true);
  document.addEventListener('visibilitychange', function () { publish(); clock(); });
  document.addEventListener('shiny:connected', publish);
  if (window.jQuery) jQuery(document).on('shiny:connected', publish);
  setInterval(clock, 1000);
}());
