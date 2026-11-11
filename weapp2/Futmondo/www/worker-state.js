/* Browser visibility/timezone and a clock that sends no per-second Shiny traffic. */
(function () {
  'use strict';
  function publish() {
    if (!window.Shiny || !Shiny.setInputValue) return;
    Shiny.setInputValue('worker_visible', !document.hidden);
    try { Shiny.setInputValue('worker_timezone_detected', Intl.DateTimeFormat().resolvedOptions().timeZone); }
    catch (_) { Shiny.setInputValue('worker_timezone_detected', 'UTC'); }
  }
  function clock() {
    if (document.hidden) return;
    document.querySelectorAll('.fm-browser-countdown').forEach(function (node) {
      if (!node.dataset.offset) node.dataset.offset = String(Number(node.dataset.serverNow) * 1000 - Date.now());
      var seconds = Math.max(0, Math.floor(Number(node.dataset.deadline) - (Date.now() + Number(node.dataset.offset)) / 1000));
      if (!Number.isFinite(seconds)) return;
      var days = Math.floor(seconds / 86400), hours = Math.floor(seconds / 3600) % 24;
      var mins = Math.floor(seconds / 60) % 60, secs = seconds % 60;
      node.textContent = (days ? days + 'd ' : '') + String(hours).padStart(2, '0') + ':' + String(mins).padStart(2, '0') + ':' + String(secs).padStart(2, '0');
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
