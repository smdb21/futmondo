(function ($) {
  'use strict';

  $(function () {
    const dialog = document.getElementById('fm-disconnect');
    if (!dialog || typeof dialog.showModal !== 'function') return;

    let serverDialog;
    let shinyDisconnected = false;

    function syncDialog() {
      // Shiny Server uses a logical socket. Its terminal UI also covers a
      // failed reconnect that did not emit a separate Shiny disconnect event.
      const disconnected = serverDialog
        ? serverDialog.style.display !== 'none' && serverDialog.querySelector('#ss-reload-link')
        : shinyDisconnected && !document.getElementById('shiny-reconnect-text');

      if (disconnected && !dialog.open) {
        dialog.showModal();
        document.body.classList.add('fm-disconnect-open');
      } else if (!disconnected && dialog.open) {
        dialog.close();
        document.body.classList.remove('fm-disconnect-open');
      }
    }

    // Watch only the upstream message, including its show/hide transitions.
    const serverObserver = new MutationObserver(syncDialog);
    function bindServerDialog() {
      if (serverDialog) return;
      serverDialog = document.getElementById('ss-connect-dialog');
      if (serverDialog) {
        serverObserver.observe(serverDialog, { childList: true, attributes: true, attributeFilter: ['style'] });
        syncDialog();
      }
    }

    // The server client inserts its message holder in its own ready callback.
    window.setTimeout(bindServerDialog, 0);
    $(document).on('shiny:connected.fmDisconnect', function () {
      shinyDisconnected = false;
      bindServerDialog();
      syncDialog();
    }).on('shiny:disconnected.fmDisconnect', function () {
      shinyDisconnected = true;
      bindServerDialog();
      // Shiny updates its overlay and any reconnect notification after the event.
      window.setTimeout(syncDialog, 0);
    });

    dialog.addEventListener('cancel', function (event) { event.preventDefault(); });
    document.getElementById('fm-disconnect-refresh').addEventListener('click', function (event) {
      const button = event.currentTarget;
      if (button.getAttribute('aria-disabled') === 'true') return;
      button.setAttribute('aria-disabled', 'true');
      // Leave time for the browser to paint the pressed state before navigation.
      window.setTimeout(() => window.location.reload(), 180);
    });
  });
})(window.jQuery);
