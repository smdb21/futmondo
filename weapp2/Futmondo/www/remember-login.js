(function () {
  'use strict';
  if (window.futmondoRememberBound) return;
  window.futmondoRememberBound = true;
  var queue = Promise.resolve();
  function bind() {
    if (!window.Shiny) return;
    Shiny.addCustomMessageHandler('futmondo_remember', function (message) {
      queue = queue.catch(function () {}).then(async function () {
        var status = 'network';
        try {
          var endpoint = new URL(message.url, document.baseURI);
          if (endpoint.origin !== location.origin || location.protocol !== 'https:') throw new Error('Invalid session endpoint');
          var response = await fetch(endpoint.href, {
            method: 'POST', credentials: 'same-origin', cache: 'no-store',
            headers: {'Content-Type': 'application/json', 'X-Futmondo-CSRF': message.csrf},
            body: JSON.stringify({action: message.action, ticket: message.ticket})
          });
          status = (await response.json()).status;
        } catch (_) { /* No authentication material is logged or sent to Shiny. */ }
        Shiny.setInputValue(message.input, {ticket: message.ticket, status: status}, {priority: 'event'});
      });
    });
  }
  if (window.Shiny) bind();
  else document.addEventListener('shiny:connected', bind, {once: true});
})();
