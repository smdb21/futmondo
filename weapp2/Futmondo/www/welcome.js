(function () {
  'use strict';

  function bind(root) {
    if (root.dataset.loginBound || !window.Shiny) return;
    root.dataset.loginBound = 'true';
    var ns = root.dataset.fmWelcome;
    var element = function (id) { return document.getElementById(ns + id); };
    var dialog = element('login_dialog');
    var launcher = element('open_login');
    var close = element('close_login');
    var submit = element('login_button');
    var user = element('user_name');
    var password = element('password');
    var progress = element('login_progress');
    var feedback = element('login_feedback');
    var visual = element('welcome_visual');
    var motion = element('toggle_motion');
    var label = submit.textContent;
    var databaseReady = false;
    var pending = false;
    var paused = false;

    user.autocomplete = 'username';
    password.autocomplete = 'current-password';
    function updateControls() {
      launcher.disabled = submit.disabled = pending || !databaseReady;
      submit.setAttribute('aria-disabled', String(submit.disabled));
    }
    function updateMotion() {
      visual.toggleAttribute('data-fm-loading-paused', paused || dialog.open);
      motion.toggleAttribute('data-paused', paused);
      motion.title = paused ? 'Play animation' : 'Pause animation';
      motion.setAttribute('aria-label', motion.title);
    }
    motion.addEventListener('click', function () { paused = !paused; updateMotion(); });
    launcher.addEventListener('click', function () {
      if (launcher.disabled || dialog.open) return;
      dialog.showModal();
      updateMotion();
      user.focus();
    });
    function dismiss(returnFocus) {
      if (!dialog.open) return;
      if (password.value) {
        password.value = '';
        password.dispatchEvent(new Event('change', {bubbles: true}));
      }
      dialog.close();
      updateMotion();
      if (returnFocus) launcher.focus();
    }
    close.addEventListener('click', function () { if (!pending) dismiss(true); });
    dialog.addEventListener('cancel', function (event) {
      event.preventDefault();
      if (!pending) dismiss(true);
    });
    function setBusy(busy, message) {
      pending = !!busy;
      updateControls();
      close.disabled = pending;
      submit.setAttribute('aria-busy', String(pending));
      submit.textContent = pending ? 'Logging in…' : label;
      progress.querySelector('span').textContent = message || 'Connecting to LigaRadar…';
      progress.hidden = !pending;
      feedback.hidden = pending;
    }
    // This capture listener gives immediate feedback before Shiny dispatches the click.
    submit.addEventListener('click', function (event) {
      if (pending || !databaseReady || !dialog.open) {
        event.preventDefault();
        event.stopImmediatePropagation();
      } else {
        // Shiny debounces typing. Commit both current values before the action,
        // including autofill and Enter without a preceding blur.
        [user, password].forEach(function (field) {
          field.dispatchEvent(new Event('change', {bubbles: true}));
        });
        setBusy(true);
      }
    }, true);
    [user, password].forEach(function (field) {
      field.addEventListener('keydown', function (event) {
        if (event.key === 'Enter' && !event.isComposing) {
          event.preventDefault();
          if (!submit.disabled) submit.click();
        }
      });
    });
    Shiny.addCustomMessageHandler(ns + 'database_state', function (message) {
      databaseReady = message.ready === true;
      updateControls();
    });
    Shiny.addCustomMessageHandler(ns + 'login_state', function (message) { setBusy(message.busy, message.message); });
    Shiny.addCustomMessageHandler(ns + 'login_admitted', function (message) { dismiss(false); });
    updateControls();
  }

  function start() { document.querySelectorAll('[data-fm-welcome]').forEach(bind); }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start, {once: true});
  else start();
  if (window.jQuery) window.jQuery(document).one('shiny:connected.fmWelcome', start);
})();
