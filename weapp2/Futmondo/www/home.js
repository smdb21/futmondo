(function () {
  const root = document.documentElement;
  const mobile = window.matchMedia('(max-width: 767px)');
  const scrollIdleMs = 550;
  let actions, idleTimer = 0, lastScroll = 0, ignoreTopReset = false;

  function finishScrolling() {
    const remaining = scrollIdleMs - (performance.now() - lastScroll);
    if (remaining > 0) {
      idleTimer = window.setTimeout(finishScrolling, remaining);
      return;
    }
    idleTimer = 0;
    actions.classList.remove('is-scrolling');
  }

  function showWhileScrolling(event) {
    if (event.target === document) {
      // Switching pages resets the viewport to the top.
      const ignore = ignoreTopReset && window.scrollY === 0 && window.scrollX === 0;
      ignoreTopReset = false;
      if (ignore) return;
    } else if (!event.target.closest('.content-wrapper') || event.target.closest('.modal, dialog')) {
      return;
    }
    lastScroll = performance.now();
    if (!idleTimer) {
      actions.classList.add('is-scrolling');
      idleTimer = window.setTimeout(finishScrolling, scrollIdleMs);
    }
  }

  function updateScrollTracking() {
    if (!actions) return;
    window.clearTimeout(idleTimer);
    idleTimer = 0;
    actions.classList.remove('is-scrolling');
    ignoreTopReset = true;
    if (mobile.matches && root.classList.contains('fm-signed-in') && !document.querySelector('#shiny-tab-login.active')) {
      document.addEventListener('scroll', showWhileScrolling, {capture: true, passive: true});
    } else {
      document.removeEventListener('scroll', showWhileScrolling, true);
    }
  }

  $(function () {
    actions = document.querySelector('.fm-page-actions');
    updateScrollTracking();
    mobile.addEventListener('change', updateScrollTracking);
  });

  // Shiny also publishes this output for a remembered browser login.
  $(document).on('shiny:value', function (event) {
    if (event.name !== 'login-signed_in') return;
    const signedIn = event.value === 'TRUE';
    root.classList.toggle('fm-signed-in', signedIn);
    updateScrollTracking();
    if (!signedIn) document.body.classList.remove('sidebar-open');
    else window.requestAnimationFrame(function () {
      const heading = document.querySelector('#shiny-tab-login.active .fm-home h1');
      if (heading) heading.focus({preventScroll: true});
    });
  });

  document.addEventListener('click', function (event) {
    const button = event.target.closest('[data-fm-page]');
    if (!button || !root.classList.contains('fm-signed-in')) return;
    const target = button.dataset.fmPage;
    // Use the same authorized link and Shiny tab binding as the sidebar.
    const link = Array.from(document.querySelectorAll('#menu a[data-value]'))
      .find(function (item) { return item.dataset.value === target; });
    if (link) link.click();
  });
  $(document).on('shown.bs.tab', '#menu a[data-value]', function () {
    if (!root.classList.contains('fm-signed-in')) return;
    updateScrollTracking();
    if (mobile.matches && document.body.classList.contains('sidebar-open')) {
      document.querySelector('.sidebar-toggle').click();
    }
    const panel = document.getElementById('shiny-tab-' + this.dataset.value);
    const heading = panel && panel.querySelector(this.dataset.value === 'login' ?
      '.fm-home h1' : '.fm-information-heading h2');
    if (heading) {
      heading.setAttribute('tabindex', '-1');
      heading.focus({preventScroll: true});
    }
    window.scrollTo(0, 0);
  });
})();
