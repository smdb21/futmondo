(function () {
  const root = document.documentElement;
  // Shiny also publishes this output for a remembered browser login.
  $(document).on('shiny:value', function (event) {
    if (event.name !== 'login-signed_in') return;
    const signedIn = event.value === 'TRUE';
    root.classList.toggle('fm-signed-in', signedIn);
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
    if (window.matchMedia('(max-width: 767px)').matches && document.body.classList.contains('sidebar-open')) {
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
