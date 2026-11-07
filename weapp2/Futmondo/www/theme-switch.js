(function () {
  'use strict';
  var storageKey = 'futmondo.color-theme';
  var media = window.matchMedia('(prefers-color-scheme: dark)');
  var choice = 'system';
  function valid(value) { return ['system', 'light', 'dark'].indexOf(value) !== -1; }
  try {
    var saved = window.localStorage.getItem(storageKey);
    if (valid(saved)) choice = saved;
  } catch (error) { /* Theme selection still works when storage is unavailable. */ }

  // Plotly needs resolved colors; CSS variables handle all other widgets directly.
  window.fmApplyPlotTheme = function (plot) {
    if (!window.Plotly || !plot.layout) return;
    var css = window.getComputedStyle(document.documentElement);
    function token(name) { return css.getPropertyValue('--fm-' + name).trim(); }
    var layout = {
      paper_bgcolor: token('surface'), plot_bgcolor: token('surface'),
      'font.color': token('text'), 'legend.font.color': token('text'),
      'title.font.color': token('text'),
      'hoverlabel.bgcolor': token('surface'), 'hoverlabel.bordercolor': token('border'),
      'hoverlabel.font.color': token('text'),
      colorway: [token('text'), token('warning'), token('danger'), token('muted'), token('accent')]
    };
    Object.keys(plot.layout).filter(function (key) { return /^[xy]axis\d*$/.test(key); }).forEach(function (axis) {
      layout[axis + '.gridcolor'] = token('border');
      layout[axis + '.zerolinecolor'] = token('border');
      layout[axis + '.tickfont.color'] = token('muted');
      layout[axis + '.title.font.color'] = token('text');
    });
    (plot.layout.annotations || []).forEach(function (annotation, index) {
      layout['annotations[' + index + '].font.color'] = token('text');
      layout['annotations[' + index + '].bgcolor'] = token('surface');
      layout['annotations[' + index + '].bordercolor'] = token('border');
    });
    window.Plotly.relayout(plot, layout);
  };

  function apply() {
    var theme = choice === 'system' ? (media.matches ? 'dark' : 'light') : choice;
    var root = document.documentElement;
    var changed = root.getAttribute('data-fm-theme') !== theme;
    root.setAttribute('data-fm-theme', theme);
    var toggle = document.getElementById('fm-theme-toggle');
    if (toggle) {
      var label = 'Color theme: ' + choice.charAt(0).toUpperCase() + choice.slice(1);
      toggle.setAttribute('aria-label', label);
      toggle.title = label;
      toggle.dataset.themeChoice = choice;
      document.querySelectorAll('#fm-theme-options [data-theme]').forEach(function (button) {
        var selected = button.dataset.theme === choice;
        button.setAttribute('aria-pressed', String(selected));
        if (selected) toggle.replaceChildren(button.firstElementChild.cloneNode(true));
      });
    }
    if (changed) document.querySelectorAll('.js-plotly-plot').forEach(window.fmApplyPlotTheme);
  }
  apply(); // Resolve the saved preference in the head, before the body is painted.
  document.addEventListener('DOMContentLoaded', apply);
  function setOpen(open, restoreFocus) {
    var toggle = document.getElementById('fm-theme-toggle');
    var options = document.getElementById('fm-theme-options');
    if (!toggle || !options) return;
    options.hidden = !open;
    toggle.setAttribute('aria-expanded', String(open));
    if (open) options.querySelector('[aria-pressed="true"]').focus();
    else if (restoreFocus) toggle.focus();
  }
  document.addEventListener('click', function (event) {
    var control = document.querySelector('.fm-theme-control');
    if (!control) return;
    if (event.target.closest('#fm-theme-toggle')) {
      setOpen(document.getElementById('fm-theme-options').hidden);
      return;
    }
    var option = event.target.closest('#fm-theme-options [data-theme]');
    if (option && valid(option.dataset.theme)) {
      choice = option.dataset.theme;
      try { window.localStorage.setItem(storageKey, choice); } catch (error) { /* Session-only fallback. */ }
      apply();
      setOpen(false, true);
    } else if (!control.contains(event.target)) setOpen(false);
  });
  document.addEventListener('keydown', function (event) {
    if (!event.target.closest('.fm-theme-control')) return;
    if (event.key === 'Escape') {
      setOpen(false, true);
      event.preventDefault();
    } else if (event.target.id === 'fm-theme-toggle' && event.key === 'ArrowDown') {
      setOpen(true);
      event.preventDefault();
    } else if (event.target.closest('#fm-theme-options') && ['ArrowLeft', 'ArrowRight', 'Home', 'End'].indexOf(event.key) !== -1) {
      var buttons = Array.from(document.querySelectorAll('#fm-theme-options [data-theme]'));
      var index = buttons.indexOf(event.target);
      if (event.key === 'Home') index = 0;
      else if (event.key === 'End') index = buttons.length - 1;
      else index = (index + (event.key === 'ArrowRight' ? 1 : -1) + buttons.length) % buttons.length;
      buttons[index].focus();
      event.preventDefault();
    }
  });
  document.addEventListener('focusout', function (event) {
    var control = event.target.closest('.fm-theme-control');
    if (control && !control.contains(event.relatedTarget)) setOpen(false);
  });
  function systemChanged() { if (choice === 'system') apply(); }
  if (media.addEventListener) media.addEventListener('change', systemChanged);
  else media.addListener(systemChanged);
  window.addEventListener('storage', function (event) {
    if (event.key !== storageKey && event.key !== null) return;
    choice = valid(event.newValue) ? event.newValue : 'system';
    apply();
  });
})();
