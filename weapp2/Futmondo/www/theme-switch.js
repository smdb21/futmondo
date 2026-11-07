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
    var selector = document.getElementById('fm-theme-choice');
    if (selector) selector.value = choice;
    if (changed) document.querySelectorAll('.js-plotly-plot').forEach(window.fmApplyPlotTheme);
  }
  apply(); // Resolve the saved preference in the head, before the body is painted.
  document.addEventListener('DOMContentLoaded', apply);
  document.addEventListener('change', function (event) {
    if (event.target.id !== 'fm-theme-choice' || !valid(event.target.value)) return;
    choice = event.target.value;
    try { window.localStorage.setItem(storageKey, choice); } catch (error) { /* Session-only fallback. */ }
    apply();
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
