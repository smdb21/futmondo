(function () {
  function update(event) {
    const handle = event.target;
    if (!handle.classList || !handle.classList.contains('fm-round-handle')) return;
    const range = handle.closest('.fm-round-range');
    const [from, through] = range.querySelectorAll('input');
    const labels = range.dataset.rounds.split(',');
    const last = labels.length - 1;
    if (handle === from) from.value = Math.min(from.valueAsNumber, through.valueAsNumber, last - 1);
    else through.value = Math.max(through.valueAsNumber, from.valueAsNumber, 1);

    const endpoints = [labels[from.valueAsNumber], labels[through.valueAsNumber]];
    [from, through].forEach((input, i) => {
      input.setAttribute('aria-valuetext', endpoints[i]);
      range.style.setProperty(i === 0 ? '--from' : '--through', input.valueAsNumber / last);
    });
    range.querySelectorAll('.fm-round-labels strong').forEach((label, i) => {
      label.textContent = endpoints[i];
    });
    // Send round identities once the drag settles. A later publication can reorder the stops.
    if (event.type === 'change') Shiny.setInputValue(range.dataset.input, endpoints);
  }
  document.addEventListener('input', update);
  document.addEventListener('change', update);
})();
