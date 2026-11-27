(function () {
  "use strict";

  const euros = new Intl.NumberFormat("de-DE", {maximumFractionDigits: 2});
  const percent = new Intl.NumberFormat("en", {maximumFractionDigits: 2, signDisplay: "exceptZero"});

  function bindMarketBidEditor(editor) {
    const field = editor.querySelector("input");
    const base = Number(editor.dataset.base);
    const playerValue = Number(editor.dataset.playerValue);
    const total = editor.querySelector(".fm-offer-total");
    const change = editor.querySelector(".fm-offer-percent");
    const warning = editor.querySelector(".fm-offer-warning");
    let anchor = field.valueAsNumber;
    let addedPercent = 0;

    function preview() {
      const amount = field.valueAsNumber;
      const valid = Number.isFinite(amount);
      total.textContent = valid ? euros.format(amount) + " €" : "";
      change.textContent = valid ? percent.format(100 * (amount - base) / base) + "%" : "";
      if (amount < base) {
        warning.textContent = "Offer is below the " + editor.dataset.baseLabel + ".";
      } else if (amount < playerValue) {
        warning.textContent = "Offer is below the player’s market value (" + euros.format(playerValue) + " €).";
      } else {
        warning.textContent = "";
      }
    }

    field.addEventListener("input", function () {
      anchor = field.valueAsNumber;
      addedPercent = 0;
      preview();
    });
    editor.addEventListener("click", function (event) {
      const preset = event.target.closest("[data-fm-offer-premium]");
      if (!preset || field.disabled) return;
      if (!Number.isFinite(anchor)) anchor = base;
      addedPercent += Number(preset.dataset.fmOfferPremium);
      field.value = Math.ceil(anchor + base * addedPercent / 100);
      field.dispatchEvent(new Event("change", { bubbles: true }));
      preview();
    });
    preview();
  }

  function unlock(marker) {
    const modal = marker.closest(".modal");
    modal.querySelector(".fm-offer-pending").textContent = "";
    delete marker.dataset.sending;
    for (const control of modal.querySelectorAll("[data-fm-offer-locked]")) {
      control.disabled = false;
      delete control.dataset.fmOfferLocked;
    }
  }

  $(document).on("show.bs.modal", ".modal", function () {
    const modal = this;
    const editor = modal.querySelector(".fm-market-bid-editor");
    if (editor) bindMarketBidEditor(editor);
    const marker = modal.querySelector(".fm-offer-dialog");
    if (!marker) return;
    const field = marker.dataset.amountId ? document.getElementById(marker.dataset.amountId) : null;
    const submit = document.getElementById(marker.dataset.submitId);
    const status = modal.querySelector(".fm-offer-pending");
    if (field) field.required = true;

    if (marker.dataset.closedInput) {
      $(modal).one("hidden.bs.modal", function () {
        Shiny.setInputValue(marker.dataset.closedInput, marker.dataset.generation, { priority: "event" });
      });
    }
    function keepOpenWhileSending(event) {
      if (!marker.dataset.sending ||
          (event.type === "keydown" ? event.key !== "Escape" : event.target !== modal)) return;
      event.preventDefault();
      event.stopImmediatePropagation();
    }
    modal.addEventListener("keydown", keepOpenWhileSending, true);
    modal.addEventListener("click", keepOpenWhileSending, true);
    submit.addEventListener("click", function () {
      if (marker.dataset.sending || (field && !field.reportValidity())) return;
      const amount = field?.valueAsNumber;
      marker.dataset.sending = "true";
      for (const control of modal.querySelectorAll("button, input")) {
        if (control.disabled) continue;
        control.disabled = true;
        control.dataset.fmOfferLocked = "true";
      }
      const icon = document.createElement("span");
      icon.className = "fas fa-spinner";
      if (!window.matchMedia("(prefers-reduced-motion: reduce)").matches) icon.classList.add("fa-spin");
      icon.setAttribute("aria-hidden", "true");
      const operation = marker.dataset.operation || "offer";
      status.replaceChildren(icon, document.createTextNode(
        " Sending " + operation + "… Waiting for Futmondo confirmation."));
      const intent = field ? { generation: marker.dataset.generation, amount } : { confirmedAt: Date.now() };
      Shiny.setInputValue(marker.dataset.submitInput, intent, { priority: "event" });
    });
  });

  Shiny.addCustomMessageHandler("fm-offer-unlock", function (message) {
    const marker = document.getElementById(message.id);
    if (marker?.dataset.generation === String(message.generation)) unlock(marker);
  });
}());
