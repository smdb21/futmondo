(function () {
  "use strict";

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
    marker.addEventListener("click", function (event) {
      const premium = event.target.closest("[data-fm-offer-premium]");
      if (!premium || marker.dataset.sending) return;
      field.value = premium.dataset.fmOfferPremium;
      field.dispatchEvent(new Event("change", { bubbles: true }));
    });
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
