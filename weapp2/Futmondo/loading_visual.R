# Static pitch and radar shared by the loader and disconnect dialog.
fm_pitch_radar <- function() {
  shiny::HTML(paste0(
      '<svg class="fm-loading-fallback" viewBox="0 0 420 280" focusable="false" aria-hidden="true">',
      '<g fill="none" stroke="currentColor" stroke-width="1.4">',
      '<rect x="48" y="38" width="324" height="204" rx="2"/>',
      '<path d="M210 38V242 M48 87H102V193H48 M372 87H318V193H372 ',
      'M48 112H70V168H48 M372 112H350V168H372"/>',
      '<circle cx="210" cy="140" r="36"/>',
      '<circle cx="210" cy="140" r="76" opacity=".2"/>',
      '<circle cx="210" cy="140" r="108" opacity=".12"/>',
      '<path d="M210 140L286 64" opacity=".5"/>',
      '</g><g fill="currentColor">',
      '<circle cx="210" cy="140" r="2"/>',
      '<circle cx="82" cy="140" r="3"/><circle cx="138" cy="82" r="3"/>',
      '<circle cx="145" cy="188" r="3"/><circle cx="185" cy="109" r="3"/>',
      '<circle cx="248" cy="185" r="3"/><circle cx="282" cy="90" r="3"/>',
      '<circle cx="324" cy="143" r="3"/>',
      '</g></svg>'
  ))
}

# Decorative browser-only loading scene. See docs/progressive_loading.md.
fm_loading_visual <- function(id, compact = FALSE) {
  shiny::tags$div(
    id = id,
    class = paste("fm-loading-visual", if (isTRUE(compact)) "fm-loading-visual--compact"),
    `data-fm-loading` = "", `aria-hidden` = "true",
    fm_pitch_radar(),
    shiny::tags$canvas(class = "fm-loading-canvas", `aria-hidden` = "true")
  )
}
