# Pitch and radar shared by the loader fallback and disconnect dialog.
fm_pitch_radar <- function(sweep = FALSE) {
  beam <- if (sweep) paste0(
    '<defs><clipPath id="fm-disconnect-pitch-clip">',
    '<rect x="48" y="38" width="324" height="204" rx="2"/></clipPath></defs>',
    '<g clip-path="url(#fm-disconnect-pitch-clip)"><g class="fm-radar-sweep" fill="currentColor">',
    '<path d="M210 140L109 -4A176 176 0 0 1 210 -36Z" opacity=".05"/>',
    '<path d="M210 140L144 -23A176 176 0 0 1 210 -36Z" opacity=".09"/>',
    '<path d="M210 140L177 -33A176 176 0 0 1 210 -36Z" opacity=".14"/>',
    '<path d="M210 140V-36" stroke="currentColor" stroke-width="1.6" opacity=".9"/>',
    '</g></g>'
  ) else '<path d="M210 140L286 64" stroke="currentColor" stroke-width="1.4" opacity=".5"/>'

  shiny::HTML(paste0(
      '<svg class="fm-loading-fallback" viewBox="0 0 420 280" focusable="false" aria-hidden="true">',
      '<g fill="none" stroke="currentColor" stroke-width="1.4">',
      '<rect x="48" y="38" width="324" height="204" rx="2"/>',
      '<path d="M210 38V242 M48 87H102V193H48 M372 87H318V193H372 ',
      'M48 112H70V168H48 M372 112H350V168H372"/>',
      '<path d="M48 122H36V158H48 M372 122H384V158H372"/>',
      '<path opacity=".45" stroke-width=".8" d="M42 122V158 M378 122V158 ',
      'M36 131H48 M36 140H48 M36 149H48 M372 131H384 M372 140H384 M372 149H384"/>',
      '<circle cx="210" cy="140" r="36"/>',
      '<circle cx="210" cy="140" r="76" opacity=".2"/>',
      '<circle cx="210" cy="140" r="108" opacity=".12"/>',
      '</g>', beam, '<g fill="currentColor">',
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
