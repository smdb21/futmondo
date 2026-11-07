# Shared visual tokens and widget adapters. See docs/terminal_theme.md.
fm_theme_tokens <- function(mode = "dark") {
  tokens <- list(bg="#050805", surface="#0B120D", text="#7CFF9B",
  muted="#9BBFA7", border="#285238", warning="#F4C95D", danger="#FF9494",
  font='ui-monospace, "Cascadia Code", "SFMono-Regular", Menlo, Consolas, "Liberation Mono", monospace')
  tokens$highlight <- '#173823'
  tokens$error <- '#24100f'
  tokens$accent <- '#76D7EA'
  if (identical(mode, "light")) tokens <- utils::modifyList(tokens, list(
    bg="#F3F7F3", surface="#FFFFFF", text="#14532D", muted="#486052",
    border="#BDCEBF", warning="#785800", danger="#B42332",
    highlight="#E1EEE3", error="#FFF0F0", accent="#168296"))
  tokens
}

fm_theme_css <- function() {
  rule <- function(selector, mode) {
    tokens <- fm_theme_tokens(mode)
    paste0(selector, ' {color-scheme:', mode, ';',
      paste(paste0('--fm-', names(tokens), ':', unlist(tokens), ';'), collapse=''), '}')
  }
  paste(rule(':root', 'dark'), rule(':root[data-fm-theme="light"]', 'light'))
}

fm_plot_layout <- function(p, ...) {
  t <- fm_theme_tokens()
  args <- list(...)
  args$paper_bgcolor <- t$surface; args$plot_bgcolor <- t$surface
  args$font <- utils::modifyList(args$font %||% list(),list(family=t$font,color=t$text,size=13))
  for (axis in c('xaxis','yaxis','xaxis2','yaxis2')) args[[axis]] <- utils::modifyList(
    args[[axis]] %||% list(),list(gridcolor=t$border,zerolinecolor=t$border,
      tickfont=list(color=t$muted,family=t$font),automargin=TRUE))
  args$hoverlabel <- list(bgcolor=t$surface,font=list(color=t$text,family=t$font),bordercolor=t$border)
  args$colorway <- c(t$text,t$warning,t$danger,t$muted,t$accent)
  if (length(args$annotations)) args$annotations <- lapply(args$annotations,function(a) {
    a$font <- utils::modifyList(a$font %||% list(),list(color=t$text,family=t$font))
    a$bgcolor<-t$surface;a$bordercolor<-t$border;a
  })
  htmlwidgets::onRender(do.call(plotly::layout,c(list(p),args)),
    "function(el) { if (window.fmApplyPlotTheme) window.fmApplyPlotTheme(el); }")
}

fm_reactable_theme <- function(...) {
  t <- lapply(setNames(names(fm_theme_tokens()), names(fm_theme_tokens())),
    function(name) paste0('var(--fm-', name, ')'))
  reactable::reactableTheme(color=t$text,backgroundColor=t$surface,borderColor=t$border,
    stripedColor=t$bg,highlightColor=t$highlight,style=list(fontFamily=t$font),
    headerStyle=list(backgroundColor=t$bg,color=t$text),
    rowSelectedStyle=list(backgroundColor=t$highlight,boxShadow=paste('inset 2px 0 0',t$text)))
}
