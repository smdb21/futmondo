# Shared visual tokens and widget adapters. See docs/terminal_theme.md.
fm_theme_tokens <- function() list(bg="#050805", surface="#0B120D", text="#7CFF9B",
  muted="#9BBFA7", border="#285238", warning="#F4C95D", danger="#FF6B6B",
  font='ui-monospace, "Cascadia Code", "SFMono-Regular", Menlo, Consolas, "Liberation Mono", monospace')

fm_theme_css <- function() {
  tokens <- fm_theme_tokens()
  paste0(':root {',paste(paste0('--fm-',names(tokens),':',unlist(tokens),';'),collapse=''),'}')
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
  args$colorway <- c(t$text,t$warning,t$danger,t$muted,'#76D7EA')
  if (length(args$annotations)) args$annotations <- lapply(args$annotations,function(a) {
    a$font <- utils::modifyList(a$font %||% list(),list(color=t$text,family=t$font))
    a$bgcolor<-t$surface;a$bordercolor<-t$border;a
  })
  do.call(plotly::layout,c(list(p),args))
}

fm_reactable_theme <- function(...) {
  t <- fm_theme_tokens()
  reactable::reactableTheme(color=t$text,backgroundColor=t$surface,borderColor=t$border,
    stripedColor=t$bg,highlightColor='#173823',style=list(fontFamily=t$font),
    headerStyle=list(backgroundColor=t$bg,color=t$text),
    rowSelectedStyle=list(backgroundColor='#173823',boxShadow=paste('inset 2px 0 0',t$text)))
}
