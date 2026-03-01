lc <- Sys.localeconv()

format_currency <- function(value, currency_symbol = "€", add_currency_symbol = TRUE, add_symbol = FALSE) {
  ret <- format(value, big.mark = lc["mon_thousands_sep"])
  if (add_symbol) {
    if (value > 0) {
      ret <- paste0("+", ret)
    }
  }
  if (add_currency_symbol) {
    ret <- paste0(ret, " ", currency_symbol)
  }
  return(ret)
}
