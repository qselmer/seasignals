# Se ejecuta al adjuntar el paquete (library/seasignals)
.onAttach <- function(libname, pkgname) {
  logo <- "
▐▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▌
▐┌─┐┌─┐┌─┐┌─┐┬┌─┐┌┐┌┌─┐┬  ┌─┐▌
▐└─┐├┤ ├─┤└─┐││ ┬│││├─┤│  └─┐▌
▐└─┘└─┘┴ ┴└─┘┴└─┘┘└┘┴ ┴┴─┘└─┘▌
▐▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▌

  Marine & climate index utilities (++++++)
  "
  packageStartupMessage(logo)
}

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("utils", quietly = TRUE)) {
    utils::globalVariables(c(
      ".data", ".tmp",
      "index", "period_date", "value", "value_agg",
      "phase3", "phase3_event", "magnitud",
      "start_date", "end_date", "z",
      "season3m", "year", "month",
      "run_id", "is_event", "bio",
      "y0", "m0", "y1", "m1", "idx0", "idx1"
    ))
  }
}
