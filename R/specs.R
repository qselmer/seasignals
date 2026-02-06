# Catálogo: definiciones, neutralidad, bins por índice, metadatos.

#' Catálogo de índices y criterios de clasificación
#' @export
index_specs <- function() {
  tibble::tibble(
    index = c("ICEN", "ONI", "LABCOS", "SOI", "PDO"),
    scale = c("regional_coastal", "macro", "local", "atmos_coupled", "decadal"),
    freq  = c("monthly_or_3m",    "3m",    "3m",   "monthly",        "monthly"),
    scheme = c("icen_bins",       "oni_3cat","labcos_bins","sign","sign"),
    neutral_lo = c(-0.7, -0.5, -0.78, NA_real_, NA_real_),
    neutral_hi = c( 0.5,  0.5,  0.27, NA_real_, NA_real_)
  )
}
