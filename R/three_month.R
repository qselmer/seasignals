# convertir tabla DEF..NDE a serie mensual con mes central (o desde fechas).

#' Mes central de una estación móvil de 3 meses
#' @param season3m etiqueta de estación 3m (según tu convención).
#' @return entero 1..12 del mes central.
#' @export

season3m_center_month <- function(season3m) {
  seasons <- c("DEF","EFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDE")
  idx <- match(season3m, seasons)
  ifelse(is.na(idx), NA_integer_, idx)  # DEF->1, ..., NDE->12
}

#' Convertir formato wide (3m) a mensual
#' @param df_wide data.frame en formato wide.
#' @param year_col nombre de la columna año.
#' @param index_name nombre del índice a asignar.
#' @return data.frame mensual en formato tidy.
#' @export
wide3m_to_monthly <- function(df_wide, year_col = "Año", index_name = "ONI") {
  seasons <- c("DEF","EFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDE")
  stopifnot(year_col %in% names(df_wide))

  df_wide |>
    dplyr::rename(year = !!year_col) |>
    tidyr::pivot_longer(dplyr::all_of(seasons), names_to = "season3m", values_to = "value") |>
    dplyr::mutate(
      month = season3m_center_month(season3m),
      date  = as.Date(sprintf("%d-%02d-01", as.integer(year), as.integer(month))),
      index = index_name,
      value = as.numeric(value)
    ) |>
    dplyr::select(date, index, value, season3m, year, month) |>
    dplyr::arrange(date)
}
