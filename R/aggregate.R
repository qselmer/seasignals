# Construir trimestral/semestre/anual: mean/median/maxabs/severity/dominancia.

#' Generar clave de periodo
#' @param date vector Date/POSIXct.
#' @param period periodo (p.ej. "month", "year", "season3m").
#' @return vector con clave del periodo.
#' @export
period_key <- function(date, period = c("month","quarter","semester","year")) {
  period <- match.arg(period)
  date <- as.Date(date)

  if (period == "month")   return(as.Date(format(date, "%Y-%m-01")))
  if (period == "quarter") return(lubridate::floor_date(date, "quarter"))
  if (period == "year")    return(lubridate::floor_date(date, "year"))

  y <- lubridate::year(date); m <- lubridate::month(date)
  as.Date(sprintf("%d-%02d-01", y, ifelse(m <= 6, 1, 7)))
}

#' Agregar (resumir) valores por periodo
#' @param df data.frame con columnas date, index y value (o equivalentes según tu implementación).
#' @param period periodo de agregación (p.ej. "month", "season3m", "year") según tu API.
#' @param method método de agregación (p.ej. "mean", "median", "sum", etc.).
#' @param warm_thr umbral para clasificar condición cálida (si aplica).
#' @param cold_thr umbral para clasificar condición fría (si aplica).
#' @return data.frame agregado por periodo.
#' @export
aggregate_index <- function(df, period = c("quarter","semester","year"),
                            method = c("mean","median","maxabs","severity"),
                            warm_thr = 0.5, cold_thr = -0.5) {
  period <- match.arg(period)
  method <- match.arg(method)

  df2 <- df |>
    dplyr::mutate(period_date = period_key(date, period))

  df2 |>
    dplyr::group_by(index, period_date) |>
    dplyr::summarise(
      n = sum(!is.na(value)),
      value_agg = dplyr::case_when(
        method == "mean"   ~ mean(value, na.rm = TRUE),
        method == "median" ~ median(value, na.rm = TRUE),
        method == "maxabs" ~ value[which.max(abs(value))][1],
        method == "severity" ~ {
          sum(pmax(value - warm_thr, 0), na.rm = TRUE) - sum(pmax(cold_thr - value, 0), na.rm = TRUE)
        },
        TRUE ~ NA_real_
      ),
      .groups = "drop"
    ) |>
    dplyr::rename(date = period_date, value = value_agg) |>
    add_classification(index_col = "index", value_col = "value")
}

#' Clasificar periodos por dominancia de fase
#' @param df data.frame con columnas period_date e indicador de fase.
#' @param period periodo de agregación.
#' @param p_dom proporción mínima para declarar dominancia (0-1).
#' @return data.frame con fase dominante por periodo.
#' @export
classify_period_by_dominance <- function(df, period = c("quarter","semester","year"), p_dom = 1/3) {
  period <- match.arg(period)

  df2 <- df |>
    add_classification() |>
    dplyr::mutate(period_date = period_key(date, period))

  df2 |>
    dplyr::group_by(index, period_date) |>
    dplyr::summarise(
      share_nino = mean(phase3 == "Niño", na.rm = TRUE),
      share_nina = mean(phase3 == "Niña", na.rm = TRUE),
      share_neut = mean(phase3 == "Neutro", na.rm = TRUE),
      phase_period = dplyr::case_when(
        share_nino >= p_dom & share_nina >= p_dom ~ "Mixto",
        share_nino >= p_dom ~ "Niño",
        share_nina >= p_dom ~ "Niña",
        TRUE ~ "Neutro"
      ),
      .groups = "drop"
    ) |>
    dplyr::rename(date = period_date)
}
