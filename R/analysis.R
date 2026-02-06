# Alinear series + lags + correlación + modelos (LM/GAM básico).

#' Correlación con rezagos entre índice y biomasa (u otra serie)
#' @param index_df data.frame con serie del índice.
#' @param bio_df data.frame con serie biológica (p.ej. biomasa).
#' @param max_lag máximo rezago (en unidades del periodo de la serie).
#' @return data.frame/tibble con correlaciones por lag.
#' @export
lag_correlation <- function(index_df, bio_df, max_lag = 24) {
  df <- dplyr::inner_join(
    index_df |> dplyr::select(date, idx = value),
    bio_df   |> dplyr::select(date, bio = value),
    by = "date"
  ) |> dplyr::arrange(date)

  out <- lapply(-max_lag:max_lag, function(L) {
    bio_l <- dplyr::lag(df$bio, n = L)
    tibble::tibble(
      lag = L,
      cor = suppressWarnings(stats::cor(df$idx, bio_l, use = "complete.obs"))
    )
  })
  dplyr::bind_rows(out)
}

#' Resumen biológico por fase ICEN (eventos/tablas)
#'
#' @param icen_df data.frame con date, year, month, value
#' @param bio_df data.frame con date, value
#' @return data.frame con medias y diferencias vs Neutro
#' @export
bio_by_icen_phase <- function(icen_df, bio_df) {
  icen_df <- icen_df |>
    dplyr::mutate(date = as.Date(date)) |>
    add_icen_event_classification(year_col = "year", month_col = "month") |>
    dplyr::select(date, phase3_event)

  df <- dplyr::inner_join(
    icen_df,
    bio_df |> dplyr::mutate(date = as.Date(date)) |> dplyr::select(date, bio = value),
    by = "date"
  )

  summ <- df |>
    dplyr::group_by(phase3_event) |>
    dplyr::summarise(
      n = sum(!is.na(bio)),
      mean_bio = mean(bio, na.rm = TRUE),
      sd_bio = stats::sd(bio, na.rm = TRUE),
      .groups = "drop"
    )

  neut <- summ$mean_bio[summ$phase3_event == "Neutro"]
  summ$delta_vs_neutro <- summ$mean_bio - neut

  summ
}

