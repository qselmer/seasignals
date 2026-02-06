# Detectar eventos y preparar data para geom_rect.

#' Construir variable de evento a partir de fases
#' @param df data.frame con columna de fecha y fase.
#' @param date_col nombre de la columna de fecha.
#' @param phase_col nombre de la columna de fase (phase3).
#' @return data.frame con columna is_event (u otra según tu implementación).
#' @export
make_events <- function(df, date_col = "date", phase_col = "phase3") {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Instala data.table: install.packages('data.table')")
  }

  d <- df |>
    dplyr::arrange(.data[[date_col]]) |>
    add_classification()

  d |>
    dplyr::mutate(
      is_event = .data[[phase_col]] != "Neutro",
      run_id = data.table::rleid(is_event, .data[[phase_col]])
    ) |>
    dplyr::group_by(index, run_id) |>
    dplyr::summarise(
      phase3 = dplyr::first(.data[[phase_col]]),
      start_date = min(.data[[date_col]], na.rm = TRUE),
      end_date   = max(.data[[date_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(phase3 != "Neutro")
}
