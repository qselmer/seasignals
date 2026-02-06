# Figuras base: bandas + serie(s).

`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Graficar bandas del índice junto con serie biológica
#' @param index_df data.frame del índice.
#' @param bio_df data.frame de la serie biológica.
#' @param alpha_band transparencia de la banda (0-1).
#' @param index_label etiqueta para el índice.
#' @param bio_label etiqueta para la serie biológica.
#' @return objeto ggplot.
#' @export
plot_index_bands_with_bio <- function(index_df, bio_df,
                                      alpha_band = 0.18,
                                      index_label = NULL,
                                      bio_label = "Bio") {

  idx <- index_df |>
    dplyr::mutate(date = as.Date(date)) |>
    add_classification()

  ev <- make_events(idx)

  idx_std <- idx |> dplyr::mutate(z = scale(value)[,1])

  bio_std <- bio_df |>
    dplyr::mutate(date = as.Date(date), z = scale(value)[,1])

  ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = ev,
      ggplot2::aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf, fill = phase3),
      alpha = alpha_band, inherit.aes = FALSE
    ) +
    ggplot2::geom_line(data = idx_std, ggplot2::aes(x = date, y = z), linewidth = 0.7) +
    ggplot2::geom_line(data = bio_std, ggplot2::aes(x = date, y = z), linewidth = 0.7, linetype = 2) +
    ggplot2::labs(
      x = NULL, y = "z-score", fill = "Fase",
      title = paste0(index_label %||% unique(idx$index)[1], " (bandas) + ", bio_label)
    ) +
    ggplot2::theme_minimal()
}
