#' Añadir clasificación ICEN a un data.frame con columna value
#' @param df data.frame con columnas date, value (y opcional index)
#' @param value_col nombre de la columna numérica con ICEN
#' @return df con phase3, magnitud, escenario
#' @export
add_icen_classification <- function(df, value_col = "value") {
  stopifnot(value_col %in% names(df))
  cl <- icen_classify_value(df[[value_col]])
  cbind(df, cl)
}
