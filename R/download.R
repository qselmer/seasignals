#' Descargar y leer ICEN (IGP/ENFEN) desde ICEN.txt
#'
#' Fuente: https://met.igp.gob.pe/datos/ICEN.txt
#' Formato esperado: líneas comentario con '%' y luego columnas yy mm ICEN.
#'
#' @param url URL del archivo ICEN.
#' @param retries reintentos de descarga.
#' @param timeout_seg timeout por intento (seg).
#' @return data.frame con columnas: date, index, value, year, month.
#' @export
download_icen <- function(
    url = "https://met.igp.gob.pe/datos/ICEN.txt",
    retries = 3,
    timeout_seg = 30
) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("Instala 'curl' para descarga robusta: install.packages('curl')")
  }

  fetch_text <- function(u) {
    last_err <- NULL
    for (k in seq_len(retries)) {
      h <- curl::new_handle(timeout = timeout_seg)
      res <- tryCatch(
        curl::curl_fetch_memory(u, handle = h),
        error = function(e) { last_err <<- e; NULL }
      )
      if (!is.null(res)) return(rawToChar(res$content))
    }
    stop("No se pudo descargar ICEN desde: ", u, "\nÚltimo error: ", conditionMessage(last_err))
  }

  # 1) https, 2) fallback http
  txt <- tryCatch(fetch_text(url), error = function(e) {
    if (grepl("^https://", url)) fetch_text(sub("^https://", "http://", url)) else stop(e)
  })

  lines <- unlist(strsplit(txt, "\n", fixed = TRUE))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  lines <- lines[!grepl("^%|^#", lines)]      # elimina comentarios
  lines <- lines[grepl("^\\d{4}\\s+", lines)] # solo filas con año al inicio

  if (length(lines) == 0) stop("ICEN: no se encontraron filas de datos (yy mm value).")

  parts <- strsplit(lines, "\\s+")
  # tomar 3 primeras columnas: yy, mm, ICEN
  mat <- do.call(rbind, lapply(parts, function(x) x[1:3]))
  if (ncol(mat) < 3) stop("ICEN: formato inesperado (no hay 3 columnas).")

  year  <- as.integer(mat[, 1])
  month <- as.integer(mat[, 2])
  value <- as.numeric(mat[, 3])

  ok <- !is.na(year) & !is.na(month) & month >= 1 & month <= 12 & !is.na(value)
  year <- year[ok]; month <- month[ok]; value <- value[ok]

  date <- as.Date(sprintf("%d-%02d-01", year, month))

  data.frame(
    date  = date,
    index = "ICEN",
    value = value,
    year  = year,
    month = month,
    stringsAsFactors = FALSE
  )
}
