# =========================
# Helpers / Normalización
# =========================

.to_index <- function(y, m) as.integer(y) * 12L + as.integer(m)

.norm_mag_icen <- function(x) {
  # Normaliza etiquetas a un set estable (femenino por consistencia con "magnitud")
  x <- as.character(x)
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x %in% c("Moderado", "Moderada") ~ "Moderada",
    x %in% c("Extraordinario", "Extraordinaria") ~ "Extraordinaria",
    x %in% c("Debil", .debil) ~ .debil,
    x %in% c("Fuerte") ~ "Fuerte",
    x %in% c("Neutra", .neutro) ~ "Neutra",
    TRUE ~ x
  )
}

.make_result <- function(phase3, magnitud) {
  magnitud <- .norm_mag_icen(magnitud)
  escenario <- dplyr::if_else(phase3 == .neutro, .neutro, paste(phase3, magnitud))
  tibble::tibble(phase3 = phase3, magnitud = magnitud, escenario = escenario)
}

# =========================
# ICEN: por valor (Tabla ENFEN)
# =========================

#' Clasificar ICEN por valor (fase y magnitud)
#' @param x vector numérico ICEN
#' @return tibble: phase3, magnitud, escenario
#' @export
icen_classify_value <- function(x) {
  x <- as.numeric(x)

  phase3 <- dplyr::case_when(
    x < -0.7 ~ .nina,
    x >  0.5 ~ .nino,
    TRUE     ~ .neutro
  )

  magnitud <- dplyr::case_when(
    x <  -1.3              ~ "Fuerte",
    x >= -1.3 & x < -1.1   ~ "Moderada",
    x >= -1.1 & x < -0.7   ~ .debil,
    x >= -0.7 & x <= 0.5   ~ "Neutra",
    x >   0.5 & x <= 1.3   ~ .debil,
    x >   1.3 & x <= 2.1   ~ "Moderada",
    x >   2.1 & x <= 3.5   ~ "Fuerte",
    x >   3.5              ~ "Extraordinaria",
    TRUE                   ~ NA_character_
  )

  .make_result(phase3, magnitud)
}

# =========================
# ICEN: tablas de eventos (cronología)
# =========================

#' Tablas de eventos ICEN (Niño/Niña) según cronología (pegada por el usuario)
#' @return lista con tibble nino y nina
#' @export
icen_event_tables <- function() {

  nino <- tibble::tibble(
    y0 = c(1951,1953,1957,1963,1965,1968,1969,1972,1976,1982,1986,1991,1993,1994,1997,2002,2006,2008,2009,2012,2014,2015,2017,2018,2023),
    m0 = c(   5,   2,   3,   8,   3,   9,   3,   2,   4,   7,  12,   8,   3,  11,   4,   3,   8,   6,   6,   4,   5,   4,   1,  11,   3),
    y1 = c(1951,1953,1958,1963,1966,1968,1970,1973,1976,1983,1987,1992,1993,1995,1998,2002,2006,2008,2009,2012,2014,2016,2017,2019,2024),
    m1 = c(  12,  10,   7,  10,   1,  11,   1,   2,  12,  11,  12,   6,   9,   1,   8,   5,  12,   9,   9,   7,  11,   5,   4,   1,   2),
    magnitud = c("Fuerte","Moderado","Fuerte",.debil,"Moderado",.debil,"Moderado","Fuerte","Moderado",
                 "Extraordinario","Moderado","Moderado",.debil,.debil,"Extraordinario",
                 .debil,.debil,.debil,.debil,.debil,.debil,"Fuerte","Moderado",.debil,"Fuerte")
  ) |>
    dplyr::mutate(
      phase3 = .nino,
      magnitud = .norm_mag_icen(magnitud),
      idx0 = .to_index(y0, m0),
      idx1 = .to_index(y1, m1)
    ) |>
    dplyr::select(phase3, magnitud, y0, m0, y1, m1, idx0, idx1)

  nina <- tibble::tibble(
    y0 = c(1950,1954,1955,1962,1964,1966,1967,1970,1970,1971,1973,1974,1975,1985,1986,1988,1994,1995,1996,1999,2001,2003,2004,2005,2007,2010,2013,2017,2020,2021,2024),
    m0 = c(   1,   1,   3,   3,   3,   4,   8,   4,  12,   5,   5,  11,   7,   3,   4,   4,   3,   4,   3,  10,   7,   5,   4,   9,   4,   7,   3,  10,   6,  10,   5),
    y1 = c(1950,1954,1956,1962,1964,1966,1968,1970,1971,1971,1974,1975,1976,1985,1986,1988,1994,1995,1997,1999,2001,2003,2004,2005,2007,2010,2013,2018,2021,2022,2024),
    m1 = c(   7,  12,   1,   5,   9,   7,   6,  10,   3,  10,   1,   1,   1,   9,   6,  11,   7,   6,   1,  12,  12,   7,   7,  12,  12,  12,   9,   5,   5,  11,   7),
    magnitud = c("Moderado","Fuerte","Moderado","Fuerte","Fuerte",.debil,"Moderado","Fuerte",.debil,.debil,
                 .debil,.debil,"Fuerte","Moderado",.debil,"Fuerte",.debil,"Moderado","Fuerte",.debil,
                 .debil,"Moderado","Moderado","Moderado","Fuerte","Moderado","Fuerte","Fuerte","Moderado","Fuerte",.debil)
  ) |>
    dplyr::mutate(
      phase3 = .nina,
      magnitud = .norm_mag_icen(magnitud),
      idx0 = .to_index(y0, m0),
      idx1 = .to_index(y1, m1)
    ) |>
    dplyr::select(phase3, magnitud, y0, m0, y1, m1, idx0, idx1)

  list(nino = nino, nina = nina)
}

#' Clasificar muchos (year,month) por TABLAS de eventos (vectorizada)
#' @param year vector int
#' @param month vector int (1..12)
#' @return tibble phase3, magnitud, escenario (mismo largo)
#' @export
icen_classify_event_vec <- function(year, month) {

  year  <- as.integer(year)
  month <- as.integer(month)

  if (length(year) != length(month)) stop("year y month deben tener el mismo largo")
  if (any(is.na(year)) || any(is.na(month)) || any(month < 1 | month > 12)) {
    stop("month debe estar en 1..12 y year/month no deben ser NA")
  }

  q <- .to_index(year, month)
  tabs <- icen_event_tables()

  # defaults: neutro
  out_phase3 <- rep(.neutro, length(q))
  out_mag    <- rep("Neutra", length(q))

  # función interna para aplicar hits (resuelve por prioridad explícita)
  apply_hits <- function(tab, phase_label) {
    hit_any <- rep(FALSE, length(q))
    mag_hit <- rep(NA_character_, length(q))

    # para cada intervalo, marca hits
    for (i in seq_len(nrow(tab))) {
      hit <- q >= tab$idx0[i] & q <= tab$idx1[i]
      if (any(hit)) {
        hit_any[hit] <- TRUE
        mag_hit[hit] <- tab$magnitud[i]
      }
    }

    list(any = hit_any, mag = mag_hit, phase = phase_label)
  }

  h_nino <- apply_hits(tabs$nino, .nino)
  h_nina <- apply_hits(tabs$nina, .nina)

  # detectar solapamiento (no debería ocurrir; si ocurre: NA para forzar revisión)
  overlap <- h_nino$any & h_nina$any
  if (any(overlap)) {
    out_phase3[overlap] <- NA_character_
    out_mag[overlap]    <- NA_character_
  }

  # asignar Niño (solo donde no hay solapamiento)
  idx_nino <- h_nino$any & !overlap
  out_phase3[idx_nino] <- .nino
  out_mag[idx_nino]    <- h_nino$mag[idx_nino]

  # asignar Niña (solo donde no hay solapamiento)
  idx_nina <- h_nina$any & !overlap
  out_phase3[idx_nina] <- .nina
  out_mag[idx_nina]    <- h_nina$mag[idx_nina]

  .make_result(out_phase3, out_mag)
}

#' Clasificar ICEN por tabla de eventos (año, mes)
#' @param year entero (año).
#' @param month entero 1..12.
#' @return tibble con phase3, magnitud y escenario.
#' @export
icen_classify_event <- function(year, month) {
  icen_classify_event_vec(year, month)
}

#' Añadir clasificación ICEN por eventos (tablas) a un data.frame
#' @param df data.frame con columnas de año y mes
#' @param year_col nombre de columna año
#' @param month_col nombre de columna mes
#' @return df con phase3_event, magnitud_event, escenario_event
#' @export
add_icen_event_classification <- function(df, year_col = "year", month_col = "month") {
  stopifnot(year_col %in% names(df), month_col %in% names(df))

  cl <- icen_classify_event_vec(df[[year_col]], df[[month_col]])

  df$phase3_event    <- cl$phase3
  df$magnitud_event  <- cl$magnitud
  df$escenario_event <- cl$escenario
  df
}

#' Clasificar ONI (fría/neutra/cálida)
#' @param x valor numérico del ONI.
#' @param lo umbral inferior de neutralidad.
#' @param hi umbral superior de neutralidad.
#' @return tibble con phase3, magnitud y escenario.
#' @export
oni_classify_value <- function(x, lo = -0.5, hi = 0.5) {
  x <- as.numeric(x)

  phase3 <- dplyr::case_when(
    x <= lo ~ .nina,
    x >= hi ~ .nino,
    TRUE    ~ .neutro
  )

  magnitud <- dplyr::case_when(
    x <= lo ~ "Condición fría",
    x >= hi ~ "Condición cálida",
    TRUE    ~ "Neutra"
  )

  escenario <- dplyr::if_else(phase3 == .neutro, .neutro, magnitud)
  tibble::tibble(phase3 = phase3, magnitud = magnitud, escenario = escenario)
}

#' Clasificar LABCOS por bins
#' @param x valor numérico del índice.
#' @return tibble con phase3, magnitud y escenario.
#' @export
labcos_classify_value <- function(x) {
  x <- as.numeric(x)

  magnitud <- dplyr::case_when(
    x >  2.77              ~ "Cálida muy fuerte",
    x >  1.37 & x <= 2.77  ~ "Cálida fuerte",
    x >  0.78 & x <= 1.37  ~ "Cálida moderada",
    x >  0.27 & x <= 0.78  ~ "Cálida débil",
    x >= -0.78 & x <= 0.27 ~ "Neutra",
    x >= -1.10 & x < -0.78 ~ "Fría débil",
    x >= -1.31 & x < -1.10 ~ "Fría moderada",
    x <  -1.31             ~ "Fría fuerte",
    TRUE ~ NA_character_
  )

  phase3 <- dplyr::case_when(
    x >  0.27 ~ .nino,
    x < -0.78 ~ .nina,
    TRUE      ~ .neutro
  )

  escenario <- dplyr::if_else(phase3 == .neutro, .neutro, magnitud)
  tibble::tibble(phase3 = phase3, magnitud = magnitud, escenario = escenario)
}

#' Clasificar por signo (Positivo/Negativo/Neutro)
#' @param x valor numérico.
#' @param eps tolerancia para declarar neutro.
#' @return tibble con phase3, magnitud y escenario.
#' @export
sign_classify_value <- function(x, eps = 0) {
  x <- as.numeric(x)
  phase3 <- dplyr::case_when(
    x >  eps ~ "Positivo",
    x < -eps ~ "Negativo",
    TRUE     ~ .neutro
  )
  tibble::tibble(phase3 = phase3, magnitud = NA_character_, escenario = phase3)
}

# =========================
# Router genérico por "spec"
# =========================

#' Clasificar un índice por nombre usando index_specs()
#' @param index nombre del índice (p.ej. "ICEN", "ONI", "LABCOS").
#' @param value valor numérico del índice.
#' @return tibble con phase3, magnitud y escenario.
#' @export
classify_value <- function(index, value) {
  spec <- index_specs() |> dplyr::filter(.data$index == !!index)
  if (nrow(spec) == 0) {
    return(tibble::tibble(phase3 = NA_character_, magnitud = NA_character_, escenario = NA_character_))
  }

  scheme <- spec$scheme[[1]]

  if (scheme == "icen_value")  return(icen_classify_value(value))
  if (scheme == "oni_3cat")    return(oni_classify_value(value, lo = spec$neutral_lo[[1]], hi = spec$neutral_hi[[1]]))
  if (scheme == "labcos_bins") return(labcos_classify_value(value))
  if (scheme == "sign")        return(sign_classify_value(value))

  tibble::tibble(phase3 = NA_character_, magnitud = NA_character_, escenario = NA_character_)
}

#' Añadir clasificación a un data.frame tidy
#' @param df data.frame con columnas de índice y valor.
#' @param index_col nombre de la columna que identifica el índice.
#' @param value_col nombre de la columna con el valor numérico del índice.
#' @return data.frame con columnas phase3, magnitud y escenario.
#' @export
add_classification <- function(df, index_col = "index", value_col = "value") {
  df |>
    dplyr::rowwise() |>
    dplyr::mutate(.tmp = list(classify_value(.data[[index_col]], .data[[value_col]]))) |>
    dplyr::mutate(
      phase3   = .tmp$phase3,
      magnitud = .tmp$magnitud,
      escenario = .tmp$escenario
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-.tmp)
}
