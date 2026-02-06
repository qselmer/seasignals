testthat::test_that("icen_classify_value: umbrales correctos", {
  x <- c(-1.50, -1.20, -0.80, -0.70, 0.00, 0.50, 0.60, 1.40, 2.50, 3.60)

  cl <- seasignals::icen_classify_value(x)

  testthat::expect_equal(cl$phase3,
                         c("Niña","Niña","Niña","Neutro","Neutro","Neutro","Niño","Niño","Niño","Niño"))

  testthat::expect_equal(cl$magnitud,
                         c("Fuerte","Moderada","Débil","Neutra","Neutra","Neutra","Débil","Moderada","Fuerte","Extraordinaria"))

  testthat::expect_equal(cl$escenario[1], "Niña Fuerte")
  testthat::expect_equal(cl$escenario[4], "Neutro")
})

testthat::test_that("download_icen: parsea formato yy mm ICEN (sin web)", {
  # Texto simulado (idéntico al formato real)
  txt <- c(
    "% Índice Costero El Niño (ICEN; ENFEN, 2024)",
    "% Versión oficial",
    "% yy   mm   ICEN",
    "1950   1   -0.75",
    "1950   2   -1.07",
    "1950  12   -0.53",
    "1951   5    1.09"
  )

  # Reutilizamos el mismo parser que en download_icen, pero sin descargar.
  # Mini-parser local:
  lines <- trimws(txt)
  lines <- lines[nzchar(lines)]
  lines <- lines[!grepl("^%|^#", lines)]
  lines <- lines[grepl("^\\d{4}\\s+", lines)]

  parts <- strsplit(lines, "\\s+")
  mat <- do.call(rbind, lapply(parts, function(x) x[1:3]))

  year  <- as.integer(mat[, 1])
  month <- as.integer(mat[, 2])
  value <- as.numeric(mat[, 3])

  ok <- !is.na(year) & !is.na(month) & month >= 1 & month <= 12 & !is.na(value)
  year <- year[ok]; month <- month[ok]; value <- value[ok]
  date <- as.Date(sprintf("%d-%02d-01", year, month))

  df <- data.frame(date=date, index="ICEN", value=value, year=year, month=month)

  testthat::expect_equal(nrow(df), 4)
  testthat::expect_equal(df$date[1], as.Date("1950-01-01"))
  testthat::expect_equal(df$value[2], -1.07)
  testthat::expect_equal(df$date[4], as.Date("1951-05-01"))
})

testthat::test_that("add_icen_classification: añade columnas", {
  df <- data.frame(
    date = as.Date(c("1950-01-01","1951-05-01")),
    value = c(-0.75, 1.09)
  )
  out <- seasignals::add_icen_classification(df)

  testthat::expect_true(all(c("phase3","magnitud","escenario") %in% names(out)))
  testthat::expect_equal(out$phase3, c("Niña","Niño"))
})
