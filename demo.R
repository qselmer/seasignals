library(seasignals)

# 1) Clasificar ICEN por valor (fase + magnitud)
icen_classify_value(c(-1.2, -0.2, 0.8, 3.8))

# 2) Clasificar por tabla de eventos (año, mes)
icen_classify_event(2016, 2)  # ejemplo
icen_classify_event_vec(c(2015, 2015, 2016), c(5, 12, 2))

# 3) Enriquecer un data.frame con clasificación (tidy)
df <- data.frame(
  index = c("ONI","ONI","ICEN","ICEN"),
  value = c(-0.8, 0.2, -1.1, 0.6)
)

add_classification(df, index_col = "index", value_col = "value")

devtools::build_vignettes()
