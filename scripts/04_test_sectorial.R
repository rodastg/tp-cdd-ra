# Proyecto:
# Recuperación económica municipal post-pandemia en la Provincia de Buenos Aires
#
# Script:
# 04_test_sectorial.R
#
# Objetivo:
# Comparar el crecimiento de los sectores productores de bienes frente al
# crecimiento de los sectores productores de servicios entre 2021 y 2022,
# mediante un test pareado (t de Student o Wilcoxon, según corresponda
# tras evaluar normalidad de las diferencias).
#
# Entrada:
# input/base_municipios.csv
#
# Salida:
# output/tablas/descriptivos_sectorial.csv
# output/tablas/test_normalidad_sectorial.txt
# output/tablas/test_sectorial_completo.txt
# output/tablas/test_sectorial_resultados.csv
# output/tablas/interpretacion_test_sectorial.txt
# output/graficos/boxplot_sectorial.png
# output/graficos/histograma_diferencias_sectoriales.png
#
# Grupo 16
# Ciencia de Datos para Economía y Negocios
# FCE - UBA
#===============================================================================

library(here)
library(tidyverse)

# Cargamos el entorno global de rutas y configuraciones iniciales
source(here("scripts", "00_configuracion.R"))

#===============================================================================
# CARGA DE DATOS (RUTAS RELATIVAS)
#===============================================================================

datos <- read_csv(
  here("input", "base_municipios.csv"), # <-- Corregido el nombre del archivo
  show_col_types = FALSE
)

#===============================================================================
# VARIABLES NECESARIAS
#===============================================================================
# 01_limpieza.R entrega estas tasas como Variacion_Bienes y Variacion_Servicios;
# se mapean a los nombres internos que usa el resto del script para no tener
# que reescribir toda la lógica posterior.

datos <- datos %>%
  mutate(
    crecimiento_bienes = Variacion_Bienes,
    crecimiento_servicios = Variacion_Servicios
  ) %>%
  drop_na(
    crecimiento_bienes,
    crecimiento_servicios
  )
#===============================================================================
# ESTADÍSTICAS DESCRIPTIVAS
#===============================================================================

resumen <- tibble(
  Grupo = c("Bienes", "Servicios"),
  Media = c(
    mean(datos$crecimiento_bienes),
    mean(datos$crecimiento_servicios)
  ),
  Mediana = c(
    median(datos$crecimiento_bienes),
    median(datos$crecimiento_servicios)
  ),
  Desvio = c(
    sd(datos$crecimiento_bienes),
    sd(datos$crecimiento_servicios)
  ),
  Minimo = c(
    min(datos$crecimiento_bienes),
    min(datos$crecimiento_servicios)
  ),
  Maximo = c(
    max(datos$crecimiento_bienes),
    max(datos$crecimiento_servicios)
  )
)

write_csv(
  resumen,
  here("output", "tablas", "descriptivos_sectorial.csv")
)

cat("Municipios analizados:", nrow(datos), "\n")

#===============================================================================
# TEST DE NORMALIDAD
#===============================================================================

datos <- datos %>%
  mutate(
    diferencia = crecimiento_bienes - crecimiento_servicios
  )

normalidad <- shapiro.test(datos$diferencia)

sink(here("output", "tablas", "test_normalidad_sectorial.txt"))

cat("=====================================================\n")
cat("TEST DE SHAPIRO-WILK\n")
cat("Diferencias: Bienes - Servicios\n")
cat("=====================================================\n\n")

print(normalidad)

sink()

#===============================================================================
# SELECCIÓN AUTOMÁTICA DEL TEST
#===============================================================================

usar_t <- normalidad$p.value > 0.05

#===============================================================================
# TEST PAREADO
#===============================================================================

if (usar_t) {
  resultado_test <- t.test(
    datos$crecimiento_bienes,
    datos$crecimiento_servicios,
    paired = TRUE
  )
  metodo <- "t de Student pareado"
} else {
  resultado_test <- wilcox.test(
    datos$crecimiento_bienes,
    datos$crecimiento_servicios,
    paired = TRUE
  )
  metodo <- "Wilcoxon pareado"
}

sink(here("output", "tablas", "test_sectorial_completo.txt"))

cat("=====================================================\n")
cat("TEST PAREADO\n")
cat("=====================================================\n\n")

cat("Método utilizado:\n")
cat(metodo)
cat("\n\n")

print(resultado_test)

sink()

#===============================================================================
# TABLA RESUMEN
#===============================================================================

tabla_resultado <- tibble(
  metodo = metodo,
  media_bienes = mean(datos$crecimiento_bienes),
  media_servicios = mean(datos$crecimiento_servicios),
  diferencia_media = mean(datos$crecimiento_bienes) - mean(datos$crecimiento_servicios),
  estadistico = unname(resultado_test$statistic),
  p_valor = resultado_test$p.value
)

write_csv(
  tabla_resultado,
  here("output", "tablas", "test_sectorial_resultados.csv")
)

#===============================================================================
# BASE LARGA PARA LOS GRÁFICOS (VERSIÓN LIMPIA)
#===============================================================================

datos_largos <- datos %>%
  select(
    municipio,
    crecimiento_bienes,
    crecimiento_servicios
  ) %>%
  pivot_longer(
    cols = c(
      crecimiento_bienes,
      crecimiento_servicios
    ),
    names_to = "sector",
    values_to = "crecimiento"
  ) %>%
  mutate(
    # Usamos un if_else tradicional de R base para evitar advertencias de versiones
    sector = if_else(sector == "crecimiento_bienes", "Bienes", "Servicios")
  )
#===============================================================================
# BOXPLOT COMPARATIVO
#===============================================================================

grafico_box <- ggplot(
  datos_largos,
  aes(
    x = sector,
    y = crecimiento,
    fill = sector
  )
) +
  geom_boxplot(
    alpha = 0.75,
    width = 0.60,
    outlier.alpha = 0.50
  ) +
  geom_jitter(
    width = 0.12,
    alpha = 0.35,
    size = 1.8
  ) +
  scale_fill_manual(
    values = c(
      "Bienes" = unname(colores_proyecto["Principal"]),
      "Servicios" = unname(colores_proyecto["Secundario"])
    )
  ) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1, accuracy = 1)
  ) +
  labs(
    title = "Crecimiento de los sectores productores de bienes y servicios",
    subtitle = "Variación porcentual del PBG real entre 2021 y 2022",
    x = NULL,
    y = "Crecimiento",
    caption = "Fuente: Ministerio de Economía de la Provincia de Buenos Aires."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = here("output", "graficos", "boxplot_sectorial.png"),
  plot = grafico_box,
  width = 9,
  height = 6,
  dpi = 300
)

#===============================================================================
# HISTOGRAMA DE LAS DIFERENCIAS
#===============================================================================

grafico_hist <- ggplot(
  datos,
  aes(diferencia)
) +
  geom_histogram(
    bins = 20,
    fill = unname(colores_proyecto["Principal"]),
    color = "white",
    alpha = 0.85
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = unname(colores_proyecto["Negativo"]),
    linewidth = 0.8
  ) +
  scale_x_continuous(
    labels = scales::label_percent(scale = 1, accuracy = 1)
  ) +
  labs(
    title = "Distribución de las diferencias entre Bienes y Servicios",
    subtitle = "Diferencia = Crecimiento Bienes − Crecimiento Servicios",
    x = "Diferencia porcentual",
    y = "Cantidad de municipios",
    caption = "Fuente: Ministerio de Economía de la Provincia de Buenos Aires."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = here("output", "graficos", "histograma_diferencias_sectoriales.png"),
  plot = grafico_hist,
  width = 9,
  height = 6,
  dpi = 300
)

#===============================================================================
# INTERPRETACIÓN AUTOMÁTICA
#===============================================================================

if (resultado_test$p.value < 0.05) {
  if (mean(datos$crecimiento_bienes) > mean(datos$crecimiento_servicios)) {
    interpretacion <- paste(
      "Se rechaza la hipótesis nula.",
      "Existe evidencia estadísticamente significativa de que",
      "los sectores productores de bienes crecieron más",
      "que los sectores productores de servicios",
      "entre 2021 y 2022."
    )
  } else {
    interpretacion <- paste(
      "Se rechaza la hipótesis nula.",
      "Los sectores productores de servicios",
      "presentaron un crecimiento promedio superior",
      "al de los sectores productores de bienes."
    )
  }
} else {
  interpretacion <- paste(
    "No se rechaza la hipótesis nula.",
    "No se encontraron diferencias estadísticamente",
    "significativas entre ambos bloques sectoriales."
  )
}

writeLines(
  interpretacion,
  here("output", "tablas", "interpretacion_test_sectorial.txt")
)

#===============================================================================
# MENSAJE FINAL
#===============================================================================

cat("\n=====================================================\n")
cat("SCRIPT 04 FINALIZADO CORRECTAMENTE\n")
cat("=====================================================\n")

cat("\nArchivos generados:\n")

cat("- output/tablas/descriptivos_sectorial.csv\n")
cat("- output/tablas/test_normalidad_sectorial.txt\n")
cat("- output/tablas/test_sectorial_completo.txt\n")
cat("- output/tablas/test_sectorial_resultados.csv\n")
cat("- output/tablas/interpretacion_test_sectorial.txt\n")
cat("- output/graficos/boxplot_sectorial.png\n")
cat("- output/graficos/histograma_diferencias_sectoriales.png\n")