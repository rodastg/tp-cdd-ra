# Proyecto:
# Recuperación económica municipal post-pandemia en la Provincia de Buenos Aires
#
# Script:
# 03_test_regiones.R
#
# Objetivo:
# Comparar la recuperación económica entre municipios del Interior y del
# Conurbano/AMBA bonaerense. Se verifican los supuestos de normalidad
# (Shapiro-Wilk) y homogeneidad de varianzas (Levene) antes de seleccionar
# de forma dinámica la prueba de hipótesis apropiada (t de Student o
# Wilcoxon-Mann-Whitney).
#
# Entrada:
# input/base_municipios.csv
#
# Salida:
# output/tablas/estadisticas_region.csv
# output/tablas/shapiro.csv
# output/tablas/levene.csv
# output/tablas/reporte_test_hipotesis.txt
# output/tablas/test_region_resultados.csv
# output/tablas/interpretacion_test_region.txt
# output/graficos/box_region.png
# output/graficos/densidad_region.png
#
# Grupo 16
# Ciencia de Datos para Economía y Negocios
# FCE - UBA
# =============================================================================

# --- 1. Limpieza del Entorno y Carga de Paquetes -----------------------------

rm(list = ls())
gc()

library(here)
library(tidyverse)
library(car)
library(broom)
library(scales)

# Cargamos el entorno global de rutas y configuraciones iniciales
source(here("scripts", "00_configuracion.R"))

message("--------------------------------------------")
message("SCRIPT 03 - TEST ENTRE REGIONES")
message("--------------------------------------------")


# --- 2. Carga y Preparación de los Datos -------------------------------------

base <- read_csv(
  file.path(INPUT_DIR, "base_municipios.csv"),
  show_col_types = FALSE
)

# Limpieza inmunizada contra errores de tipeo o mayúsculas en el CSV original
base <- base |>
  mutate(
    variacion_pbg = Variacion_Total,         # 01_limpieza.R ya entrega esta variable
    region        = tolower(trimws(region)), # Asegura minúsculas y vuela espacios
    region        = case_when(
      region == "amba"     ~ "AMBA",
      region == "interior" ~ "Interior",
      TRUE                 ~ as.character(region)
    ),
    region        = factor(region, levels = c("AMBA", "Interior"))
  )


# --- 3. Estadísticas Descriptivas por Región ---------------------------------

estadisticas_region <- base |>
  group_by(region) |>
  summarise(
    Municipios = n(),
    Media      = mean(variacion_pbg, na.rm = TRUE),
    Mediana    = median(variacion_pbg, na.rm = TRUE),
    Desvio     = sd(variacion_pbg, na.rm = TRUE),
    Minimo     = min(variacion_pbg, na.rm = TRUE),
    Maximo     = max(variacion_pbg, na.rm = TRUE),
    .groups    = "drop"
  )

write_csv(
  estadisticas_region,
  file.path(OUTPUT_DIR, "tablas", "estadisticas_region.csv")
)


# --- 4. Visualizaciones Avanzadas (Con Paleta Institucional) -----------------

# Gráfico A: Boxplot comparativo con Jitter de puntos reales
grafico_box <- ggplot(base, aes(x = region, y = variacion_pbg, fill = region)) +
  geom_boxplot(alpha = 0.75, width = 0.50, outlier.color = colores_proyecto["Negativo"]) +
  geom_jitter(width = 0.12, alpha = 0.50, color = "gray40") +
  scale_fill_manual(values = c("AMBA" = unname(colores_proyecto["Principal"]), "Interior" = unname(colores_proyecto["Secundario"]))) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  labs(
    title    = "Variación del PBG real según región",
    subtitle = "Municipios bonaerenses (2021-2022)",
    x        = NULL,
    y        = "Variación porcentual",
    caption  = "Fuente: Elaboración propia sobre datos del Ministerio de Economía PBA."
  ) +
  theme(legend.position = "none")

ggsave(
  file.path(OUTPUT_DIR, "graficos", "box_region.png"),
  plot = grafico_box, width = 8, height = 5, dpi = 300
)

# Gráfico B: Curvas de Densidad Suavizadas
grafico_densidad <- ggplot(base, aes(x = variacion_pbg, fill = region, color = region)) +
  geom_density(alpha = 0.30, linewidth = 0.9) +
  scale_fill_manual(values = c("AMBA" = unname(colores_proyecto["Principal"]), "Interior" = unname(colores_proyecto["Secundario"]))) +
  scale_color_manual(values = c("AMBA" = unname(colores_proyecto["Principal"]), "Interior" = unname(colores_proyecto["Secundario"]))) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  labs(
    title    = "Distribución de la recuperación económica por región",
    subtitle = "Comparación de curvas de densidad (Interior vs AMBA)",
    x        = "Variación porcentual del PBG",
    y        = "Densidad",
    fill     = NULL, 
    color    = NULL,
    caption  = "Fuente: Elaboración propia."
  )

ggsave(
  file.path(OUTPUT_DIR, "graficos", "densidad_region.png"),
  plot = grafico_densidad, width = 8, height = 5, dpi = 300
)


# --- 5. Validación de Supuestos Estadísticos --------------------------------

# Test de Normalidad de Shapiro-Wilk (evaluado de forma independiente por grupo)
shapiro_region <- base |>
  group_by(region) |>
  summarise(
    Estadistico = shapiro.test(variacion_pbg)$statistic,
    p_valor     = shapiro.test(variacion_pbg)$p.value,
    .groups     = "drop"
  )

write_csv(shapiro_region, file.path(OUTPUT_DIR, "tablas", "shapiro.csv"))

# Aislamos los p-valores para la estructura lógica posterior
p_amba     <- shapiro_region |> filter(region == "AMBA") |> pull(p_valor)
p_interior <- shapiro_region |> filter(region == "Interior") |> pull(p_valor)

# Test de Homogeneidad de Varianzas de Levene
levene_resultado <- leveneTest(variacion_pbg ~ region, data = base)
levene_tabla     <- broom::tidy(levene_resultado)

write_csv(levene_tabla, file.path(OUTPUT_DIR, "tablas", "levene.csv"))

message("Supuestos estadísticos evaluados correctamente.")


# --- 6. Selección Dinámica y Ejecución del Test Inferencial ------------------

# El modelo evalúa la normalidad de ambos grupos simultáneamente (Alpha = 0.05)
usar_t <- (p_amba > 0.05) & (p_interior > 0.05)

if (usar_t) {
  resultado_test <- t.test(variacion_pbg ~ region, data = base, var.equal = FALSE)
  metodo         <- "t de Student (Welch)"
} else {
  resultado_test <- wilcox.test(variacion_pbg ~ region, data = base)
  metodo         <- "Wilcoxon-Mann-Whitney"
}


# --- 7. Exportación de Resultados e Interpretación Textual -------------------

# Generamos el reporte técnico oficial en formato plano (TXT)
sink(file.path(OUTPUT_DIR, "tablas", "reporte_test_hipotesis.txt"))
cat("=====================================================\n")
cat("REPORTE DE TEST DE HIPÓTESIS ENTRE REGIONES\n")
cat("=====================================================\n\n")
cat("Método estadístico seleccionado:", metodo, "\n\n")
print(resultado_test)
sink()

# Estructuramos la tabla sintética de resultados en un dataframe ordenado
tabla_resultado <- tibble(
  metodo         = metodo,
  media_amba     = mean(base$variacion_pbg[base$region == "AMBA"], na.rm = TRUE),
  media_interior = mean(base$variacion_pbg[base$region == "Interior"], na.rm = TRUE),
  diferencia     = media_interior - media_amba,
  estadistico    = unname(resultado_test$statistic),
  p_valor        = resultado_test$p.value
)

write_csv(tabla_resultado, file.path(OUTPUT_DIR, "tablas", "test_region_resultados.csv"))

# Algoritmo de interpretación automática según significatividad
interpretacion <- if (resultado_test$p.value < 0.05) {
  "Se rechaza H0. Existe evidencia estadísticamente significativa de diferencias en la recuperación económica entre el Interior y el AMBA."
} else {
  "No se rechaza H0. No se encontraron diferencias estadísticamente significativas en la recuperación económica entre ambas regiones."
}

writeLines(interpretacion, file.path(OUTPUT_DIR, "tablas", "interpretacion_test_region.txt"))


# --- 8. Finalización del Módulo ----------------------------------------------

cat("\n=====================================================\n")
cat("Script 03_test_regiones.R finalizado correctamente.\n")
cat("=====================================================\n")