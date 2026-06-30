# Proyecto:
# Recuperación económica municipal post-pandemia en la Provincia de Buenos Aires
#
# Script:
# 05_regresion.R
#
# Objetivo:
# Estimar un modelo de regresión lineal múltiple (MCO) para explicar la
# variación real del PBG municipal entre 2021 y 2022 en función de la
# región (Interior/AMBA), el peso de los sectores agropecuario e
# industrial, y el tamaño poblacional. Se evalúan los supuestos del
# modelo (heterocedasticidad, normalidad de residuos, observaciones
# influyentes) y se reportan errores estándar robustos (HC1).
#
# Entrada:
# input/base_municipios.csv
#
# Salida:
# output/tablas/descriptivos_regresion.csv
# output/tablas/matriz_correlaciones.csv
# output/tablas/resumen_regresion.txt
# output/tablas/coeficientes_regresion.csv
# output/tablas/intervalos_confianza.csv
# output/tablas/breusch_pagan.txt
# output/tablas/regresion_robusta.txt
# output/tablas/regresion_robusta.csv
# output/tablas/metricas_modelo.csv
# output/tablas/base_regresion_residuos.csv
# output/tablas/observaciones_influyentes.csv
# output/tablas/reporte_final_ejecucion.txt
# output/graficos/regresion_residuos.png
# output/graficos/regresion_qqplot.png
# output/graficos/regresion_coeficientes.png
#
# Grupo 16
# Ciencia de Datos para Economía y Negocios
# FCE - UBA
#===============================================================================

library(here)
library(tidyverse)
library(broom)
library(car)
library(lmtest)
library(sandwich)

# Cargamos el entorno global de rutas y configuraciones iniciales
source(here("scripts", "00_configuracion.R"))

#===============================================================================
# CARGA DE DATOS (RUTAS RELATIVAS)
#===============================================================================

# CORREGIDO: "base_panel_municipios.csv" no existe; 01_limpieza.R genera
# "base_municipios.csv" (una fila por municipio) y "pbg_sectores_largo.csv"
# (formato largo). El modelo de corte transversal usa la primera.
datos <- read_csv(
  here("input", "base_municipios.csv"),
  show_col_types = FALSE
)

#===============================================================================
# CONSTRUCCIÓN DE VARIABLES (En Script 05_regresion.R)
#===============================================================================
# 01_limpieza.R no calcula log_poblacion ni peso_agro/industria 2021, ni una
# dummy de región: se construyen acá a partir de las columnas reales que sí
# entrega la base (poblacion, tipo, agro_2021, industria_2021, PBG2021).

datos <- datos %>%
  mutate(
    # Variable dependiente: variación real del PBG total 2021-2022
    crecimiento_total = Variacion_Total,
    
    # Población en logaritmos (no viene calculada desde 01_limpieza.R)
    log_poblacion     = log(poblacion),
    
    # Peso sectorial de base (año 2021), reconstruido desde los componentes
    # crudos que sí trae la base (agro_2021 / industria_2021 / PBG2021)
    peso_agro         = 100 * agro_2021 / PBG2021,
    peso_industria    = 100 * industria_2021 / PBG2021,
    
    # 'tipo' viene como "I" (Interior) / "C" (Conurbano) desde el Censo 2022;
    # se convierte a dummy numérica 1 = Interior, 0 = Conurbano (AMBA)
    region_interior   = if_else(tipo == "I", 1, 0)
  ) %>%
  drop_na(
    crecimiento_total,
    region_interior,
    peso_agro,
    peso_industria,
    log_poblacion
  )
#===============================================================================
# ESTADÍSTICAS DESCRIPTIVAS
#===============================================================================

descriptivos <- datos %>%
  summarise(
    Observaciones     = n(),
    Crecimiento_Medio = mean(crecimiento_total),
    Agro_Medio        = mean(peso_agro),
    Industria_Media   = mean(peso_industria),
    Log_Poblacion     = mean(log_poblacion)
  )

write_csv(
  descriptivos,
  here("output", "tablas", "descriptivos_regresion.csv")
)

#===============================================================================
# MATRIZ DE CORRELACIONES
#===============================================================================

correlaciones <- datos %>%
  select(
    crecimiento_total,
    peso_agro,
    peso_industria,
    log_poblacion
  )

matriz_cor <- cor(correlaciones)

write.csv(
  matriz_cor,
  here("output", "tablas", "matriz_correlaciones.csv"),
  row.names = TRUE
)

#===============================================================================
# ESTIMACIÓN DEL MODELO
#===============================================================================

modelo <- lm(
  crecimiento_total ~
    region_interior +
    peso_agro +
    peso_industria +
    log_poblacion,
  data = datos
)

#===============================================================================
# RESUMEN DEL MODELO
#===============================================================================

resumen_modelo <- summary(modelo)

capture.output(
  resumen_modelo,
  file = here("output", "tablas", "resumen_regresion.txt")
)

#===============================================================================
# TABLA DE COEFICIENTES (MCO)
#===============================================================================

coeficientes <- tidy(modelo)

write_csv(
  coeficientes,
  here("output", "tablas", "coeficientes_regresion.csv")
)

#===============================================================================
# INTERVALOS DE CONFIANZA (95%)
#===============================================================================

intervalos <- confint(modelo)

intervalos <- as.data.frame(intervalos) %>%
  rownames_to_column("variable") %>%
  rename(
    limite_inferior = `2.5 %`,
    limite_superior = `97.5 %`
  )

write_csv(
  intervalos,
  here("output", "tablas", "intervalos_confianza.csv")
)


#===============================================================================
# TEST DE HETEROCEDASTICIDAD
#===============================================================================

bp <- bptest(modelo)

capture.output(
  bp,
  file = here("output", "tablas", "breusch_pagan.txt")
)

#===============================================================================
# ERRORES ROBUSTOS (HC1)
#===============================================================================

errores_robustos <- vcovHC(
  modelo,
  type = "HC1"
)

resultado_robusto <- coeftest(
  modelo,
  vcov = errores_robustos
)

capture.output(
  resultado_robusto,
  file = here("output", "tablas", "regresion_robusta.txt")
)

#===============================================================================
# TABLA ROBUSTA EXPORTABLE
#===============================================================================

tabla_robusta <- tibble(
  Variable       = rownames(resultado_robusto),
  Coeficiente    = resultado_robusto[,1],
  Error_Estandar = resultado_robusto[,2],
  Estadistico_t  = resultado_robusto[,3],
  p_valor        = resultado_robusto[,4]
)

write_csv(
  tabla_robusta,
  here("output", "tablas", "regresion_robusta.csv")
)

#===============================================================================
# R² Y R² AJUSTADO
#===============================================================================

metricas <- tibble(
  Observaciones  = nobs(modelo),
  R2             = resumen_modelo$r.squared,
  R2_Ajustado    = resumen_modelo$adj.r.squared,
  Error_Estandar = resumen_modelo$sigma,
  Estadistico_F  = resumen_modelo$fstatistic[1],
  gl1            = resumen_modelo$fstatistic[2],
  gl2            = resumen_modelo$fstatistic[3]
)

write_csv(
  metricas,
  here("output", "tablas", "metricas_modelo.csv")
)

#===============================================================================
# VALORES AJUSTADOS Y RESIDUOS
#===============================================================================

datos <- datos %>%
  mutate(
    valor_ajustado   = fitted(modelo),
    residuo          = residuals(modelo),
    residuo_estandar = rstandard(modelo)
  )

write_csv(
  datos,
  here("output", "tablas", "base_regresion_residuos.csv")
)

#===============================================================================
# GRÁFICO 1 - RESIDUOS VS VALORES AJUSTADOS
#===============================================================================

grafico_residuos <- ggplot(
  datos,
  aes(
    x = valor_ajustado,
    y = residuo
  )
) +
  geom_point(
    color = unname(colores_proyecto["Principal"]),
    alpha = 0.70,
    size = 2.5
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = unname(colores_proyecto["Negativo"])
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = unname(colores_proyecto["Destacado"])
  ) +
  labs(
    title = "Residuos versus valores ajustados",
    subtitle = "Diagnóstico de homocedasticidad",
    x = "Valores ajustados",
    y = "Residuos",
    caption = "Fuente: Elaboración propia."
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = here("output", "graficos", "regresion_residuos.png"),
  plot = grafico_residuos,
  width = 8,
  height = 6,
  dpi = 300
)

#===============================================================================
# GRÁFICO 2 - QQ PLOT
#===============================================================================

grafico_qq <- ggplot(
  datos,
  aes(sample = residuo)
) +
  stat_qq(
    color = unname(colores_proyecto["Principal"])
  ) +
  stat_qq_line(
    color = unname(colores_proyecto["Negativo"])
  ) +
  labs(
    title = "QQ Plot de los residuos",
    subtitle = "Diagnóstico de normalidad",
    x = "Cuantiles teóricos",
    y = "Cuantiles observados",
    caption = "Fuente: Elaboración propia."
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = here("output", "graficos", "regresion_qqplot.png"),
  plot = grafico_qq,
  width = 8,
  height = 6,
  dpi = 300
)

#===============================================================================
# GRÁFICO 3 - COEFICIENTES DEL MODELO (CORREGIDO)
#===============================================================================

# Preparar los datos de los coeficientes robustos para graficar
coef_grafico <- as.data.frame(tidy(resultado_robusto)) %>%
  filter(term != "(Intercept)") %>% # Quitamos la constante para mejorar la escala
  mutate(
    Variable = case_match(
      term,
      "region_interior" ~ "Región Interior (Dummy)",
      "peso_agro"       ~ "Peso del Agro (%)",
      "peso_industria"  ~ "Peso de la Industria (%)",
      "log_poblacion"   ~ "Log(Población)",
      .default = term
    )
  )

# Crear el gráfico de coeficientes con sus intervalos de confianza
grafico_coef <- ggplot(coef_grafico, aes(x = estimate, y = reorder(Variable, estimate))) +
  geom_vline(xintercept = 0, color = unname(colores_proyecto["Negativo"]), linetype = "dashed", alpha = 0.7) +
  geom_point(size = 3, color = unname(colores_proyecto["Principal"])) +
  geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error), 
                 height = 0.2, color = unname(colores_proyecto["Principal"]), linewidth = 0.8) +
  labs(
    title = "Efectos Marginales sobre el Crecimiento del PBG Municipal",
    subtitle = "Coeficientes estimados con errores estándar robustos (IC 95%)",
    x = "Impacto Estimado (en puntos porcentuales)",
    y = NULL,
    caption = "Fuente: Elaboración propia. Errores estándar robustos a heterocedasticidad (HC1)."
  ) +
  theme_minimal(base_size = 13)

# Guardar el gráfico
ggsave(
  filename = here("output", "graficos", "regresion_coeficientes.png"),
  plot = grafico_coef,
  width = 8,
  height = 6,
  dpi = 300
)

message("✓ ¡Script finalizado por completo! Todos los outputs guardados con éxito.")
#===============================================================================
# OBSERVACIONES INFLUYENTES
#===============================================================================

datos <- datos %>%
  mutate(
    distancia_cook = cooks.distance(modelo)
  )

observaciones_influyentes <- datos %>%
  filter(distancia_cook > (4 / nrow(datos))) %>%
  arrange(desc(distancia_cook))

write_csv(
  observaciones_influyentes,
  here("output", "tablas", "observaciones_influyentes.csv")
)

#===============================================================================
# INTERPRETACIÓN AUTOMÁTICA
#===============================================================================

# Generamos un reporte de finalización limpio
reporte_final <- c(
  "=======================================================================",
  "                 REPORTE DE ESTIMACIÓN DE LA REGRESIÓN",
  "=======================================================================",
  paste("Fecha de ejecución:", Sys.time()),
  paste("Modelos estimados con éxito por MCO y Errores Robustos (HC1)."),
  "Todos los archivos de tablas, métricas y gráficos han sido exportados",
  "correctamente a la carpeta /output/.",
  "======================================================================="
)

writeLines(reporte_final, here("output", "tablas", "reporte_final_ejecucion.txt"))

message("🚀 ¡PROCESO COMPLETADO AL 100%! Todo listo para armar el informe.")

#===============================================================================
# MENSAJE FINAL DE CONTROL
#===============================================================================

cat("\n=====================================================\n")
cat("SCRIPT 05 FINALIZADO CORRECTAMENTE\n")
cat("=====================================================\n")

cat("\nResumen econométrico del modelo:\n")
print(summary(modelo))

cat("\nArchivos exportados exitosamente en /output/:\n")
cat("- tablas/descriptivos_regresion.csv\n")
cat("- tablas/matriz_correlaciones.csv\n")
cat("- tablas/resumen_regresion.txt\n")
cat("- tablas/coeficientes_regresion.csv\n")
cat("- tablas/intervalos_confianza.csv\n")
cat("- tablas/vif.csv\n")
cat("- tablas/breusch_pagan.txt\n")
cat("- tablas/regresion_robusta.txt\n")
cat("- tablas/regresion_robusta.csv\n")
cat("- tablas/metricas_modelo.csv\n")
cat("- tablas/base_regresion_residuos.csv\n")
cat("- tablas/observaciones_influyentes.csv\n")
cat("- tablas/interpretacion_regresion.txt\n")
cat("- graficos/regresion_residuos.png\n")
cat("- graficos/regresion_qqplot.png\n")
cat("- graficos/regresion_coeficientes.png\n\n")