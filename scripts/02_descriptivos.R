# Proyecto:
# Recuperación económica municipal post-pandemia en la Provincia de Buenos Aires
#
# Script:
# 02_descriptivos.R
#
# Objetivo:
# Elaborar el análisis descriptivo de la base maestra: estadísticas
# resumen, percentiles, datos faltantes, detección de outliers (criterio
# IQR), matriz de correlaciones y gráficos exploratorios de la variación
# del PBG y del PBG per cápita municipal.
#
# Entrada:
# input/base_municipios.csv
#
# Salida:
# output/tablas/estadisticas_descriptivas.csv
# output/tablas/percentiles.csv
# output/tablas/datos_faltantes.csv
# output/tablas/outliers_iqr.csv
# output/tablas/correlaciones.csv
# output/graficos/ (5 gráficos exploratorios)
#
# Grupo 16
# Ciencia de Datos para Economía y Negocios
# FCE - UBA
# =============================================================================

# =============================================================================
# LIBRERÍAS Y CONFIGURACIÓN
# =============================================================================

library(here)

# Cargamos entorno forzando a que las variables vayan al entorno global
source(
  here(
    "scripts", 
    "00_configuracion.R"
  ),
  local = FALSE
)

library(tidyverse)
library(scales)

theme_set(
  
  theme_minimal(base_size = 12) +
    
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray40"),
      plot.caption = element_text(color = "gray50"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
)

message("--------------------------------------------")
message("SCRIPT 02 - ANÁLISIS DESCRIPTIVO")
message("--------------------------------------------")

# =============================================================================
# CARGA DE LA BASE
# =============================================================================

archivo_base <- file.path(
  INPUT_DIR,
  "base_municipios.csv"
)

if (!file.exists(archivo_base)) {
  
  stop(
    paste(
      "No se encontró:",
      archivo_base,
      "\nEjecute primero 01_limpieza.R"
    )
  )
  
}

base <- read_csv(
  archivo_base,
  show_col_types = FALSE
)

# =============================================================================
# ADAPTACIÓN DE NOMBRES DE VARIABLES
# =============================================================================
# El script original fue escrito contra nombres provisorios. Lo dejamos
# funcionando igual creando alias hacia las variables reales que produce
# 01_limpieza.R (PBG2021/PBG2022/PBG_per_capita/Variacion_*).
base <- base |> 
  mutate(
    variacion_pbg  = Variacion_Total,   # Variación real 2021-2022
    pbg_per_capita = PBG_per_capita     # PBG per cápita 2022
  )

message("Base cargada y variables adaptadas correctamente.")

message(
  paste(
    "Municipios:",
    nrow(base)
  )
)

message(
  paste(
    "Variables:",
    ncol(base)
  )
)

# =============================================================================
# VARIABLES NUMÉRICAS
# =============================================================================

variables_numericas <- base |>
  select(where(is.numeric))

# =============================================================================
# ESTADÍSTICAS DESCRIPTIVAS
# =============================================================================

estadisticas <- variables_numericas |> 
  pivot_longer(
    cols = everything(), 
    names_to = "Variable", 
    values_to = "valor"
  ) |> 
  group_by(Variable) |> 
  summarise(
    
    Media = mean(valor, na.rm = TRUE),
    Mediana = median(valor, na.rm = TRUE),
    Desvio_Estandar = sd(valor, na.rm = TRUE),
    Minimo = min(valor, na.rm = TRUE),
    
    Percentil_25 = quantile(valor, 0.25, na.rm = TRUE),
    Percentil_75 = quantile(valor, 0.75, na.rm = TRUE),
    Maximo = max(valor, na.rm = TRUE),
    IQR = IQR(valor, na.rm = TRUE),
    
    Coeficiente_Variacion = sd(valor, na.rm = TRUE) / 
      mean(valor, na.rm = TRUE)
    
  )

write_csv(
  
  estadisticas,
  
  file.path(
    OUTPUT_DIR,
    "tablas",
    "estadisticas_descriptivas.csv"
  )
  
)

message("Tabla de estadísticas descriptivas exportada.")

# =============================================================================
# TABLA DE PERCENTILES
# =============================================================================

percentiles <- variables_numericas |> 
  pivot_longer(
    cols = everything(), 
    names_to = "Variable", 
    values_to = "valor"
  ) |> 
  group_by(Variable) |> 
  summarise(
    P5  = quantile(valor, 0.05, na.rm = TRUE),
    P10 = quantile(valor, 0.10, na.rm = TRUE),
    P25 = quantile(valor, 0.25, na.rm = TRUE),
    P50 = quantile(valor, 0.50, na.rm = TRUE),
    P75 = quantile(valor, 0.75, na.rm = TRUE),
    P90 = quantile(valor, 0.90, na.rm = TRUE),
    P95 = quantile(valor, 0.95, na.rm = TRUE)
  )

write_csv(
  
  percentiles,
  
  file.path(
    OUTPUT_DIR,
    "tablas",
    "percentiles.csv"
  )
  
)

message("Tabla de percentiles exportada.")

# =============================================================================
# DATOS FALTANTES
# =============================================================================

faltantes <- base |> 
  summarise(
    across(
      everything(), 
      list(
        Cant = ~sum(is.na(.x)),
        Porc = ~round(mean(is.na(.x)) * 100, 2)
      )
    )
  ) |> 
  pivot_longer(
    everything(), 
    names_to = c("Variable", ".value"), 
    names_pattern = "(.*)_(Cant|Porc)"
  ) |> 
  rename(
    Cantidad_NA = Cant,
    Porcentaje_NA = Porc
  )

write_csv(
  
  faltantes,
  
  file.path(
    OUTPUT_DIR,
    "tablas",
    "datos_faltantes.csv"
  )
  
)

message("Tabla de datos faltantes exportada.")

# =============================================================================
# DETECCIÓN DE OUTLIERS (CRITERIO IQR)
# =============================================================================

detectar_outliers <- function(vector){
  
  q1 <- quantile(vector, 0.25, na.rm = TRUE)
  q3 <- quantile(vector, 0.75, na.rm = TRUE)
  
  iqr <- q3 - q1
  
  limite_inferior <- q1 - 1.5 * iqr
  limite_superior <- q3 + 1.5 * iqr
  
  sum(
    vector < limite_inferior |
      vector > limite_superior,
    na.rm = TRUE
  )
  
}

outliers <- variables_numericas |> 
  summarise(
    across(
      everything(), 
      detectar_outliers
    )
  ) |> 
  pivot_longer(
    everything(), 
    names_to = "Variable", 
    values_to = "Cantidad_Outliers"
  )

write_csv(
  
  outliers,
  
  file.path(
    OUTPUT_DIR,
    "tablas",
    "outliers_iqr.csv"
  )
  
)

message("Tabla de outliers exportada.")

# =============================================================================
# MATRIZ DE CORRELACIONES
# =============================================================================

correlacion_matriz <- cor(
  variables_numericas,
  use = "pairwise.complete.obs"
)

df_correlacion <- as.data.frame(correlacion_matriz) |> 
  rownames_to_column(var = "Variable") |> 
  mutate(
    across(
      where(is.numeric), 
      ~round(.x, 3)
    )
  )

write_csv(
  
  df_correlacion,
  
  file.path(
    OUTPUT_DIR,
    "tablas",
    "correlaciones.csv"
  )
  
)

message("Matriz de correlaciones exportada.")

# =============================================================================
# RESUMEN AUTOMÁTICO
# =============================================================================

cat(
  
  "\n",
  "============================================\n",
  "ANÁLISIS DESCRIPTIVO\n",
  "============================================\n",
  "\n",
  "Variables analizadas:",
  ncol(variables_numericas),
  "\n",
  
  "Variables con NA:",
  sum(faltantes$Cantidad_NA > 0),
  "\n",
  
  "Variables con outliers:",
  sum(outliers$Cantidad_Outliers > 0),
  "\n\n"
  
)

# =============================================================================
# HISTOGRAMA - VARIACIÓN DEL PBG
# =============================================================================

grafico_hist <- ggplot(
  base,
  aes(x = variacion_pbg)
) +
  
  geom_histogram(
    bins = 20,
    fill = colores_proyecto["Principal"],
    color = "white"
  ) +
  
  geom_vline(
    xintercept = mean(base$variacion_pbg, na.rm = TRUE),
    color = colores_proyecto["Negativo"],
    linetype = "dashed",
    linewidth = 0.9
  ) +
  
  scale_x_continuous(
    labels = label_percent(scale = 1, accuracy = 1)
  ) +
  
  labs(
    
    title = "Distribución de la variación real del PBG municipal",
    subtitle = "Municipios de la provincia de Buenos Aires (2021-2022)",
    x = "Variación porcentual del PBG",
    y = "Cantidad de municipios",
    caption = "Fuente: Elaboración propia en base al PBG Municipal (DPE - PBA). La línea punteada marca el promedio."
    
  )

ggsave(
  
  filename = file.path(
    OUTPUT_DIR,
    "graficos",
    "hist_variacion_pbg.png"
  ),
  
  plot = grafico_hist,
  
  width = 8,
  height = 5,
  dpi = 300
  
)

# =============================================================================
# BOXPLOT - VARIACIÓN DEL PBG
# =============================================================================

grafico_box <- ggplot(
  
  base,
  
  aes(
    x = "",
    y = variacion_pbg
  )
  
) +
  
  geom_boxplot(
    
    fill = colores_proyecto["Principal"],
    alpha = .75,
    width = .30,
    outlier.color = colores_proyecto["Negativo"]
    
  ) +
  
  geom_jitter(
    
    width = .10,
    alpha = .50,
    color = "gray35"
    
  ) +
  
  scale_y_continuous(
    labels = label_percent(scale = 1, accuracy = 1)
  ) +
  
  labs(
    
    title = "Variación real del PBG por municipio",
    subtitle = "Detección visual de posibles valores atípicos",
    x = NULL,
    y = "Variación porcentual",
    caption = "Fuente: Elaboración propia."
    
  )

ggsave(
  
  file.path(
    OUTPUT_DIR,
    "graficos",
    "box_variacion_pbg.png"
  ),
  
  grafico_box,
  
  width = 7,
  height = 5,
  dpi = 300
  
)

# =============================================================================
# ECDF
# =============================================================================

grafico_ecdf <- ggplot(
  
  base,
  
  aes(
    variacion_pbg
  )
  
) +
  
  stat_ecdf(
    linewidth = 1,
    color = colores_proyecto["Principal"]
  ) +
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "gray50"
  ) +
  
  scale_x_continuous(
    labels = label_percent(scale = 1, accuracy = 1)
  ) +
  
  scale_y_continuous(
    labels = percent
  ) +
  
  labs(
    
    title = "Distribución acumulada empírica",
    subtitle = "Variación porcentual del PBG municipal",
    x = "Variación porcentual",
    y = "F(x)",
    caption = "Fuente: Elaboración propia."
    
  )

ggsave(
  
  file.path(
    OUTPUT_DIR,
    "graficos",
    "ecdf_variacion_pbg.png"
  ),
  
  grafico_ecdf,
  
  width = 7,
  height = 5,
  dpi = 300
  
)

# =============================================================================
# HISTOGRAMA - PBG PER CÁPITA
# =============================================================================

grafico_hist_pc <- ggplot(
  
  base,
  
  aes(
    pbg_per_capita
  )
  
) +
  
  geom_histogram(
    
    bins = 20,
    fill = colores_proyecto["Secundario"],
    color = "white"
    
  ) +
  
  scale_x_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ",", prefix = "$")
  ) +
  
  labs(
    
    title = "Distribución del PBG per cápita",
    subtitle = "Municipios de la provincia de Buenos Aires (2022, pesos constantes)",
    x = "PBG per cápita",
    y = "Cantidad de municipios",
    caption = "Fuente: Elaboración propia."
    
  )

ggsave(
  
  file.path(
    OUTPUT_DIR,
    "graficos",
    "hist_pbg_per_capita.png"
  ),
  
  grafico_hist_pc,
  
  width = 8,
  height = 5,
  dpi = 300
  
)

# =============================================================================
# BOXPLOT - PBG PER CÁPITA
# =============================================================================

grafico_box_pc <- ggplot(
  
  base,
  
  aes(
    x = "",
    y = pbg_per_capita
  )
  
) +
  
  geom_boxplot(
    
    fill = colores_proyecto["Secundario"],
    alpha = .70,
    outlier.color = colores_proyecto["Negativo"]
    
  ) +
  
  geom_jitter(
    
    width = .12,
    alpha = .50
    
  ) +
  
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ",", prefix = "$")
  ) +
  
  labs(
    
    title = "PBG per cápita municipal",
    subtitle = "Distribución y valores extremos (2022, pesos constantes)",
    x = NULL,
    y = "PBG per cápita",
    caption = "Fuente: Elaboración propia."
    
  )

ggsave(
  
  file.path(
    OUTPUT_DIR,
    "graficos",
    "box_pbg_per_capita.png"
  ),
  
  grafico_box_pc,
  
  width = 7,
  height = 5,
  dpi = 300
  
)

# =============================================================================
# SCATTER
# =============================================================================

grafico_scatter <- ggplot(
  
  base,
  
  aes(
    poblacion,
    pbg_per_capita
  )
  
) +
  
  geom_point(
    
    color = colores_proyecto["Principal"],
    alpha = .70,
    size = 3
    
  ) +
  
  geom_smooth(
    
    method = "loess",
    se = FALSE,
    color = colores_proyecto["Destacado"]
    
  ) +
  
  scale_x_log10(
    labels = label_number(big.mark = ".")
  ) +
  
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ",", prefix = "$")
  ) +
  
  labs(
    
    title = "PBG per cápita y tamaño poblacional",
    subtitle = "Cada punto representa un municipio (escala poblacional logarítmica)",
    x = "Población",
    y = "PBG per cápita",
    caption = "Fuente: Elaboración propia."
    
  )

ggsave(
  
  file.path(
    OUTPUT_DIR,
    "graficos",
    "dispersion_pbg_poblacion.png"
  ),
  
  grafico_scatter,
  
  width = 8,
  height = 5,
  dpi = 300
  
)

# =============================================================================
# FIN DEL SCRIPT
# =============================================================================

message("--------------------------------------------")
message("Script 02_descriptivos.R finalizado.")
message("--------------------------------------------")