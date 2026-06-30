#===============================================================================
# TRABAJO PRÁCTICO INTEGRADOR
# Ciencia de Datos para Economía y Negocios - FCE UBA
#
# Script: 06_graficos.R
#===============================================================================

# Limpieza del entorno
rm(list = ls())

# Carga de librerías
library(tidyverse)
library(ggrepel)
library(scales)
library(viridis)
library(here)
library(ggridges)

#===============================================================================
# 1. CARGA Y PREPARACIÓN DE DATOS
#===============================================================================

datos <- read_csv(
  here("input", "base_municipios.csv"),
  show_col_types = FALSE
)

#===============================================================================
# 1.1 ADAPTACIÓN DE NOMBRES DE VARIABLES
#===============================================================================
# El resto del script fue escrito contra nombres provisorios
# (pbg_per_capita_2022, crecimiento_pbg). Se crean como alias de las
# variables reales que entrega 01_limpieza.R para no reescribir cada gráfico.
datos <- datos %>%
  mutate(
    pbg_per_capita_2022 = PBG_per_capita,
    crecimiento_pbg      = Variacion_Total
  )

# Verificación de paquetes específicos de este script (no están en
# 00_configuracion.R): si faltan, se informa con un mensaje claro en vez
# de un error críptico de "could not find function".
paquetes_graficos <- c("viridis", "ggridges")
faltantes_graficos <- paquetes_graficos[
  !paquetes_graficos %in% installed.packages()[, "Package"]
]
if (length(faltantes_graficos) > 0) {
  stop(
    paste0(
      "\nFaltan instalar los siguientes paquetes para 06_graficos.R:\n\n",
      paste(faltantes_graficos, collapse = "\n"),
      "\n\nInstalalos con install.packages(c(\"viridis\", \"ggridges\"))\n"
    )
  )
}

# Configuración del tema global para gráficos
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray30"),
      plot.caption = element_text(color = "gray40", size = 9),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
)

#===============================================================================
# 2. SCATTERPLOT (PBG Per Cápita vs Crecimiento)
#===============================================================================

datos_limpios <- datos %>% drop_na(pbg_per_capita_2022, crecimiento_pbg)

grafico_scatter <- ggplot(datos_limpios, aes(x = pbg_per_capita_2022, y = crecimiento_pbg)) + 
  geom_point(
    aes(color = region, size = poblacion),
    alpha = 0.75
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 0.8
  ) +
  geom_text_repel(
    data = datos_limpios %>% slice_max(abs(crecimiento_pbg), n = 10),
    aes(label = municipio),
    size = 3,
    max.overlaps = 20
  ) +
  scale_size_continuous(range = c(2, 10), guide = "none") +
  scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "PBG per cápita y recuperación económica",
    subtitle = "Cada punto representa un municipio de la Provincia de Buenos Aires",
    x = "PBG per cápita 2022 (pesos constantes)",
    y = "Crecimiento del PBG (%)",
    color = "Región:",
    caption = "Fuente: Elaboración propia en base a Ministerio de Economía PBA."
  )

ggsave(
  here("output", "graficos", "scatter_pbg_percapita.png"),
  grafico_scatter, width = 9, height = 6, dpi = 300
)

#===============================================================================
# 3. RANKING DE CRECIMIENTO (Top 15)
#===============================================================================

ranking <- datos %>%
  arrange(desc(crecimiento_pbg)) %>%
  slice_head(n = 15) %>%
  mutate(municipio = forcats::fct_reorder(municipio, crecimiento_pbg))

grafico_ranking <- ggplot(ranking, aes(x = crecimiento_pbg, y = municipio)) +
  geom_col(fill = "#0072B2") +
  geom_text(
    aes(label = paste0(round(crecimiento_pbg, 1), "%")),
    hjust = -0.15, size = 3.2
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Municipios con mayor recuperación económica",
    subtitle = "Top 15 según crecimiento del PBG real (2021-2022)",
    x = "Crecimiento (%)",
    y = NULL,
    caption = "Fuente: Ministerio de Economía de la Provincia de Buenos Aires."
  )

ggsave(
  here("output", "graficos", "top15_crecimiento.png"),
  grafico_ranking, width = 9, height = 7, dpi = 300
)

#===============================================================================
# 4. DISTRIBUCIÓN DEL CRECIMIENTO POR REGIÓN (Ridgeline Plot)
#===============================================================================

datos_distribucion <- datos %>% drop_na(crecimiento_pbg, region)

grafico_distribucion <- ggplot(datos_distribucion, aes(x = crecimiento_pbg, y = region, fill = region)) +
  geom_density_ridges(
    alpha = 0.85, 
    scale = 1.2, 
    rel_min_height = 0.01,
    color = "white"
  ) +
  scale_fill_viridis_d(option = "plasma", guide = "none") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  labs(
    title = "Distribución del crecimiento económico por región",
    subtitle = "Variación del PBG real (2021-2022) según densidad de municipios",
    x = "Crecimiento del PBG (%)",
    y = NULL,
    caption = "Fuente: Elaboración propia en base a Ministerio de Economía PBA."
  )

ggsave(
  here("output", "graficos", "07_distribucion_regional.png"),
  grafico_distribucion, width = 9, height = 6, dpi = 300
)

#===============================================================================
# FINALIZACIÓN
#===============================================================================

cat("\n----------------------------------------------------------\n")
cat("Proceso finalizado. Los gráficos han sido guardados en 'output/graficos/'.\n")
cat("----------------------------------------------------------\n")