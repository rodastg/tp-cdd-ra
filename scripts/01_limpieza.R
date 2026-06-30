# Proyecto:
# Recuperación económica municipal post-pandemia en la Provincia de Buenos Aires
#
# Script:
# 01_limpieza.R
#
# Objetivo:
# Construir la base maestra del proyecto a partir de las bases originales
# del Producto Bruto Geográfico (PBG) Municipal y del Censo Nacional 2022.
#
# Entrada:
# raw/01-PBG-PBA por Municipio 2021-2022 Corrientes y Constantes.xlsx
# auxiliar/c2022_bsas_est_c2_2.xlsx
#
# Salida:
# input/base_municipios.csv
# input/pbg_sectores_largo.csv
#
# Grupo 16
# Ciencia de Datos para Economía y Negocios
# FCE - UBA
# =============================================================================


# =============================================================================
# 1. LIBRERÍAS
# =============================================================================

library(tidyverse)
library(readxl)
library(janitor)
library(stringi)
library(here)
library(glue)


# =============================================================================
# 2. CONFIGURACIÓN GENERAL
# =============================================================================

source(here("scripts", "00_configuracion.R"))

options(scipen = 999)

set.seed(1234)


# =============================================================================
# 3. FUNCIONES AUXILIARES
# =============================================================================
# La función mensaje() ya queda disponible en el entorno global desde
# 00_configuracion.R; no se redefine acá para evitar duplicación.


# -----------------------------------------------------------------------------
# Normalización de nombres de municipios
# (mayúsculas + sin acentos + sin puntuación + sin espacios extra)
# -----------------------------------------------------------------------------

normalizar_texto <- function(x) {
  
  x |>
    
    stringi::stri_trans_general("Latin-ASCII") |>
    
    toupper() |>
    
    stringr::str_replace_all("[[:punct:]]", " ") |>
    
    stringr::str_squish()
  
}


# -----------------------------------------------------------------------------
# Conversión numérica robusta
#
# Las columnas económicas de este archivo llegan ya como numéricas desde
# readxl (no hay celdas de texto en el rango de datos). Por eso el primer
# paso es un atajo de seguridad: si la columna ya es numeric, se devuelve
# sin tocar. Aplicar manipulación de texto (separadores de miles/decimales)
# sobre un número ya parseado es contraproducente: as.character() de un
# double reintroduce un "." como marca decimal, y cualquier regla pensada
# para formato europeo lo interpretaría erróneamente como separador de
# miles, multiplicando el valor por una potencia de 10 según la
# cantidad de decimales de cada celda.
#
# El resto de la función cubre el caso (no presente en este archivo, pero
# sí contemplado por la consigna) de que alguna celda llegue como texto:
# guiones ("-") o vacíos se convierten en "0", y solo se reformatea el
# separador de miles/decimales si el texto realmente tiene formato europeo
# (coma decimal detectable).
# -----------------------------------------------------------------------------

limpiar_numeros <- function(x) {
  
  if (is.numeric(x)) {
    return(x)
  }
  
  x_chr <- str_squish(as.character(x))
  
  x_chr <- if_else(x_chr %in% c("-", "", "NA") | is.na(x_chr), "0", x_chr)
  
  formato_europeo <- str_detect(x_chr, ",")
  
  x_chr <- if_else(
    formato_europeo,
    x_chr |> str_replace_all("\\.", "") |> str_replace(",", "."),
    x_chr
  )
  
  as.numeric(x_chr)
  
}


# -----------------------------------------------------------------------------
# Variación porcentual
# -----------------------------------------------------------------------------

variacion_pct <- function(inicial, final) {
  
  100 * (final - inicial) / inicial
  
}


# =============================================================================
# 4. NOMBRES DE COLUMNAS DEL PBG (mapeo real - 26 columnas)
# =============================================================================

nombres_pbg <- c(
  
  "region",
  "dominio",
  "orden_publicacion",
  "codigo_partido",
  "municipio",
  
  "pbg_total",
  "impuestos",
  "vab",
  
  "bienes",
  "agro",
  "pesca",
  "mineria",
  "industria",
  "electricidad",
  "construccion",
  
  "servicios",
  "comercio",
  "hoteles",
  "transporte",
  "financiero",
  "inmobiliario",
  "publico",
  "ensenanza",
  "salud",
  "comunitarios",
  "domestico"
  
)

columnas_numericas <- setdiff(
  nombres_pbg,
  c("region", "dominio", "orden_publicacion", "codigo_partido", "municipio")
)


# =============================================================================
# 5. LECTURA Y LIMPIEZA DEL PBG
# =============================================================================

mensaje("Leyendo y limpiando base del Producto Bruto Geográfico...")

# -----------------------------------------------------------------------------
# Función de lectura + limpieza para una hoja del Excel del PBG
#
# - skip = 5: las primeras 4 filas son título/metadatos y la fila 5 son
#   subencabezados de sector; los datos municipales arrancan en la fila 6
#   (Adolfo Alsina), que es la primera fila leída por read_excel al saltear 5.
# - col_names = FALSE + asignación manual de los 26 nombres reales.
# - Se filtran las filas finales no municipales: "TOTAL PBA" y las líneas
#   metodológicas/de fuente posteriores (municipio NA o pbg_total NA tras
#   convertir, o texto "TOTAL").
# -----------------------------------------------------------------------------

leer_pbg <- function(anio, hoja) {
  
  datos <-
    read_excel(
      archivo_pbg,
      sheet = hoja,
      skip = 5,
      col_names = FALSE
    )
  
  names(datos) <- nombres_pbg
  
  datos <-
    datos |>
    
    mutate(
      
      municipio = normalizar_texto(municipio),
      
      across(
        all_of(columnas_numericas),
        limpiar_numeros
      ),
      
      region = if_else(region == "I", "Interior", "AMBA")
      
    ) |>
    
    # Eliminamos "TOTAL PBA" y cualquier fila de metadatos/fuente posterior:
    # son filas sin código de partido numérico válido o sin nombre de
    # municipio real (NA o que contenga la palabra "TOTAL").
    filter(
      
      !is.na(municipio),
      
      municipio != "",
      
      !str_detect(municipio, "TOTAL"),
      
      !is.na(codigo_partido)
      
    ) |>
    
    mutate(Anio = anio, Tipo_Valor = "Constante")
  
  datos
  
}

pbg_2021 <- leer_pbg(2021, "2021 Constantes")
pbg_2022 <- leer_pbg(2022, "2022 Constantes")

stopifnot(nrow(pbg_2021) == 135)
stopifnot(nrow(pbg_2022) == 135)

mensaje("Base PBG correctamente importada y limpiada (135 municipios x año).")


# =============================================================================
# 6. RESHAPE A FORMATO LARGO (sectores de actividad)
# =============================================================================

mensaje("Reestructurando sectores de actividad a formato largo...")

sectores_actividad <- c(
  "bienes", "agro", "pesca", "mineria", "industria",
  "electricidad", "construccion", "servicios", "comercio",
  "hoteles", "transporte", "financiero", "inmobiliario",
  "publico", "ensenanza", "salud", "comunitarios", "domestico"
)

pbg_largo <-
  bind_rows(pbg_2021, pbg_2022) |>
  
  pivot_longer(
    cols = all_of(sectores_actividad),
    names_to = "sector",
    values_to = "valor"
  ) |>
  
  select(
    Anio, Tipo_Valor, region, municipio, codigo_partido,
    pbg_total, impuestos, vab, sector, valor
  )

mensaje("Reestructuración a formato largo completada.")


# =============================================================================
# 7. LECTURA DEL CENSO 2022: POBLACIÓN (Cuadro 2.2)
# =============================================================================

mensaje("Leyendo población del Censo 2022 (Cuadro 2.2)...")

poblacion <-
  read_excel(
    archivo_poblacion,
    sheet = "Cuadro2.2",
    skip = 5,
    # La hoja tiene una 6ta columna técnica vacía (sin encabezado real)
    # que readxl igual cuenta como columna: se nombra "extra" y se descarta.
    col_names = c(
      "codigo", "municipio", "superficie_km2",
      "poblacion", "densidad", "extra"
    )
  ) |>
  
  select(-extra) |>
  
  mutate(
    poblacion = limpiar_numeros(poblacion)
  ) |>
  
  # Quedan solo las filas de municipios reales: excluimos el total
  # provincial y los subtotales de agrupación ("Total", "24/31 Partidos
  # del Gran Buenos Aires", "Resto de partidos...").
  filter(
    
    !is.na(municipio),
    
    !str_detect(municipio, regex("^Total$", ignore_case = TRUE)),
    
    !str_detect(municipio, regex("Partidos del Gran Buenos Aires", ignore_case = TRUE)),
    
    !str_detect(municipio, regex("Resto de partidos", ignore_case = TRUE)),
    
    !is.na(poblacion)
    
  ) |>
  
  mutate(
    municipio = normalizar_texto(municipio)
  ) |>
  
  select(municipio, poblacion)

stopifnot(nrow(poblacion) == 135)

mensaje("Población del Censo 2022 correctamente importada (135 municipios).")


# =============================================================================
# 8. LECTURA DEL CENSO 2022: CLASIFICACIÓN TERRITORIAL (Cuadro 2.2 bis)
# =============================================================================

mensaje("Leyendo clasificación Interior/Conurbano (Cuadro 2.2 bis)...")

# El Cuadro 2.2 bis lista primero los partidos del Conurbano bonaerense
# (encabezado de subtotal "... Partidos del Gran Buenos Aires") y luego los
# del Interior (encabezado de subtotal "Resto de partidos..."). No hay una
# columna explícita de tipo: se construye identificando las filas de
# subtotal y propagando la clasificación hacia abajo con fill().

clasificacion <-
  read_excel(
    archivo_poblacion,
    sheet = "Cuadro2.2 bis",
    skip = 5,
    col_names = c(
      "codigo", "municipio", "superficie_km2",
      "poblacion", "densidad", "extra"
    )
  ) |>
  
  select(-extra) |>
  
  filter(!is.na(municipio)) |>
  
  mutate(
    
    tipo = case_when(
      
      str_detect(municipio, regex("^Total$", ignore_case = TRUE)) ~ NA_character_,
      
      str_detect(municipio, regex("Partidos del Gran Buenos Aires", ignore_case = TRUE)) ~ "C",
      
      str_detect(municipio, regex("Resto de partidos", ignore_case = TRUE)) ~ "I",
      
      TRUE ~ NA_character_
      
    )
    
  ) |>
  
  fill(tipo, .direction = "down") |>
  
  filter(
    
    !str_detect(municipio, regex("^Total$", ignore_case = TRUE)),
    
    !str_detect(municipio, regex("Partidos del Gran Buenos Aires", ignore_case = TRUE)),
    
    !str_detect(municipio, regex("Resto de partidos", ignore_case = TRUE))
    
  ) |>
  
  mutate(
    municipio = normalizar_texto(municipio)
  ) |>
  
  select(municipio, tipo)

stopifnot(nrow(clasificacion) == 135)

mensaje("Clasificación territorial correctamente importada (135 municipios).")


# =============================================================================
# 9. RECODIFICACIÓN EXPLÍCITA DE DISCREPANCIAS CENSO <-> PBG
# =============================================================================

mensaje("Recodificando discrepancias de nombres entre Censo y PBG...")

# -----------------------------------------------------------------------------
# Tras normalizar (mayúsculas, sin acentos, sin puntuación), la Dirección
# Provincial de Estadística (PBG) y el INDEC (Censo) usan denominaciones
# levemente distintas para algunos partidos. Se homogeneízan ambas fuentes
# al estándar usado por el PBG, que es la base "ancla" del análisis.
#
# Discrepancias detectadas en este archivo (verificado contra ambos Excel):
#   - "CORONEL DE MARINA LEONARDO ROSALES" (Censo) vs.
#     "CORONEL DE MARINA L ROSALES" (PBG)
#
# Se incluyen además, a modo preventivo, las recodificaciones típicas que
# suelen aparecer en otras versiones de estos archivos (9 de Julio,
# 25 de Mayo, José C. Paz, Exaltación de la Cruz), aunque en esta versión
# particular ya coinciden tras la normalización estándar.
# -----------------------------------------------------------------------------

recodificar_municipios <- function(x) {
  
  # Paso 1: reemplazo literal de la denominación larga del Censo por la
  # denominación corta usada en el PBG.
  x_reemplazado <-
    str_replace_all(
      x,
      "CORONEL DE MARINA LEONARDO ROSALES",
      "CORONEL DE MARINA L ROSALES"
    )
  
  # Paso 2: recodificaciones adicionales. Se usa case_when() en lugar de
  # case_match() (deprecado desde dplyr 1.2.0) para no depender de la
  # versión instalada de dplyr. El .default equivale a x_reemplazado y
  # no al parámetro original "x" sin modificar.
  case_when(
    
    x_reemplazado == "VEINTICINCO DE MAYO"   ~ "25 DE MAYO",
    x_reemplazado == "NUEVE DE JULIO"        ~ "9 DE JULIO",
    x_reemplazado == "JOSE C PAZ"            ~ "JOSE C PAZ",
    x_reemplazado == "EXALTACION DE LA CRUZ" ~ "EXALTACION DE LA CRUZ",
    
    TRUE ~ x_reemplazado
    
  )
  
}

poblacion <-
  poblacion |>
  mutate(municipio = recodificar_municipios(municipio))

clasificacion <-
  clasificacion |>
  mutate(municipio = recodificar_municipios(municipio))

mensaje("Recodificación de municipios aplicada.")


# =============================================================================
# 10. VERIFICACIÓN DEL JOIN ENTRE FUENTES
# =============================================================================

mensaje("Verificando coincidencia de municipios entre PBG y Censo...")

cat("\nMunicipios únicamente en PBG (sin match en Censo - Población):\n")
print(
  anti_join(pbg_2022, poblacion, by = "municipio") |>
    select(municipio)
)

cat("\nMunicipios únicamente en Censo - Población (sin match en PBG):\n")
print(
  anti_join(poblacion, pbg_2022, by = "municipio") |>
    select(municipio)
)

# Validación dura: la recodificación debe haber resuelto el 100% del join.
stopifnot(
  nrow(anti_join(pbg_2022, poblacion, by = "municipio")) == 0
)

stopifnot(
  nrow(anti_join(pbg_2022, clasificacion, by = "municipio")) == 0
)

mensaje("Join validado: los 135 municipios coinciden en ambas fuentes.")


# =============================================================================
# 11. DATAFRAME FINAL CONSOLIDADO (una fila por municipio)
# =============================================================================

mensaje("Construyendo dataframe final consolidado...")

base <-
  
  pbg_2021 |>
  
  select(
    municipio, region, codigo_partido,
    pbg_total, bienes, servicios, agro, industria
  ) |>
  
  rename(
    
    PBG2021       = pbg_total,
    bienes_2021   = bienes,
    servicios_2021 = servicios,
    agro_2021     = agro,
    industria_2021 = industria
    
  ) |>
  
  left_join(
    
    pbg_2022 |>
      
      select(
        municipio,
        pbg_total, bienes, servicios, agro, industria
      ) |>
      
      rename(
        
        PBG2022       = pbg_total,
        bienes_2022   = bienes,
        servicios_2022 = servicios,
        agro_2022     = agro,
        industria_2022 = industria
        
      ),
    
    by = "municipio"
    
  ) |>
  
  left_join(poblacion, by = "municipio") |>
  
  left_join(clasificacion, by = "municipio") |>
  
  mutate(
    
    # Código INDEC estándar de 5 dígitos (provincia "06" + partido 3
    # dígitos). Es el formato habitual de las capas cartográficas de
    # partidos de la Provincia de Buenos Aires (IGN/INDEC) y se usa más
    # adelante para el mapa coroplético del script 06_graficos.R.
    codigo_indec = paste0("06", codigo_partido),
    
    # -------------------------------------------------------------------
    # Indicadores per cápita
    # -------------------------------------------------------------------
    PBG_per_capita = PBG2022 / poblacion,
    
    # -------------------------------------------------------------------
    # Pesos sectoriales como % del PBG total 2022
    # -------------------------------------------------------------------
    Peso_Agro       = 100 * agro_2022 / PBG2022,
    Peso_Industria  = 100 * industria_2022 / PBG2022,
    Peso_Servicios  = 100 * servicios_2022 / PBG2022,
    
    # -------------------------------------------------------------------
    # Tasas de variación porcentual 2021-2022
    # -------------------------------------------------------------------
    Variacion_Total      = variacion_pct(PBG2021, PBG2022),
    Variacion_Bienes     = variacion_pct(bienes_2021, bienes_2022),
    Variacion_Servicios  = variacion_pct(servicios_2021, servicios_2022)
    
  )


# -----------------------------------------------------------------------------
# Variable categórica "Agroindustrial":
# un municipio se clasifica como "Agroindustrial" si su Peso_Agro Y su
# Peso_Industria superan SIMULTÁNEAMENTE las medianas provinciales de
# ambos indicadores; en caso contrario, "Resto".
# -----------------------------------------------------------------------------

mediana_agro <- median(base$Peso_Agro, na.rm = TRUE)
mediana_industria <- median(base$Peso_Industria, na.rm = TRUE)

base <-
  base |>
  
  mutate(
    
    Agroindustrial = if_else(
      
      Peso_Agro > mediana_agro & Peso_Industria > mediana_industria,
      
      "Agroindustrial",
      
      "Resto"
      
    )
    
  )

mensaje("Dataframe final consolidado.")


# =============================================================================
# 12. VALIDACIONES DE CALIDAD (estrictas, antes de exportar)
# =============================================================================

mensaje("Ejecutando validaciones de calidad...")

# Cantidad de filas: exactamente 135 municipios, una fila cada uno.
stopifnot(nrow(base) == 135)

# Sin municipios duplicados.
stopifnot(!anyDuplicated(base$municipio))

# Sin NA en identificadores clave.
stopifnot(sum(is.na(base$municipio)) == 0)
stopifnot(sum(is.na(base$tipo)) == 0)

# Sin NA en población (requisito explícito del enunciado).
stopifnot(sum(is.na(base$poblacion)) == 0)

# Sin NA en los indicadores de variación (requisito explícito del enunciado).
stopifnot(sum(is.na(base$Variacion_Total)) == 0)
stopifnot(sum(is.na(base$Variacion_Bienes)) == 0)
stopifnot(sum(is.na(base$Variacion_Servicios)) == 0)

# Sin NA en PBG per cápita ni en pesos sectoriales.
stopifnot(sum(is.na(base$PBG_per_capita)) == 0)
stopifnot(sum(is.na(base$Peso_Agro)) == 0)
stopifnot(sum(is.na(base$Peso_Industria)) == 0)
stopifnot(sum(is.na(base$Peso_Servicios)) == 0)

# Coherencia de magnitudes: ningún PBG total negativo o cero.
stopifnot(all(base$PBG2021 > 0))
stopifnot(all(base$PBG2022 > 0))

# Coherencia del clasificador territorial.
stopifnot(all(base$tipo %in% c("I", "C")))

mensaje("Validaciones de calidad superadas correctamente.")


# =============================================================================
# 13. RESUMEN GENERAL
# =============================================================================

mensaje("Resumen de la base")

cat("\n------------------------------------------\n")
cat("Cantidad de municipios:", nrow(base), "\n")
cat("Municipios Interior:", sum(base$tipo == "I"), "\n")
cat("Municipios Conurbano:", sum(base$tipo == "C"), "\n")
cat("Municipios Agroindustriales:", sum(base$Agroindustrial == "Agroindustrial"), "\n")
cat("------------------------------------------\n")

print(
  summary(
    select(
      base,
      PBG2021, PBG2022, PBG_per_capita,
      Variacion_Total, Variacion_Bienes, Variacion_Servicios
    )
  )
)


# =============================================================================
# 14. EXPORTACIÓN
# =============================================================================

mensaje("Guardando base maestra y base intermedia en formato largo...")

write_csv(
  base,
  file.path(INPUT_DIR, "base_municipios.csv")
)

write_csv(
  pbg_largo,
  file.path(INPUT_DIR, "pbg_sectores_largo.csv")
)


# =============================================================================
# 15. FINALIZACIÓN
# =============================================================================

cat("\n==============================================\n")
cat(" LIMPIEZA FINALIZADA CORRECTAMENTE\n")
cat("==============================================\n\n")
cat("Archivos generados:\n")
cat(" -", file.path(INPUT_DIR, "base_municipios.csv"), "\n")
cat(" -", file.path(INPUT_DIR, "pbg_sectores_largo.csv"), "\n\n")
cat("La base quedó lista para 02_descriptivos.R\n\n")