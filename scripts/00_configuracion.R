# ==========================================================
# Proyecto:
# Recuperación económica municipal post-pandemia
#
# Materia:
# Ciencia de Datos para Economía y Negocios
# FCE - UBA
#
# Script:
# 00_configuracion.R
#
# Objetivo:
# Configurar el entorno de trabajo del proyecto.
#
# Autor:
# Rodrigo Astengo
# ==========================================================

# =============================================================================
# 1. PAQUETES
# =============================================================================

paquetes_necesarios <- c(
  
  "tidyverse",
  "readxl",
  "janitor",
  "here",
  "scales",
  "ggrepel",
  "sf",
  "broom",
  "car",
  "lmtest",
  "sandwich"
  
)

paquetes_faltantes <-
  
  paquetes_necesarios[
    !paquetes_necesarios %in%
      installed.packages()[, "Package"]
  ]

if(length(paquetes_faltantes) > 0){
  
  stop(
    
    paste0(
      
      "\nFaltan instalar los siguientes paquetes:\n\n",
      
      paste(paquetes_faltantes,
            collapse="\n")
      
    )
    
  )
  
}

invisible(
  
  lapply(
    
    paquetes_necesarios,
    
    library,
    
    character.only=TRUE
    
  )
  
)

# =============================================================================
# 2. CARPETAS DEL PROYECTO
# =============================================================================

if(!dir.exists(here("input")))
  dir.create(here("input"))

if(!dir.exists(here("output")))
  dir.create(here("output"))

if(!dir.exists(here("output","graficos")))
  dir.create(here("output","graficos"))

if(!dir.exists(here("output","tablas")))
  dir.create(here("output","tablas"))

if(!dir.exists(here("utils")))
  dir.create(here("utils"))

# =============================================================================
# 3. DIRECTORIOS
# =============================================================================

RAW_DIR <- here("raw")

AUX_DIR <- here("auxiliar")

INPUT_DIR <- here("input")

OUTPUT_DIR <- here("output")

# =============================================================================
# 4. ARCHIVOS DEL PROYECTO
# =============================================================================

archivo_pbg <-
  
  file.path(
    
    RAW_DIR,
    
    "01-PBG-PBA por Municipio 2021-2022 Corrientes y Constantes.xlsx"
    
  )

archivo_poblacion <-
  
  file.path(
    
    AUX_DIR,
    
    "c2022_bsas_est_c2_2.xlsx"
    
  )

# =============================================================================
# 5. VERIFICACIÓN DE ARCHIVOS
# =============================================================================

archivos_requeridos <- c(
  
  archivo_pbg,
  
  archivo_poblacion
  
)

archivos_faltantes <-
  
  archivos_requeridos[
    !file.exists(archivos_requeridos)
  ]

if(length(archivos_faltantes)>0){
  
  stop(
    
    paste0(
      
      "\nNo se encontraron los siguientes archivos:\n\n",
      
      paste(
        
        archivos_faltantes,
        
        collapse="\n"
        
      )
      
    )
    
  )
  
}

# =============================================================================
# 6. TEMA GRÁFICO
# =============================================================================

theme_set(
  
  theme_minimal(base_size=12)+
    
    theme(
      
      plot.title=
        
        element_text(
          
          face="bold",
          
          size=15
          
        ),
      
      plot.subtitle=
        
        element_text(
          
          size=11,
          
          color="gray40"
          
        ),
      
      plot.caption=
        
        element_text(
          
          color="gray50",
          
          size=9,
          
          hjust=0
          
        ),
      
      legend.position="bottom",
      
      panel.grid.minor=
        
        element_blank()
      
    )
  
)

# =============================================================================
# 7. PALETA
# =============================================================================

colores_proyecto <- c(
  
  Principal="#1B4965",
  
  Secundario="#5FA8D3",
  
  Destacado="#CA6702",
  
  Positivo="#2A9D8F",
  
  Negativo="#C1121F",
  
  Gris="gray60"
  
)

# =============================================================================
# 8. OPCIONES GENERALES
# =============================================================================

options(
  
  scipen=999,
  
  dplyr.summarise.inform=FALSE
  
)

# =============================================================================
# 9. FUNCIÓN DE MENSAJES
# =============================================================================

mensaje <- function(texto){
  
  cat(
    
    "\n",
    
    "--------------------------------------------------\n",
    
    texto,
    
    "\n",
    
    "--------------------------------------------------\n\n",
    
    sep=""
    
  )
  
}

# =============================================================================
# 10. MENSAJE FINAL
# =============================================================================

cat(
  
  "\n",
  
  "==============================================\n",
  
  " CONFIGURACION INICIAL COMPLETADA\n",
  
  "==============================================\n\n",
  
  " ✓ Paquetes cargados\n",
  
  " ✓ Carpetas verificadas\n",
  
  " ✓ Archivos encontrados\n",
  
  " ✓ Tema gráfico configurado\n",
  
  " ✓ Paleta institucional cargada\n\n",
  
  "Puede comenzar la ejecución de los scripts.\n\n",
  
  sep=""
  
)