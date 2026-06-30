# Recuperación económica municipal post-pandemia en la Provincia de Buenos Aires (2021–2022)

### Ciencia de Datos para Economía y Negocios — FCE-UBA

---

## Grupo 16; Integrante:
- Rodrigo Astengo (914720)

## Objetivo
Explorar la velocidad y los determinantes sectoriales de la reactivación económica en los municipios de la Provincia de Buenos Aires durante el período posterior a la crisis sanitaria. El objetivo es evaluar si la recuperación fue homogénea o si existieron brechas sistemáticas explicadas por la localización geográfica y la especialización productiva de cada partido.

> **Hipótesis principal:** Los municipios con una estructura económica diversificada o vinculada a la cadena agroindustrial en el Interior de la provincia mostraron una recuperación del PBG real más acelerada en 2021-22 que los municipios del AMBA dependientes del sector servicios y el comercio minorista.

> **Hipótesis complementaria:** La recuperación económica del período 2021-22 estuvo traccionada principalmente por los sectores productores de bienes (agropecuario e industria), mientras que los sectores productores de servicios presenciales (hoteles y restaurantes; y servicios comunitarios y personales) presentaron un rezago significativo en términos reales, independientemente de la localización geográfica del municipio.

## Datos
- **Fuente principal:** [Producto Bruto Geográfico Municipal — Ministerio de Economía de la Provincia de Buenos Aires](https://drive.google.com/drive/u/0/folders/1xt510FQgK-0MK8MfRdWHQvpc9Kc9YOxI)
- **Fuente complementaria:** [Censo Nacional de Población, Hogares y Viviendas 2022 — INDEC](https://www.indec.gob.ar)
- **Período:** 2021–2022 (valores medidos a precios constantes).
- **Unidad de análisis:** 135 municipios de la Provincia de Buenos Aires.

## Análisis realizado
1. **Limpieza y reestructuración:** Consolidación de hojas de cálculo, normalización de nombres de municipios para evitar errores de duplicación y pivoteo de la matriz sectorial (CIIU de la A a la P) a formato de tabla larga (*tidy data*).
2. **Análisis exploratorio:** Cálculo de estadísticas de resumen (medias, desvíos y coeficientes de variación) y detección de valores atípicos (*outliers*) mediante el método del rango intercuartílico (IQR).
3. **Test de diferencia de medias:** Comparación de las tasas de crecimiento promedio entre el Interior y el AMBA (utilizando pruebas t de Student o Wilcoxon según el cumplimiento de supuestos).
4. **Test de diferencias pareadas:** Evaluación interna intra-municipio para contrastar la variación real del bloque de Bienes frente al bloque de Servicios.
5. **Modelo de regresión lineal múltiple:** Estimación por Mínimos Cuadrados Ordinarios (MCO) en sección cruzada para cuantificar el impacto de la localización y el peso sectorial inicial sobre el crecimiento del PBG, controlando por tamaño poblacional y verificando supuestos econométricos (heterocedasticidad y multicolinealidad).
6. **Visualizaciones avanzadas:** Diagnóstico gráfico detallado a través de curvas de densidad comparadas, diagramas de caja combinados con dispersión de puntos (*jitter plots*), histogramas de frecuencias por variable y curvas de distribución acumulada empírica (ECDF).

## Estructura del repositorio
```text
proyecto/
├── raw/                 # Bases de datos originales (DPE e INDEC)
├── auxiliar/            # Nomencladores y tablas de códigos secundarios
├── input/               # Dataset consolidado y limpio listo para el análisis
├── output/
│   ├── tablas/          # Resultados de descriptivos, tests y regresiones
│   └── graficos/        # Visualizaciones y distribuciones empíricas generadas
├── scripts/
│   ├── 00_configuracion.R
│   ├── 01_limpieza.R
│   ├── 02_descriptivos.R
│   ├── 03_test_regiones.R
│   ├── 04_test_sectorial.R
│   ├── 05_regresion.R
│   └── 06_graficos.R
├── utils/               # Funciones auxiliares personalizadas
└── README.md
```

## Reproducción

### Paquetes necesarios
```r
install.packages(c(
  "tidyverse", "readxl", "janitor", "ggrepel", 
  "viridis", "broom", "car", "lmtest", 
  "sandwich", "performance", "scales"
))
```

### Orden de ejecución

| Paso | Script | Descripción y Propósito del Proceso |
| :---: | :--- | :--- |
| **1** | `scripts/00_configuracion.R` | **Gestión automatizada del entorno:** Inicializa variables globales, comprueba e instala librerías faltantes de forma dinámica y previene conflictos de enmascaramiento para blindar la reproducibilidad. |
| **2** | `scripts/01_limpieza.R` | **Proceso ETL:** Lee las fuentes crudas en `raw/` y `auxiliar/`, realiza la limpieza, normalización de nombres municipales y exporta el dataset consolidado a `input/`. |
| **3** | `scripts/02_descriptivos.R` | **Estadística descriptiva:** Calcula medidas de tendencia central y dispersión, junto con el diagnóstico de valores atípicos (*outliers*) mediante el método IQR. |
| **4** | `scripts/03_test_regiones.R` | **Inferencia geográfica:** Aplica pruebas de hipótesis de muestras independientes para evaluar la brecha real de crecimiento económico entre el Interior y el AMBA. |
| **5** | `scripts/04_test_sectorial.R` | **Inferencia sectorial:** Realiza el testeo de diferencias pareadas intra-municipio para analizar la dinámica de reactivación de Bienes vs. Servicios. |
| **6** | `scripts/05_regresion.R` | **Modelado econométrico:** Estima el modelo lineal múltiple por MCO en sección cruzada y testea el cumplimiento de los supuestos clásicos de diagnóstico. |
| **7** | `scripts/06_graficos.R` | **Visualización analítica:** Genera y exporta las funciones de distribución acumulada (ECDF), histogramas, boxplots y curvas de densidad en `output/graficos/`. |

## Conclusiones principales
El análisis demuestra que la recuperación económica posterior a la pandemia presentó una marcada heterogeneidad de carácter estructural en la provincia. Los municipios del Interior vinculados a eslabonamientos agroindustriales exhibieron tasas de crecimiento real promedio superiores y curvas de densidad con mayor dispersión que los centros urbanos concentrados en el AMBA, cuya recuperación fue más homogénea pero acotada. Asimismo, la evidencia sectorial confirma que la reactivación estuvo traccionada de forma neta por las ramas productoras de bienes, mientras que las actividades de servicios presenciales registraron un rezago relativo persistente, validando las conjeturas iniciales de la investigación.
