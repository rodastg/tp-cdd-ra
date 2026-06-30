#(readme temporal)

# Recuperación económica municipal post-pandemia en la Provincia de Buenos Aires (2021–2022)

## Ciencia de Datos para Economía y Negocios

**Facultad de Ciencias Económicas – Universidad de Buenos Aires**

---

# Integrantes

* Rodrigo Astengo

---

# Objetivo del trabajo

Este trabajo analiza la recuperación económica de los municipios de la Provincia de Buenos Aires durante el período posterior a la pandemia de COVID-19 utilizando información del Producto Bruto Geográfico (PBG) municipal a precios constantes para los años 2021 y 2022.

El objetivo es determinar si la recuperación económica fue homogénea entre los municipios o si existieron diferencias sistemáticas asociadas a su estructura productiva y localización geográfica.

## Hipótesis principal

> Los municipios con una estructura económica diversificada o vinculada a la cadena agroindustrial en el Interior de la provincia mostraron una recuperación del PBG real más acelerada en 2021-22 que los municipios del AMBA dependientes del sector servicios y el comercio minorista.

## Hipótesis complementaria

> La recuperación económica del período 2021-22 estuvo traccionada principalmente por los sectores productores de bienes (agropecuario e industria), mientras que los sectores productores de servicios presenciales (hoteles y restaurantes; y servicios comunitarios y personales) presentaron un rezago significativo en términos reales, independientemente de la localización geográfica del municipio.

---

# Bases de datos

## Base principal

**Producto Bruto Geográfico Municipal de la Provincia de Buenos Aires**

* Organismo: Ministerio de Economía de la Provincia de Buenos Aires
* Período utilizado: 2021–2022
* Valores utilizados: precios constantes
* Unidad de análisis: municipios (135 partidos)

Fuente oficial:

[https://drive.google.com/drive/u/0/folders/1xt510FQgK-0MK8MfRdWHQvpc9Kc9YOxI]

## Base complementaria

**Censo Nacional de Población, Hogares y Viviendas 2022**

Se utiliza exclusivamente para incorporar la población municipal y construir indicadores per cápita y variables de control demográfico.

Fuente:

https://www.indec.gob.ar

## Información geográfica

GeoJSON de municipios de la Provincia de Buenos Aires utilizado para construir el mapa coroplético.

---

# Variables principales

| Variable                     | Tipo       | Descripción                                    |
| ---------------------------- | ---------- | ---------------------------------------------- |
| Municipio                    | Categórica | Partido de la Provincia de Buenos Aires        |
| Región                       | Categórica | Interior / AMBA                                |
| PBG constante 2021           | Numérica   | Producto Bruto Geográfico a precios constantes |
| PBG constante 2022           | Numérica   | Producto Bruto Geográfico a precios constantes |
| Variación porcentual del PBG | Numérica   | Tasa de crecimiento real entre 2021 y 2022     |
| Participación del agro       | Numérica   | Peso del sector agropecuario en el PBG         |
| Participación industrial     | Numérica   | Peso del sector industrial en el PBG           |
| Población                    | Numérica   | Censo Nacional 2022                            |
| Logaritmo de la población    | Numérica   | Variable de control utilizada en la regresión  |

---

# Metodología

El análisis se desarrolla mediante un flujo completamente reproducible compuesto por seis etapas.

## 1. Limpieza y preparación de datos

* Lectura de las bases originales.
* Limpieza de encabezados.
* Estandarización de nombres de municipios.
* Conversión de variables numéricas.
* Construcción de indicadores de crecimiento.
* Incorporación de población mediante uniones entre bases.
* Exportación de la base limpia.

## 2. Estadística descriptiva

Se calculan:

* media
* mediana
* desvío estándar
* mínimo
* máximo
* cuartiles
* coeficiente de variación

Además se realiza:

* detección de datos faltantes
* detección de outliers mediante IQR
* visualización de distribuciones

## 3. Test de diferencia de medias

Se compara la recuperación económica promedio entre:

* Municipios del Interior
* Municipios del AMBA

Dependiendo del cumplimiento de los supuestos se aplica:

* Test t de Student
* Test de Wilcoxon-Mann-Whitney

## 4. Test de diferencias pareadas

Se compara, para cada municipio, el crecimiento de:

* Sectores productores de bienes
* Sectores productores de servicios

mediante una prueba t para muestras pareadas.

## 5. Modelo de regresión lineal múltiple

Se estima un modelo por Mínimos Cuadrados Ordinarios donde la variable dependiente es la variación porcentual del PBG municipal.

Variables explicativas:

* Interior / AMBA
* Participación inicial del sector agropecuario
* Participación inicial del sector industrial
* Logaritmo de la población

Se verifican además los principales supuestos econométricos del modelo.

## 6. Visualización

Se generan gráficos exploratorios y comunicacionales utilizando **ggplot2**, incluyendo un mapa coroplético de la Provincia de Buenos Aires para representar espacialmente la recuperación económica municipal.

---

# Estructura del repositorio

```text
proyecto/
│
├── raw/
│
├── auxiliar/
│
├── input/
│
├── output/
│   ├── tablas/
│   └── graficos/
│
├── scripts/
│   ├── 00_configuracion.R
│   ├── 01_limpieza.R
│   ├── 02_descriptivos.R
│   ├── 03_test_regiones.R
│   ├── 04_test_sectorial.R
│   ├── 05_regresion.R
│   └── 06_graficos.R
│
├── utils/
│
└── README.md
```

---

# Reproducibilidad

El proyecto fue desarrollado siguiendo criterios de reproducibilidad.

* No se utilizan rutas absolutas.
* Cada script constituye una unidad autónoma.
* Todos los archivos intermedios se guardan en disco.
* Ningún script depende del Environment de RStudio.
* Todos los resultados pueden reproducirse ejecutando nuevamente los scripts.

## Paquetes utilizados

```r
install.packages(c(
  "tidyverse",
  "readxl",
  "janitor",
  "stringr",
  "lubridate",
  "sf",
  "ggplot2",
  "ggrepel",
  "viridis",
  "broom",
  "car",
  "lmtest",
  "sandwich",
  "performance",
  "scales"
))
```

---

# Orden de ejecución

Los scripts deben ejecutarse en el siguiente orden:

1. `00_configuracion.R`
2. `01_limpieza.R`
3. `02_descriptivos.R`
4. `03_test_regiones.R`
5. `04_test_sectorial.R`
6. `05_regresion.R`
7. `06_graficos.R`

Cada script genera automáticamente los archivos necesarios para la siguiente etapa del análisis.

---

# Resultados generados

## Carpeta input/

* Base de datos limpia utilizada en todo el proyecto.

## Carpeta output/tablas/

* Estadísticas descriptivas.
* Resultados de los tests de hipótesis.
* Resultados de la regresión.
* Diagnóstico de datos faltantes y outliers.

## Carpeta output/graficos/

* Histogramas.
* Boxplots.
* Scatterplots.
* Barras ordenadas.
* Boxplots por región.
* Mapa coroplético de municipios.

---

# Principales resultados

Los resultados obtenidos permiten evaluar si la recuperación económica posterior a la pandemia presentó diferencias estadísticamente significativas entre municipios según su localización geográfica y su estructura productiva.

En particular, el trabajo analiza el papel de los sectores agropecuario e industrial como posibles determinantes del crecimiento económico municipal durante el período 2021–2022.

---

# Posibles extensiones

Entre las principales líneas de trabajo futuras se destacan:

* Incorporar una mayor cantidad de años para construir un panel de datos.
* Incorporar variables socioeconómicas adicionales (empleo, pobreza, exportaciones, etc.).
* Analizar la heterogeneidad espacial mediante modelos econométricos espaciales.
* Construir indicadores sintéticos de especialización productiva municipal.
