# Hidrología superficial con R
[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.20287572-blue.svg)](https://doi.org/10.5281/zenodo.20287572)

[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

[![R](https://img.shields.io/badge/language-R-blue.svg)](https://www.r-project.org/)

[![Version](https://img.shields.io/badge/version-v1.0-green.svg)]()

Scripts de R para el análisis hidrológico de cuencas mediante:

- Método racional
- Método SCS
- Hidrograma unitario de Clark

Los scripts están orientados a docencia universitaria en geología, ingeniería y ciencias ambientales.

---

## Documentación y DOI

La guía metodológica completa asociada a este repositorio está disponible en Zenodo. El documento PDF describe el flujo de trabajo completo integrando QGIS, SAGA GIS y scripts desarrollados en R.

https://doi.org/10.5281/zenodo.20287572


---

## Uso recomendado IMPORTANTE

Abrir el siguiente archivo en RStudio para trabajar con rutas relativas y estructura reproducible de proyecto.

`hidrologia_superficial.Rproj`


---

## Contenido

### 01. Preparación del DTM
- Corrección hidrológica
- Quemado de cauces
- Dirección de flujo
- Acumulación

### 02. Parámetros hidrológicos
- Tiempo de concentración
- Longitud de cauce
- Pendientes
- Número de curva (CN)

### 03. Lluvia efectiva
- Hietogramas
- Intensidad-duración-frecuencia

### 04. Método racional
- Cálculo de caudal punta

### 05. Hidrograma SCS
- Generación de hidrogramas

### 06. Hidrograma de Clark
- Traducción y almacenamiento
- Convolución

### 07. Comparación de métodos
- Comparación racional / SCS / Clark

---

## Requisitos

Paquetes principales de R:

```r
terra
sf
dplyr
ggplot2
readr
```

---

## Licencia

Creative Commons Attribution 4.0 International (CC BY 4.0)

---

## Autores

Luis Arlegui Crespo  
Paula Quílez Benegas
University of Zaragoza
