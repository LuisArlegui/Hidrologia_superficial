# Hidrología superficial con R

Scripts de R para el análisis hidrológico de cuencas mediante:

- Método racional
- Método SCS
- Hidrograma unitario de Clark

Los scripts están orientados a docencia universitaria en geología, ingeniería y ciencias ambientales.

## Uso recomendado IMPORTANTE

Abrir el archivo:

`hidrologia_superficial.Rproj`

mediante RStudio para trabajar con rutas relativas y estructura reproducible del proyecto.
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

## Autor

Luis Arlegui Crespo  
University of Zaragoza