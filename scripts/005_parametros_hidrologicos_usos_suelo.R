############################################################
#
# 005_parametros_hidrologicos_usos_suelo.R
#
# Objetivo:
# - Leer la cuenca desde el GeoPackage único del proyecto
# - Leer el raster CN generado por SIOSE2CN
# - Leer capas vectoriales generadas por SIOSE2CN/SIOSE2Manning
# - Importar dichas capas al GeoPackage único, si existen como GPKG externos
# - Calcular CN medio, percentiles y distribución
# - Calcular P0 medio ponderado por superficie
# - Calcular Manning ponderado por superficie
# - Exportar tablas y figuras
#
############################################################


# 0. PAQUETES ====

paquetes <- c("terra", "sf", "dplyr", "readr", "ggplot2", "tibble")

instalar <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(instalar) > 0) install.packages(instalar)

library(terra)
library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(tibble)


# 1. CONFIGURACION GENERAL ====

if (file.exists("scripts/000_rutas_y_capas.R")) {
  source("scripts/000_rutas_y_capas.R")
} else {
  stop("No se encuentra scripts/000_rutas_y_capas.R")
}

dir.create("salidas/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/figuras", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/mapas", recursive = TRUE, showWarnings = FALSE)


# 2. RUTAS Y CAPAS ====

capa_cuenca <- capas_gpkg$cuenca

archivo_cn <- "salidas/mapas/NC_CURSO.tif"

archivo_manning <- "salidas/mapas/SIOSE_Manning_AOI.gpkg"
archivo_p0 <- "salidas/mapas/CAPA_FINAL_CN.gpkg"

capa_manning <- "siose_manning_aoi"
capa_p0 <- "capa_final_cn_p0"

salida_resumen <- "salidas/tablas/005_parametros_hidrologicos_resumen.csv"
salida_cn_stats <- "salidas/tablas/005_estadisticos_CN.csv"
salida_p0_stats <- "salidas/tablas/005_estadisticos_P0.csv"
salida_manning_stats <- "salidas/tablas/005_estadisticos_Manning.csv"

salida_fig_cn <- "salidas/figuras/005_histograma_CN.png"
salida_fig_manning <- "salidas/figuras/005_manning_por_cobertura.png"


# 3. PARAMETROS ====

epsg_objetivo <- 25830
importar_capas_vectoriales <- TRUE

campo_p0 <- "PO_MM"
campo_area_p0 <- "area_i"

campo_manning <- "MANNING_N"
campo_area_manning <- "AREA_M2"
campo_cobertura_manning <- "ID_COBERTURA_MAX"


# 4. FUNCIONES AUXILIARES ====

leer_vector_desde_archivo <- function(archivo, epsg_objetivo = 25830) {
  if (!file.exists(archivo)) {
    stop("No se encuentra el archivo vectorial: ", archivo)
  }

  x <- st_read(archivo, quiet = TRUE)
  x <- st_zm(x, drop = TRUE, what = "ZM")

  if (is.na(st_crs(x))) {
    st_crs(x) <- epsg_objetivo
  }

  x <- st_make_valid(x)
  x
}

leer_o_importar_capa <- function(archivo, layer, descripcion) {

  if (existe_capa_gpkg(layer) && !importar_capas_vectoriales) {
    return(leer_capa_gpkg(layer))
  }

  if (existe_capa_gpkg(layer) && importar_capas_vectoriales && !file.exists(archivo)) {
    return(leer_capa_gpkg(layer))
  }

  if (!file.exists(archivo)) {
    if (existe_capa_gpkg(layer)) {
      return(leer_capa_gpkg(layer))
    }

    stop(
      "No se encuentra ", descripcion, ".\n",
      "Archivo esperado: ", archivo, "\n",
      "Tampoco existe la capa '", layer, "' en el GeoPackage."
    )
  }

  x <- leer_vector_desde_archivo(archivo, epsg_objetivo)

  if (importar_capas_vectoriales) {
    guardar_capa_gpkg(x, layer = layer, overwrite = TRUE)
    message("Capa importada al GeoPackage: ", layer)
  }

  x
}


# 5. LEER DATOS ====

if (!existe_capa_gpkg(capa_cuenca)) {
  stop(
    "No existe la capa de cuenca '", capa_cuenca, "' en el GeoPackage.\n",
    "Ejecuta antes el script 002_importar_resultados_QGIS_a_gpkg.R.\n",
    "Capas disponibles:\n",
    paste(listar_capas_gpkg()$name, collapse = ", ")
  )
}

if (!file.exists(archivo_cn)) {
  stop("No se encuentra el raster CN: ", archivo_cn)
}

cuenca <- leer_capa_gpkg(capa_cuenca)
cuenca <- st_zm(cuenca, drop = TRUE, what = "ZM")
cuenca <- st_make_valid(cuenca)

cn <- rast(archivo_cn)

manning <- leer_o_importar_capa(
  archivo = archivo_manning,
  layer = capa_manning,
  descripcion = "la capa Manning de SIOSE2Manning"
)

p0_layer <- leer_o_importar_capa(
  archivo = archivo_p0,
  layer = capa_p0,
  descripcion = "la capa P0/CN de SIOSE2CN"
)

cat("\nCuenca:\n")
print(cuenca)

cat("\nRaster CN:\n")
print(cn)

cat("\nCapa Manning:\n")
print(manning)

cat("\nCapa P0/CN:\n")
print(p0_layer)


# 6. COMPROBAR Y AJUSTAR CRS ====

if (is.na(st_crs(cuenca))) {
  stop("La capa de cuenca no tiene CRS definido.")
}

if (is.na(crs(cn))) {
  stop("El raster CN no tiene CRS definido.")
}

if (is.na(st_crs(manning))) {
  stop("La capa Manning no tiene CRS definido.")
}

if (is.na(st_crs(p0_layer))) {
  stop("La capa P0/CN no tiene CRS definido.")
}

if (st_crs(cuenca)$epsg != epsg_objetivo) {
  cuenca <- st_transform(cuenca, epsg_objetivo)
}

if (st_crs(manning)$epsg != epsg_objetivo) {
  manning <- st_transform(manning, epsg_objetivo)
}

if (st_crs(p0_layer)$epsg != epsg_objetivo) {
  p0_layer <- st_transform(p0_layer, epsg_objetivo)
}


# 7. AREA DE CUENCA ====

area_cuenca_m2 <- sum(as.numeric(st_area(cuenca)))
area_cuenca_ha <- area_cuenca_m2 / 10000
area_cuenca_km2 <- area_cuenca_m2 / 1e6

cat("\nArea de cuenca:\n")
cat(round(area_cuenca_ha, 3), "ha\n")
cat(round(area_cuenca_km2, 4), "km2\n")


# 8. ESTADISTICOS DEL RASTER CN ====

cat("\nCalculando estadisticos del CN...\n")

cuenca_vect <- vect(cuenca)

cn_crop <- crop(cn, cuenca_vect)
cn_mask <- mask(cn_crop, cuenca_vect)

valores_cn <- values(cn_mask, na.rm = TRUE)
valores_cn <- as.numeric(valores_cn[, 1])
valores_cn <- valores_cn[is.finite(valores_cn)]

if (length(valores_cn) == 0) {
  stop("No se han encontrado valores validos de CN dentro de la cuenca.")
}

cn_stats <- tibble(
  variable = "CN",
  n_pixeles = length(valores_cn),
  minimo = min(valores_cn),
  p05 = as.numeric(quantile(valores_cn, 0.05)),
  p10 = as.numeric(quantile(valores_cn, 0.10)),
  media = mean(valores_cn),
  mediana = median(valores_cn),
  p90 = as.numeric(quantile(valores_cn, 0.90)),
  p95 = as.numeric(quantile(valores_cn, 0.95)),
  maximo = max(valores_cn),
  desviacion_tipica = sd(valores_cn)
)

write_csv(cn_stats, salida_cn_stats)


# 9. P0 MEDIO PONDERADO POR SUPERFICIE ====

cat("\nCalculando P0 medio ponderado...\n")

campos_p0 <- names(p0_layer)

if (!(campo_p0 %in% campos_p0)) {
  stop(
    "No existe el campo ", campo_p0, " en la capa P0/CN. Campos disponibles: ",
    paste(campos_p0, collapse = ", ")
  )
}

if (!(campo_area_p0 %in% campos_p0)) {
  warning(
    "No existe el campo ", campo_area_p0, " en la capa P0/CN. ",
    "Se recalcula a partir de la geometria."
  )
  p0_layer[[campo_area_p0]] <- as.numeric(st_area(p0_layer))
}

p0_layer <- p0_layer %>%
  mutate(
    PO_MM = as.numeric(.data[[campo_p0]]),
    area_i = as.numeric(.data[[campo_area_p0]])
  ) %>%
  filter(
    is.finite(PO_MM),
    is.finite(area_i),
    area_i > 0
  )

if (nrow(p0_layer) == 0) {
  stop("No hay poligonos validos con P0 y area.")
}

PO_ponderado_mm <- sum(
  p0_layer$PO_MM * p0_layer$area_i,
  na.rm = TRUE
) / sum(
  p0_layer$area_i,
  na.rm = TRUE
)

p0_stats <- tibble(
  variable = "P0",
  n_poligonos = nrow(p0_layer),
  area_total_m2 = sum(p0_layer$area_i, na.rm = TRUE),
  P0_ponderado_mm = PO_ponderado_mm,
  P0_min = min(p0_layer$PO_MM, na.rm = TRUE),
  P0_media_aritmetica = mean(p0_layer$PO_MM, na.rm = TRUE),
  P0_mediana = median(p0_layer$PO_MM, na.rm = TRUE),
  P0_max = max(p0_layer$PO_MM, na.rm = TRUE),
  P0_sd = sd(p0_layer$PO_MM, na.rm = TRUE)
)

write_csv(p0_stats, salida_p0_stats)

cat("\nP0 ponderado:\n")
cat(round(PO_ponderado_mm, 3), "mm\n")


# 10. ESTADISTICOS DE MANNING ====

cat("\nCalculando Manning ponderado...\n")

campos_manning <- names(manning)

if (!(campo_manning %in% campos_manning)) {
  stop(
    "No existe el campo ", campo_manning, " en la capa Manning. Campos disponibles: ",
    paste(campos_manning, collapse = ", ")
  )
}

if (!(campo_area_manning %in% campos_manning)) {
  cat("\nNo existe ", campo_area_manning, ". Se recalcula a partir de geometria.\n", sep = "")
  manning[[campo_area_manning]] <- as.numeric(st_area(manning))
}

if (!(campo_cobertura_manning %in% campos_manning)) {
  warning(
    "No existe el campo ", campo_cobertura_manning,
    ". Se usara una categoria generica."
  )
  manning[[campo_cobertura_manning]] <- "sin_cobertura"
}

manning <- manning %>%
  mutate(
    MANNING_N = as.numeric(.data[[campo_manning]]),
    AREA_M2 = as.numeric(.data[[campo_area_manning]]),
    ID_COBERTURA_MAX = as.character(.data[[campo_cobertura_manning]])
  ) %>%
  filter(
    is.finite(MANNING_N),
    is.finite(AREA_M2),
    AREA_M2 > 0
  )

if (nrow(manning) == 0) {
  stop("No hay poligonos validos con Manning y area.")
}

manning_ponderado <- sum(manning$MANNING_N * manning$AREA_M2) /
  sum(manning$AREA_M2)

manning_stats <- manning %>%
  st_drop_geometry() %>%
  group_by(ID_COBERTURA_MAX, MANNING_N) %>%
  summarise(
    area_m2 = sum(AREA_M2, na.rm = TRUE),
    area_ha = area_m2 / 10000,
    .groups = "drop"
  ) %>%
  mutate(
    porcentaje = 100 * area_m2 / sum(area_m2, na.rm = TRUE)
  ) %>%
  arrange(desc(area_m2))

write_csv(manning_stats, salida_manning_stats)


# 11. TABLA RESUMEN GENERAL ====

resumen <- tibble(
  area_cuenca_m2 = area_cuenca_m2,
  area_cuenca_ha = area_cuenca_ha,
  area_cuenca_km2 = area_cuenca_km2,
  CN_medio = cn_stats$media,
  CN_mediano = cn_stats$mediana,
  CN_p10 = cn_stats$p10,
  CN_p90 = cn_stats$p90,
  CN_p95 = cn_stats$p95,
  P0_ponderado_mm = PO_ponderado_mm,
  Manning_n_ponderado = manning_ponderado
)

write_csv(resumen, salida_resumen)


# 12. FIGURAS ====

df_cn <- tibble(CN = valores_cn)

g_cn <- ggplot(df_cn, aes(x = CN)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Distribucion de Curve Number (CN) en la cuenca",
    x = "CN",
    y = "Frecuencia"
  ) +
  theme_minimal()

ggsave(
  salida_fig_cn,
  g_cn,
  width = 8,
  height = 5,
  dpi = 300
)

print(g_cn)


g_manning <- ggplot(
  manning_stats,
  aes(
    x = reorder(as.factor(ID_COBERTURA_MAX), area_ha),
    y = area_ha,
    fill = MANNING_N
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Superficie por cobertura dominante SIOSE y Manning n",
    x = "ID_COBERTURA_MAX",
    y = "Superficie (ha)",
    fill = "Manning n"
  ) +
  theme_minimal()

ggsave(
  salida_fig_manning,
  g_manning,
  width = 8,
  height = 6,
  dpi = 300
)

print(g_manning)


# 13. RESUMEN EN CONSOLA ====

cat("\n====================================================\n")
cat("SCRIPT 005 FINALIZADO\n")
cat("====================================================\n")

cat("\nGeoPackage del proyecto:\n")
cat(gpkg_proyecto, "\n")

cat("\nCapas usadas/importadas:\n")
cat("Cuenca:", capa_cuenca, "\n")
cat("Manning:", capa_manning, "\n")
cat("P0/CN:", capa_p0, "\n")

cat("\nArea de cuenca:\n")
cat(round(area_cuenca_ha, 3), "ha\n")
cat(round(area_cuenca_km2, 4), "km2\n")

cat("\nEstadisticos CN:\n")
print(cn_stats)

cat("\nP0 ponderado:\n")
cat(round(PO_ponderado_mm, 4), "mm\n")

cat("\nManning ponderado:\n")
cat(round(manning_ponderado, 4), "\n")

cat("\nTablas guardadas:\n")
cat(salida_resumen, "\n")
cat(salida_cn_stats, "\n")
cat(salida_p0_stats, "\n")
cat(salida_manning_stats, "\n")

cat("\nFiguras guardadas:\n")
cat(salida_fig_cn, "\n")
cat(salida_fig_manning, "\n")

cat("\nCapas actualmente disponibles en el GeoPackage:\n")
print(listar_capas_gpkg())

cat("====================================================\n")
