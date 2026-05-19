############################################################
#
# 005_parametros_hidrologicos_usos_suelo.R
#
# Objetivo:
# - Leer la cuenca delimitada
# - Leer el raster CN generado por SIOSE2CN
# - Leer la capa vectorial Manning generada por SIOSE2Manning
# - Calcular CN medio, percentiles y distribución
# - Calcular Manning ponderado por superficie
# - Exportar tablas y figuras
#
############################################################


# 1. PAQUETES ====

paquetes <- c("terra", "sf", "dplyr", "readr", "ggplot2")

instalar <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(instalar) > 0) install.packages(instalar)

library(terra)
library(sf)
library(dplyr)
library(readr)
library(ggplot2)


# 2. CONFIGURACION GENERAL ====

if (file.exists("scripts/00_configuracion.R")) {
  source("scripts/00_configuracion.R")
}

epsg_objetivo <- 25830

# 3. RUTAS EDITABLES ====

archivo_cuenca <- "salidas/mapas/cuenca.gpkg"

# Raster generado por SIOSE2CN
archivo_cn <- "salidas/mapas/NC_CURSO.tif"

# Vector generado por SIOSE2Manning
archivo_manning <- "salidas/mapas/SIOSE_Manning_AOI.gpkg"

# Salidas
salida_resumen <- "salidas/tablas/parametros_hidrologicos_resumen.csv"
salida_cn_stats <- "salidas/tablas/estadisticos_CN.csv"
salida_manning_stats <- "salidas/tablas/estadisticos_Manning.csv"
salida_fig_cn <- "salidas/figuras/histograma_CN.png"
salida_fig_manning <- "salidas/figuras/manning_por_cobertura.png"

dir.create("salidas/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/figuras", recursive = TRUE, showWarnings = FALSE)

# 4. LEER DATOS ====

if (!file.exists(archivo_cuenca)) {
  stop("No se encuentra la capa de cuenca: ", archivo_cuenca)
}

if (!file.exists(archivo_cn)) {
  stop("No se encuentra el raster CN: ", archivo_cn)
}

if (!file.exists(archivo_manning)) {
  stop("No se encuentra la capa Manning: ", archivo_manning)
}

cuenca <- st_read(archivo_cuenca, quiet = TRUE)
cn <- rast(archivo_cn)
manning <- st_read(archivo_manning, quiet = TRUE)

cat("\nCuenca:\n")
print(cuenca)

cat("\nRaster CN:\n")
print(cn)

cat("\nCapa Manning:\n")
print(manning)

# 5. COMPROBAR CRS ====

if (is.na(st_crs(cuenca))) {
  stop("La capa de cuenca no tiene CRS definido.")
}

if (is.na(crs(cn))) {
  stop("El raster CN no tiene CRS definido.")
}

if (is.na(st_crs(manning))) {
  stop("La capa Manning no tiene CRS definido.")
}

if (st_crs(cuenca)$epsg != epsg_objetivo) {
  cuenca <- st_transform(cuenca, epsg_objetivo)
}

if (st_crs(manning)$epsg != epsg_objetivo) {
  manning <- st_transform(manning, epsg_objetivo)
}

# 6. AREA DE CUENCA ====

cuenca <- st_make_valid(cuenca)

area_cuenca_m2 <- sum(as.numeric(st_area(cuenca)))
area_cuenca_ha <- area_cuenca_m2 / 10000
area_cuenca_km2 <- area_cuenca_m2 / 1e6

cat("\nArea de cuenca:\n")
cat(round(area_cuenca_ha, 3), "ha\n")
cat(round(area_cuenca_km2, 4), "km2\n")



# 7. ESTADISTICOS DEL RASTER CN ====

cat("\nCalculando estadísticos del CN...\n")

cuenca_vect <- vect(cuenca)

cn_crop <- crop(cn, cuenca_vect)
cn_mask <- mask(cn_crop, cuenca_vect)

valores_cn <- values(cn_mask, na.rm = TRUE)
valores_cn <- as.numeric(valores_cn[, 1])

valores_cn <- valores_cn[!is.na(valores_cn)]

if (length(valores_cn) == 0) {
  stop("No se han encontrado valores válidos de CN dentro de la cuenca.")
}

cn_stats <- tibble::tibble(
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



# 8. P0 MEDIO PONDERADO POR SUPERFICIE ====

cat("\nCalculando P0 medio ponderado...\n")

archivo_pO <- "salidas/mapas/CAPA_FINAL_CN.gpkg"

if (!file.exists(archivo_pO)) {
  stop("No se encuentra la capa con PO: ", archivo_pO)
}

pO_layer <- st_read(archivo_pO, quiet = TRUE)

campos_pO <- names(pO_layer)

if (!("PO_MM" %in% campos_pO)) {
  stop(
    "No existe el campo PO_MM en la capa P0. Campos disponibles: ",
    paste(campos_pO, collapse = ", ")
  )
}

if (!("area_i" %in% campos_pO)) {
  stop(
    "No existe el campo area_i en la capa P0. Campos disponibles: ",
    paste(campos_pO, collapse = ", ")
  )
}

pO_layer <- pO_layer %>%
  mutate(
    PO_MM = as.numeric(PO_MM),
    area_i = as.numeric(area_i)
  ) %>%
  filter(
    !is.na(PO_MM),
    !is.na(area_i),
    area_i > 0
  )

if (nrow(pO_layer) == 0) {
  stop("No hay polígonos válidos con PO_MM y area_i.")
}

# CALCULO DE P0 PONDERADO 

PO_ponderado_mm <- sum(
  pO_layer$PO_MM * pO_layer$area_i,
  na.rm = TRUE
) / sum(
  pO_layer$area_i,
  na.rm = TRUE
)



# ESTADISTICOS P0

pO_stats <- tibble::tibble(
  variable = "PO",
  n_poligonos = nrow(pO_layer),
  area_total_m2 = sum(pO_layer$area_i, na.rm = TRUE),
  PO_ponderado_mm = PO_ponderado_mm,
  PO_min = min(pO_layer$PO_MM, na.rm = TRUE),
  PO_media_aritmetica = mean(pO_layer$PO_MM, na.rm = TRUE),
  PO_mediana = median(pO_layer$PO_MM, na.rm = TRUE),
  PO_max = max(pO_layer$PO_MM, na.rm = TRUE),
  PO_sd = sd(pO_layer$PO_MM, na.rm = TRUE)
)

salida_pO_stats <- "salidas/tablas/estadisticos_P0.csv"

write_csv(
  pO_stats,
  salida_pO_stats
)

cat("\nPO ponderado:\n")
cat(round(PO_ponderado_mm, 3), "mm\n")

# 9. ESTADISTICOS DE MANNING ====

cat("\nCalculando Manning ponderado...\n")

campos_manning <- names(manning)

if (!("MANNING_N" %in% campos_manning)) {
  stop(
    "No existe el campo MANNING_N en la capa Manning. Campos disponibles: ",
    paste(campos_manning, collapse = ", ")
  )
}

if (!("AREA_M2" %in% campos_manning)) {
  cat("\nNo existe AREA_M2. Se recalcula a partir de geometría.\n")
  manning$AREA_M2 <- as.numeric(st_area(manning))
}

manning <- manning %>%
  mutate(
    MANNING_N = as.numeric(MANNING_N),
    AREA_M2 = as.numeric(AREA_M2)
  ) %>%
  filter(!is.na(MANNING_N), !is.na(AREA_M2), AREA_M2 > 0)

if (nrow(manning) == 0) {
  stop("No hay polígonos válidos con Manning y área.")
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



# 10. TABLA RESUMEN GENERAL ====

resumen <- tibble::tibble(
  area_cuenca_m2 = area_cuenca_m2,
  area_cuenca_ha = area_cuenca_ha,
  area_cuenca_km2 = area_cuenca_km2,
  CN_medio = cn_stats$media,
  CN_mediano = cn_stats$mediana,
  CN_p10 = cn_stats$p10,
  CN_p90 = cn_stats$p90,
  CN_p95 = cn_stats$p95,
  Manning_n_ponderado = manning_ponderado
)

write_csv(resumen, salida_resumen)



# 11. FIGURAS ====

df_cn <- tibble::tibble(CN = valores_cn)

g_cn <- ggplot(df_cn, aes(x = CN)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    title = "Distribución de Curve Number (CN) en la cuenca",
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
g_cn


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
g_manning

ggsave(
  salida_fig_manning,
  g_manning,
  width = 8,
  height = 6,
  dpi = 300
)

# 12. RESUMEN EN CONSOLA ====

cat("\n====================================================\n")
cat("SCRIPT 005 FINALIZADO\n")
cat("====================================================\n")

cat("\nArea de cuenca:\n")
cat(round(area_cuenca_ha, 3), "ha\n")
cat(round(area_cuenca_km2, 4), "km2\n")

cat("\nEstadísticos CN:\n")
print(cn_stats)

cat("\nManning ponderado:\n")
cat(round(manning_ponderado, 4), "\n")

cat("\nTabla resumen guardada en:\n")
cat(salida_resumen, "\n")

cat("\nTabla CN guardada en:\n")
cat(salida_cn_stats, "\n")

cat("\nTabla Manning guardada en:\n")
cat(salida_manning_stats, "\n")

cat("\nFiguras guardadas en:\n")
cat(salida_fig_cn, "\n")
cat(salida_fig_manning, "\n")

cat("====================================================\n")

