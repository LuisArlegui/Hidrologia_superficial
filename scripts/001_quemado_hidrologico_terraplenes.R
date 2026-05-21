############################################################
#
# 001_quemado_hidrologico_terraplenes.R
#
# Objetivo:
# - Leer un DTM en GeoTIFF
# - Leer la capa de cruces bajo terraplén desde shapefile
# - Crear, si no existe, el GeoPackage vectorial del proyecto
# - Guardar los cruces como capa interna del GeoPackage
# - Quemar el DTM usando una cota objetivo variable
# - Guardar el DTM quemado como GeoTIFF
#
############################################################


# 0. PAQUETES ====

if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")

library(terra)
library(sf)


# 1. CONFIGURACION GENERAL ====

if (file.exists("scripts/000_rutas_y_capas.R")) {
  source("scripts/000_rutas_y_capas.R")
} else {
  stop("No se encuentra scripts/000_rutas_y_capas.R")
}

dir.create("datos/procesados", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/mapas", recursive = TRUE, showWarnings = FALSE)


# 2. RUTAS ====

ruta_dtm <- "datos/procesados/DTM_COMBI.tif"
ruta_cauces_shp <- "datos/auxiliares/cruces_bajo_terraplen.shp"

ruta_salida_dtm <- "salidas/mapas/DTM_5m_quemado.tif"


# 3. CAPAS GPKG ====

capa_cruces <- capas_gpkg$cruces_bajo_terraplen
capa_buffer <- capas_gpkg$cruces_bajo_terraplen_buffer


# 4. PARAMETROS ====

epsg_objetivo <- "EPSG:25830"

campo_cota_objetivo <- "prof_burn"

ancho_quemado_m <- 10

solo_bajar <- TRUE

guardar_buffer_gpkg <- TRUE


# 5. COMPROBAR ARCHIVOS ====

if (!file.exists(ruta_dtm)) {
  stop("No se encuentra el DTM: ", ruta_dtm)
}

if (!file.exists(ruta_cauces_shp)) {
  stop("No se encuentra la capa shapefile de cauces: ", ruta_cauces_shp)
}


# 6. CARGAR DTM ====

dtm <- rast(ruta_dtm)

if (is.na(crs(dtm))) {
  crs(dtm) <- epsg_objetivo
}


# 7. LEER SHAPEFILE DE CRUCES Y GUARDARLO EN EL GPKG ====

cauces_sf <- st_read(ruta_cauces_shp, quiet = TRUE)

if (is.na(st_crs(cauces_sf))) {
  st_crs(cauces_sf) <- 25830
}

cauces_sf <- st_transform(cauces_sf, crs(dtm))

guardar_capa_gpkg(
  obj = cauces_sf,
  layer = capa_cruces,
  overwrite = TRUE
)

cat("\nCapa guardada en el GeoPackage:\n")
cat(gpkg_proyecto, " | layer =", capa_cruces, "\n")


# 8. CONVERTIR A TERRA ====

cauces <- vect(cauces_sf)


# 9. COMPROBAR CAMPO DE COTA OBJETIVO ====

if (!(campo_cota_objetivo %in% names(cauces))) {
  stop(
    "El campo '", campo_cota_objetivo,
    "' no existe en la capa de cauces.\nCampos disponibles: ",
    paste(names(cauces), collapse = ", ")
  )
}

valores_cota <- cauces[[campo_cota_objetivo]][, 1]

if (!is.numeric(valores_cota)) {
  stop(
    "El campo '", campo_cota_objetivo,
    "' debe ser numérico y contener la cota objetivo en metros."
  )
}

if (any(is.na(valores_cota))) {
  stop("Hay valores NA en el campo de cota objetivo.")
}


# 10. CREAR BUFFER DE QUEMADO ====

cauces_buffer <- buffer(cauces, width = ancho_quemado_m / 2)

if (guardar_buffer_gpkg) {
  cauces_buffer_sf <- st_as_sf(cauces_buffer)
  
  guardar_capa_gpkg(
    obj = cauces_buffer_sf,
    layer = capa_buffer,
    overwrite = TRUE
  )
  
  cat("\nBuffer guardado en el GeoPackage:\n")
  cat(gpkg_proyecto, " | layer =", capa_buffer, "\n")
}


# 11. RASTERIZAR COTA OBJETIVO ====

r_cota_objetivo <- rasterize(
  cauces_buffer,
  dtm,
  field = campo_cota_objetivo,
  background = NA
)


# 12. APLICAR QUEMADO ====

if (solo_bajar) {
  
  dtm_min <- min(dtm, r_cota_objetivo, na.rm = TRUE)
  
  dtm_quemado <- cover(
    dtm_min,
    dtm
  )
  
} else {
  
  dtm_quemado <- cover(
    r_cota_objetivo,
    dtm
  )
}


# 13. GUARDAR DTM QUEMADO ====

writeRaster(
  dtm_quemado,
  ruta_salida_dtm,
  overwrite = TRUE,
  gdal = c("COMPRESS=LZW")
)


# 14. COMPROBACION RAPIDA ====

plot(dtm, main = "DTM original")
plot(cauces, add = TRUE, col = "blue", lwd = 2)

plot(dtm_quemado, main = "DTM con cauces quemados por cota objetivo")
plot(cauces, add = TRUE, col = "red", lwd = 2)


# 15. RESUMEN ====

cat("\n====================================================\n")
cat("SCRIPT 001 FINALIZADO\n")
cat("====================================================\n")

cat("\nGeoPackage del proyecto:\n")
cat(gpkg_proyecto, "\n")

cat("\nCapas generadas en el GeoPackage:\n")
cat(capa_cruces, "\n")
if (guardar_buffer_gpkg) cat(capa_buffer, "\n")

cat("\nDTM original:\n")
cat(ruta_dtm, "\n")

cat("\nDTM quemado guardado en:\n")
cat(ruta_salida_dtm, "\n")

cat("\nCampo usado como cota objetivo:\n")
cat(campo_cota_objetivo, "\n")

cat("\nAncho de quemado:\n")
cat(ancho_quemado_m, "m\n")

cat("\nSolo bajar terreno:\n")
cat(solo_bajar, "\n")

cat("\nCapas actualmente disponibles en el GeoPackage:\n")
print(listar_capas_gpkg())

cat("====================================================\n")
