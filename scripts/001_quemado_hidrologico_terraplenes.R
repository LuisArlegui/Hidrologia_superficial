############################################################
#
#   Quemado de cauces en DTM
#   Corrección hidrológica de cruces bajo terraplén
#   Cota objetivo variable desde atributos del shapefile
#   EPSG:25830 - ETRS89 / UTM zone 30N
#
############################################################

#-----------------------------------------------------------
# 1. Paquetes
#-----------------------------------------------------------

if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")
library(terra)

#-----------------------------------------------------------
# 2. Archivos de entrada
#-----------------------------------------------------------

ruta_dtm    <- "datos/procesados/DTM_COMBI.tif"
ruta_cauces <- "datos/auxiliares/cruces_bajo_terraplen.shp"

ruta_salida <- "salidas/mapas/DTM_5m_quemado.tif"

if (!dir.exists(dirname(ruta_salida))) {
  dir.create(dirname(ruta_salida), recursive = TRUE)
}

#-----------------------------------------------------------
# 3. Parámetros de quemado
#-----------------------------------------------------------

epsg_objetivo <- "EPSG:25830"

# Este campo contiene la COTA OBJETIVO del cauce, no la profundidad
campo_cota_objetivo <- "prof_burn"

ancho_quemado_m <- 10

# Si TRUE, nunca eleva el DTM: solo baja donde la cota objetivo
# sea menor que la cota original.
solo_bajar <- TRUE

#-----------------------------------------------------------
# 4. Cargar DTM y cauces
#-----------------------------------------------------------

dtm <- rast(ruta_dtm)
cauces <- vect(ruta_cauces)

if (is.na(crs(dtm))) {
  crs(dtm) <- epsg_objetivo
}

if (is.na(crs(cauces))) {
  crs(cauces) <- epsg_objetivo
}

if (crs(cauces) != crs(dtm)) {
  cauces <- project(cauces, crs(dtm))
}

#-----------------------------------------------------------
# 5. Comprobar campo de cota objetivo
#-----------------------------------------------------------

if (!(campo_cota_objetivo %in% names(cauces))) {
  stop(
    paste0(
      "El campo '", campo_cota_objetivo,
      "' no existe en la capa de cauces.\nCampos disponibles: ",
      paste(names(cauces), collapse = ", ")
    )
  )
}

valores_cota <- cauces[[campo_cota_objetivo]][, 1]

if (!is.numeric(valores_cota)) {
  stop(
    paste0(
      "El campo '", campo_cota_objetivo,
      "' debe ser numérico y contener la cota objetivo en metros."
    )
  )
}

if (any(is.na(valores_cota))) {
  stop("Hay valores NA en el campo de cota objetivo.")
}

#-----------------------------------------------------------
# 6. Crear buffer de quemado
#-----------------------------------------------------------

cauces_buffer <- buffer(cauces, width = ancho_quemado_m / 2)

#-----------------------------------------------------------
# 7. Rasterizar cota objetivo
#-----------------------------------------------------------

r_cota_objetivo <- rasterize(
  cauces_buffer,
  dtm,
  field = campo_cota_objetivo,
  background = NA
)

#-----------------------------------------------------------
# 8. Aplicar quemado por sustitución de cota
#-----------------------------------------------------------

if (solo_bajar) {
  dtm_quemado <- ifel(
    !is.na(r_cota_objetivo),
    pmin(dtm, r_cota_objetivo),
    dtm
  )
} else {
  dtm_quemado <- ifel(
    !is.na(r_cota_objetivo),
    r_cota_objetivo,
    dtm
  )
}

#-----------------------------------------------------------
# 9. Guardar resultado
#-----------------------------------------------------------

writeRaster(
  dtm_quemado,
  ruta_salida,
  overwrite = TRUE,
  gdal = c("COMPRESS=LZW")
)

#-----------------------------------------------------------
# 10. Comprobación rápida
#-----------------------------------------------------------

plot(dtm, main = "DTM original")
plot(cauces, add = TRUE, col = "blue", lwd = 2)

plot(dtm_quemado, main = "DTM con cauces quemados por cota objetivo")
plot(cauces, add = TRUE, col = "red", lwd = 2)

cat("\nDTM quemado guardado en:\n", ruta_salida, "\n")
cat("Campo usado como cota objetivo:", campo_cota_objetivo, "\n")
cat("Solo bajar terreno:", solo_bajar, "\n")