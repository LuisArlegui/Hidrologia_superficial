############################################################
#
# 002_importar_resultados_QGIS_a_gpkg.R
#
# Objetivo:
# - Integrar en el GeoPackage único del proyecto las capas
#   vectoriales generadas en QGIS/SAGA tras:
#     1) Fill sinks (Wang & Liu)
#     2) cálculo de red de drenaje / canales
#     3) delimitación de cuenca
#     4) definición de exutorio, si existe
# - Mantener los raster como GeoTIFF independientes.
#
# Entradas esperadas:
# - salidas/mapas/DTM_Burnt_Filled_Clipped.tif
# - salidas/mapas/canales.gpkg, canales.shp u otro equivalente
# - salidas/mapas/cuenca.gpkg, cuenca.shp u otro equivalente
# - opcionalmente salidas/mapas/exutorio.gpkg, exutorio.shp
# - opcionalmente salidas/mapas/strahler.gpkg, strahler.shp
#
# Salidas:
# - datos/procesados/hidrologia_superficial.gpkg
#   con capas:
#     red_drenaje
#     cuenca
#     exutorio, si existe
#     strahler, si existe
#
############################################################


# 0. PAQUETES ====

paquetes <- c("sf", "dplyr")

instalar <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(instalar) > 0) install.packages(instalar)

library(sf)
library(dplyr)


# 1. CONFIGURACION GENERAL ====

if (file.exists("scripts/000_rutas_y_capas.R")) {
  source("scripts/000_rutas_y_capas.R")
} else {
  stop("No se encuentra scripts/000_rutas_y_capas.R")
}

dir.create("datos/procesados", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/mapas", recursive = TRUE, showWarnings = FALSE)


# 2. RUTAS ====

archivo_dtm_hidrologico <- "salidas/mapas/DTM_Burnt_Filled_Clipped.tif"

archivos_red_candidatos <- c(
  "salidas/mapas/canales.gpkg",
  "salidas/mapas/canales.shp",
  "salidas/mapas/red_drenaje.gpkg",
  "salidas/mapas/red_drenaje.shp",
  "datos/procesados/canales.gpkg",
  "datos/procesados/red_drenaje.gpkg"
)

archivos_cuenca_candidatos <- c(
  "salidas/mapas/cuenca.gpkg",
  "salidas/mapas/cuenca.shp",
  "datos/procesados/cuenca.gpkg",
  "datos/procesados/cuenca.shp"
)

archivos_exutorio_candidatos <- c(
  "salidas/mapas/exutorio.gpkg",
  "salidas/mapas/exutorio.shp",
  "datos/procesados/exutorio.gpkg",
  "datos/procesados/exutorio.shp"
)

archivos_strahler_candidatos <- c(
  "salidas/mapas/strahler.gpkg",
  "salidas/mapas/strahler.shp",
  "salidas/mapas/red_strahler.gpkg",
  "salidas/mapas/red_strahler.shp",
  "datos/procesados/strahler.gpkg",
  "datos/procesados/red_strahler.gpkg"
)


# 3. PARAMETROS EDITABLES ====

epsg_objetivo <- 25830
sobrescribir_capas <- TRUE
validar_geometrias <- TRUE
eliminar_ZM <- TRUE


# 4. FUNCIONES AUXILIARES ====

primer_archivo_existente <- function(candidatos, obligatorio = TRUE, etiqueta = "archivo") {
  existe <- candidatos[file.exists(candidatos)]

  if (length(existe) > 0) {
    return(existe[1])
  }

  if (obligatorio) {
    stop(
      "No se encontro ", etiqueta, ". Candidatos:\n",
      paste(candidatos, collapse = "\n")
    )
  }

  NA_character_
}


leer_y_preparar_vector <- function(archivo,
                                   epsg_objetivo = 25830,
                                   validar_geometrias = TRUE,
                                   eliminar_ZM = TRUE) {

  x <- sf::st_read(archivo, quiet = TRUE)

  if (eliminar_ZM) {
    x <- sf::st_zm(x, drop = TRUE, what = "ZM")
  }

  if (is.na(sf::st_crs(x))) {
    sf::st_crs(x) <- epsg_objetivo
  }

  if (validar_geometrias) {
    x <- sf::st_make_valid(x)
  }

  x
}


guardar_vector_en_gpkg <- function(archivo,
                                   layer,
                                   obligatorio = TRUE,
                                   etiqueta = layer) {

  if (is.na(archivo) || !file.exists(archivo)) {
    if (obligatorio) {
      stop("No se encuentra el archivo obligatorio para ", etiqueta)
    } else {
      message("No se encontro archivo opcional para ", etiqueta, ". Se omite.")
      return(invisible(NULL))
    }
  }

  cat("\nImportando ", etiqueta, ":\n", archivo, "\n", sep = "")

  x <- leer_y_preparar_vector(
    archivo = archivo,
    epsg_objetivo = epsg_objetivo,
    validar_geometrias = validar_geometrias,
    eliminar_ZM = eliminar_ZM
  )

  guardar_capa_gpkg(
    obj = x,
    layer = layer,
    overwrite = sobrescribir_capas
  )

  cat("Guardado en GeoPackage como capa: ", layer, "\n", sep = "")

  invisible(x)
}


# 5. COMPROBAR DTM HIDROLOGICO ====

if (!file.exists(archivo_dtm_hidrologico)) {
  warning(
    "No se encuentra el DTM hidrologico final:\n",
    archivo_dtm_hidrologico,
    "\nEste script puede importar capas vectoriales, pero conviene revisar ",
    "que ya se haya ejecutado Fill sinks (Wang & Liu) en QGIS."
  )
} else {
  cat("\nDTM hidrologico encontrado:\n")
  cat(archivo_dtm_hidrologico, "\n")
}


# 6. LOCALIZAR ARCHIVOS VECTORIALES ====

archivo_red <- primer_archivo_existente(
  archivos_red_candidatos,
  obligatorio = TRUE,
  etiqueta = "red de drenaje / canales"
)

archivo_cuenca <- primer_archivo_existente(
  archivos_cuenca_candidatos,
  obligatorio = TRUE,
  etiqueta = "cuenca"
)

archivo_exutorio <- primer_archivo_existente(
  archivos_exutorio_candidatos,
  obligatorio = FALSE,
  etiqueta = "exutorio"
)

archivo_strahler <- primer_archivo_existente(
  archivos_strahler_candidatos,
  obligatorio = FALSE,
  etiqueta = "red Strahler"
)


# 7. IMPORTAR CAPAS AL GEOPACKAGE UNICO ====

red_drenaje <- guardar_vector_en_gpkg(
  archivo = archivo_red,
  layer = capas_gpkg$red_drenaje,
  obligatorio = TRUE,
  etiqueta = "red de drenaje"
)

cuenca <- guardar_vector_en_gpkg(
  archivo = archivo_cuenca,
  layer = capas_gpkg$cuenca,
  obligatorio = TRUE,
  etiqueta = "cuenca"
)

if (!is.na(archivo_exutorio)) {
  exutorio <- guardar_vector_en_gpkg(
    archivo = archivo_exutorio,
    layer = capas_gpkg$exutorio,
    obligatorio = FALSE,
    etiqueta = "exutorio"
  )
}

if (!is.na(archivo_strahler)) {
  strahler <- guardar_vector_en_gpkg(
    archivo = archivo_strahler,
    layer = capas_gpkg$strahler,
    obligatorio = FALSE,
    etiqueta = "strahler"
  )
}


# 8. COMPROBACIONES BASICAS ====

cat("\nResumen de capas importadas:\n")

if (exists("red_drenaje")) {
  cat("red_drenaje: ", nrow(red_drenaje), " entidades\n", sep = "")
}

if (exists("cuenca")) {
  cat("cuenca: ", nrow(cuenca), " entidades\n", sep = "")
}

if (exists("exutorio")) {
  cat("exutorio: ", nrow(exutorio), " entidades\n", sep = "")
}

if (exists("strahler")) {
  cat("strahler: ", nrow(strahler), " entidades\n", sep = "")
}


# 9. RESUMEN EN CONSOLA ====

cat("\n====================================================\n")
cat("SCRIPT 002 FINALIZADO\n")
cat("====================================================\n")

cat("\nGeoPackage del proyecto:\n")
cat(gpkg_proyecto, "\n")

cat("\nDTM hidrologico final:\n")
cat(archivo_dtm_hidrologico, "\n")

cat("\nArchivos importados:\n")
cat("Red de drenaje:", archivo_red, "\n")
cat("Cuenca:", archivo_cuenca, "\n")
cat("Exutorio:", ifelse(is.na(archivo_exutorio), "no encontrado", archivo_exutorio), "\n")
cat("Strahler:", ifelse(is.na(archivo_strahler), "no encontrado", archivo_strahler), "\n")

cat("\nCapas disponibles en el GeoPackage:\n")
print(listar_capas_gpkg())

cat("====================================================\n")
