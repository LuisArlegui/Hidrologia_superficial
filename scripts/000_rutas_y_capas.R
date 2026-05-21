############################################################
#
# 000_rutas_y_capas.R
#
# Objetivo:
# - Centralizar rutas del proyecto hidrológico
# - Definir el GeoPackage vectorial del proyecto
# - Crear funciones auxiliares para leer y guardar capas
#
# Nota:
# - Este script NO crea el GeoPackage.
# - El GPKG se creará automáticamente cuando el primer
#   script escriba una capa vectorial real.
#
############################################################


# 0. PAQUETES ====

if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
library(sf)


# 1. RUTA DEL GEOPACKAGE DEL PROYECTO ====

gpkg_proyecto <- "datos/procesados/hidrologia_superficial.gpkg"

dir.create(dirname(gpkg_proyecto), recursive = TRUE, showWarnings = FALSE)


# 2. CAPAS VECTORIALES ESTANDAR ====

capas_gpkg <- list(
  cuenca                        = "cuenca",
  exutorio                      = "exutorio",
  red_drenaje                   = "red_drenaje",
  cauces_quemados               = "cauces_quemados",
  cruces_bajo_terraplen         = "cruces_bajo_terraplen",
  cruces_bajo_terraplen_buffer  = "cruces_bajo_terraplen_buffer",
  subcuencas                    = "subcuencas",
  strahler                      = "strahler",
  puntos_control                = "puntos_control"
)


# 3. FUNCIONES AUXILIARES ====

existe_gpkg <- function(gpkg = gpkg_proyecto) {
  file.exists(gpkg)
}


existe_capa_gpkg <- function(layer, gpkg = gpkg_proyecto) {
  
  if (!file.exists(gpkg)) return(FALSE)
  
  capas <- tryCatch(
    sf::st_layers(gpkg)$name,
    error = function(e) character(0)
  )
  
  layer %in% capas
}


leer_capa_gpkg <- function(layer,
                           gpkg = gpkg_proyecto,
                           quiet = TRUE) {
  
  if (!file.exists(gpkg)) {
    stop("No existe el GeoPackage del proyecto: ", gpkg)
  }
  
  capas <- tryCatch(
    sf::st_layers(gpkg)$name,
    error = function(e) {
      stop("El GeoPackage no parece válido: ", gpkg)
    }
  )
  
  if (!(layer %in% capas)) {
    stop(
      "No existe la capa '", layer, "' en ", gpkg, ".\n",
      "Capas disponibles: ",
      paste(capas, collapse = ", ")
    )
  }
  
  sf::st_read(
    dsn = gpkg,
    layer = layer,
    quiet = quiet
  )
}


guardar_capa_gpkg <- function(obj,
                              layer,
                              gpkg = gpkg_proyecto,
                              overwrite = TRUE) {
  
  dir.create(dirname(gpkg),
             recursive = TRUE,
             showWarnings = FALSE)
  
  sf::st_write(
    obj,
    dsn = gpkg,
    layer = layer,
    delete_layer = overwrite,
    quiet = TRUE
  )
  
  invisible(gpkg)
}


borrar_capa_gpkg <- function(layer,
                             gpkg = gpkg_proyecto) {
  
  if (!existe_capa_gpkg(layer, gpkg)) {
    return(invisible(FALSE))
  }
  
  sf::st_delete(
    dsn = gpkg,
    layer = layer,
    quiet = TRUE
  )
  
  invisible(TRUE)
}


listar_capas_gpkg <- function(gpkg = gpkg_proyecto) {
  
  if (!file.exists(gpkg)) {
    message("Todavía no existe el GeoPackage: ", gpkg)
    return(invisible(NULL))
  }
  
  sf::st_layers(gpkg)
}