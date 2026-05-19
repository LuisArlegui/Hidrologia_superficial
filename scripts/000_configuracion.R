############################################################
#
# 00_configuracion.R
#
# Configuracion general del proyecto cartuja_hidro
#
############################################################

# ==========================================================
# 1. PAQUETES
# ==========================================================

paquetes <- c(
  "terra",
  "sf",
  "dplyr",
  "ggplot2",
  "whitebox"
)

instalar <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(instalar) > 0) install.packages(instalar)

lapply(paquetes, library, character.only = TRUE)

# ==========================================================
# 2. CRS Y PARAMETROS GENERALES
# ==========================================================

epsg_objetivo <- 25830
crs_objetivo  <- "EPSG:25830"

# Resolucion esperada del DTM, en metros
resolucion_dtm_m <- 5

# Semilla para procesos reproducibles
set.seed(1234)

# ==========================================================
# 3. RUTAS DEL PROYECTO
# ==========================================================

# Al trabajar dentro del .Rproj, getwd() debe ser la raiz del proyecto
ruta_proyecto <- getwd()

ruta_datos_brutos     <- file.path(ruta_proyecto, "datos", "brutos")
ruta_datos_procesados <- file.path(ruta_proyecto, "datos", "procesados")
ruta_datos_auxiliares <- file.path(ruta_proyecto, "datos", "auxiliares")

ruta_scripts <- file.path(ruta_proyecto, "scripts")

ruta_salidas         <- file.path(ruta_proyecto, "salidas")
ruta_salidas_figuras <- file.path(ruta_proyecto, "salidas", "figuras")
ruta_salidas_tablas  <- file.path(ruta_proyecto, "salidas", "tablas")
ruta_salidas_control <- file.path(ruta_proyecto, "salidas", "control")
ruta_salidas_mapas   <- file.path(ruta_proyecto, "salidas", "mapas")

# ==========================================================
# 4. ARCHIVOS PRINCIPALES DEL WORKFLOW
# ==========================================================

archivo_dtm_recortado <- file.path(ruta_datos_brutos, "dtm_recortado.tif")
archivo_cauces_quemar <- file.path(ruta_datos_brutos, "cauces_a_quemar.gpkg")

archivo_dtm_hidrocorregido <- file.path(
  ruta_datos_procesados,
  "dtm_hidrocorregido.tif"
)

# ==========================================================
# 5. MENSAJE DE CONTROL
# ==========================================================

cat("\nProyecto cartuja_hidro configurado correctamente.\n")
cat("Directorio raiz:\n")
cat(ruta_proyecto, "\n")
