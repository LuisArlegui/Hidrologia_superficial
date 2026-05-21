############################################################
#
# 004b_morfometria_cauce_principal.R
#
# Criterio:
# - Identificar ORDER máximo
# - Buscar el nodo de menor cota en los tramos de ORDER máximo
# - Usar ese nodo como exutorio aproximado
# - Orientar todos los tramos por cota: alto -> bajo
# - Invertir la red: bajo -> alto
# - Buscar el camino ascendente más largo desde el exutorio
#
############################################################

# 0. PAQUETES ====

paquetes <- c("sf", "terra", "dplyr", "igraph", "readr", "tibble")

instalar <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(instalar) > 0) install.packages(instalar)

library(sf)
library(terra)
library(dplyr)
library(igraph)
library(readr)
library(tibble)

# 1. CONFIGURACION ====

if (file.exists("scripts/00_configuracion.R")) {
  source("scripts/00_configuracion.R")
}

dir.create("datos/procesados", recursive = TRUE, showWarnings = FALSE)
dir.create("datos/auxiliares", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/tablas", recursive = TRUE, showWarnings = FALSE)

# 2. RUTAS ====

if (file.exists("scripts/000_rutas_y_capas.R")) {
  source("scripts/000_rutas_y_capas.R")
} else {
  stop("No se encuentra scripts/000_rutas_y_capas.R")
}

archivo_dtm <- "salidas/mapas/DTM_Burnt_Filled_Clipped.tif"

capa_canales <- capas_gpkg$red_drenaje
capa_cauce_principal <- "cauce_principal"
capa_exutorio_aprox <- "exutorio_aproximado"
capa_cabecera_cauce_principal <- "cabecera_cauce_principal"
capa_nodos_red_drenaje <- "nodos_red_drenaje"

salida_morfometria_detalle <- "salidas/tablas/004_morfometria_cauce_principal.csv"
salida_morfometria_006 <- "datos/auxiliares/004_morfometria_para_006.csv"

# 3. PARAMETROS ====

I1_Id_defecto <- 10
paso_m <- 10

# 4. LEER DATOS ====

if (!file.exists(archivo_dtm)) {
  stop("No se encuentra el DTM hidrologicamente corregido: ", archivo_dtm)
}

if (!existe_capa_gpkg(capa_canales)) {
  stop(
    "No existe la capa '", capa_canales, "' en el GeoPackage del proyecto.\n",
    "Capas disponibles:\n",
    paste(listar_capas_gpkg()$name, collapse = ", ")
  )
}

canales <- leer_capa_gpkg(capa_canales)

# Eliminar Z/M, porque GEOS no trabaja bien con XYM/XYZM
canales <- st_zm(canales, drop = TRUE, what = "ZM")
canales <- st_make_valid(canales)

dtm <- rast(archivo_dtm)

cat("\nCampos de la capa de canales:\n")
print(names(canales))

# 5. COMPROBACIONES ====

campos_necesarios <- c("SEGMENT_ID", "NODE_A", "NODE_B", "LENGTH", "ORDER")
faltan <- setdiff(campos_necesarios, names(canales))

if (length(faltan) > 0) {
  stop("Faltan campos necesarios en canales: ", paste(faltan, collapse = ", "))
}

if (is.na(st_crs(canales))) {
  stop("La capa de canales no tiene CRS definido.")
}

if (is.na(crs(dtm))) {
  stop("El DTM no tiene CRS definido.")
}

if (st_crs(canales)$wkt != crs(dtm)) {
  canales <- st_transform(canales, crs(dtm))
  canales <- st_zm(canales, drop = TRUE, what = "ZM")
}

canales <- canales %>%
  mutate(
    SEGMENT_ID = as.character(SEGMENT_ID),
    NODE_A = as.character(NODE_A),
    NODE_B = as.character(NODE_B),
    LENGTH = as.numeric(LENGTH),
    ORDER = as.numeric(ORDER)
  ) %>%
  filter(
    !is.na(NODE_A),
    !is.na(NODE_B),
    !is.na(LENGTH),
    LENGTH > 0,
    !is.na(ORDER)
  )

if (nrow(canales) == 0) {
  stop("No quedan tramos válidos tras filtrar la red.")
}

# 6. OBTENER COORDENADAS DE NODOS DESDE GEOMETRIA ====

get_line_endpoints <- function(geom) {
  coords <- st_coordinates(geom)
  coords <- as.data.frame(coords)
  data.frame(
    x_ini = coords$X[1],
    y_ini = coords$Y[1],
    x_fin = coords$X[nrow(coords)],
    y_fin = coords$Y[nrow(coords)]
  )
}

endpoints <- do.call(
  rbind,
  lapply(st_geometry(canales), get_line_endpoints)
)

canales <- bind_cols(canales, endpoints)

# Relacionar NODE_A/NODE_B con el extremo inicial/final de cada línea
# Se asume que NODE_A corresponde al primer vértice y NODE_B al último.
nodos_A <- canales %>%
  st_drop_geometry() %>%
  transmute(
    node = NODE_A,
    x = x_ini,
    y = y_ini
  )

nodos_B <- canales %>%
  st_drop_geometry() %>%
  transmute(
    node = NODE_B,
    x = x_fin,
    y = y_fin
  )

nodos <- bind_rows(nodos_A, nodos_B) %>%
  group_by(node) %>%
  summarise(
    x = mean(x, na.rm = TRUE),
    y = mean(y, na.rm = TRUE),
    .groups = "drop"
  )

nodos_sf <- st_as_sf(
  nodos,
  coords = c("x", "y"),
  crs = st_crs(canales)
)

# 7. EXTRAER COTAS DE LOS NODOS ====

z_nodos <- terra::extract(dtm, vect(nodos_sf))
nodos$z <- z_nodos[[2]]

if (all(is.na(nodos$z))) {
  stop("No se han podido extraer cotas del DTM en los nodos.")
}

# 8. BUSCAR EXUTORIO APROXIMADO ====

# ORDER máximo existente en la red
order_max <- max(canales$ORDER, na.rm = TRUE)

# Algunos nodos de ORDER máximo pueden caer sobre NoData del DTM.
# Por eso se busca el ORDER más alto que tenga nodos con cota válida.

ordenes_disponibles <- sort(
  unique(canales$ORDER),
  decreasing = TRUE
)

nodos_candidatos <- NULL
order_usado <- NA_real_

for (ord in ordenes_disponibles) {
  
  tramos_ord <- canales %>%
    filter(ORDER == ord)
  
  nodos_ord <- unique(c(
    tramos_ord$NODE_A,
    tramos_ord$NODE_B
  ))
  
  candidatos_ord <- nodos %>%
    filter(
      node %in% nodos_ord,
      is.finite(z)
    )
  
  cat(
    "\nORDER =", ord,
    "| tramos =", nrow(tramos_ord),
    "| nodos =", length(nodos_ord),
    "| nodos con cota válida =", nrow(candidatos_ord)
  )
  
  if (nrow(candidatos_ord) > 0) {
    nodos_candidatos <- candidatos_ord
    order_usado <- ord
    break
  }
}

if (is.null(nodos_candidatos) || nrow(nodos_candidatos) == 0) {
  stop("No hay nodos candidatos válidos en ningún ORDER.")
}

# Nodo más bajo del ORDER usado = exutorio aproximado
exutorio_node <- nodos_candidatos$node[
  which.min(nodos_candidatos$z)
]

exutorio_z <- min(nodos_candidatos$z, na.rm = TRUE)

cat("\n----------------------------------------\n")
cat("ORDER máximo de la red:", order_max, "\n")
cat("ORDER usado para localizar exutorio:", order_usado, "\n")
cat("Nodo exutorio aproximado:", exutorio_node, "\n")
cat("Cota exutorio:", round(exutorio_z, 2), "m\n")
cat("----------------------------------------\n")

# 9. ORIENTAR TRAMOS POR COTA ====

edges <- canales %>%
  st_drop_geometry() %>%
  left_join(
    nodos %>% select(NODE_A = node, z_A = z),
    by = "NODE_A"
  ) %>%
  left_join(
    nodos %>% select(NODE_B = node, z_B = z),
    by = "NODE_B"
  ) %>%
  filter(
    is.finite(z_A),
    is.finite(z_B)
  ) %>%
  mutate(
    # Sentido hidráulico real: alto -> bajo
    from_down = ifelse(z_A <= z_B, NODE_A, NODE_B),
    to_up     = ifelse(z_A <= z_B, NODE_B, NODE_A),
    
    # Para recorrer desde exutorio aguas arriba usamos bajo -> alto
    from = from_down,
    to = to_up,
    
    dz_up = abs(z_A - z_B)
  ) %>%
  transmute(
    from,
    to,
    weight = LENGTH,
    SEGMENT_ID,
    ORDER,
    z_A,
    z_B,
    dz_up
  )

if (nrow(edges) == 0) {
  stop("No quedan aristas válidas tras orientar por cota.")
}

# 10. GRAFO DIRIGIDO BAJO -> ALTO ====

g <- graph_from_data_frame(
  d = edges,
  directed = TRUE,
  vertices = nodos %>% select(name = node, z)
)

E(g)$weight <- edges$weight
E(g)$SEGMENT_ID <- edges$SEGMENT_ID
E(g)$ORDER <- edges$ORDER

cat("\nResumen del grafo dirigido bajo -> alto:\n")
cat("Nodos:", vcount(g), "\n")
cat("Aristas:", ecount(g), "\n")

if (!(exutorio_node %in% V(g)$name)) {
  stop("El nodo exutorio no está en el grafo.")
}

# 11. BUSCAR CAMINO ASCENDENTE MÁS LARGO DESDE EXUTORIO ====

distancias <- distances(
  g,
  v = exutorio_node,
  mode = "out",
  weights = E(g)$weight
)

distancias_vec <- as.numeric(distancias[1, ])
names(distancias_vec) <- colnames(distancias)

distancias_vec[is.infinite(distancias_vec)] <- NA

if (all(is.na(distancias_vec))) {
  stop("No hay nodos alcanzables aguas arriba desde el exutorio.")
}

cabecera_node <- names(distancias_vec)[which.max(distancias_vec)]
longitud_grafo_m <- max(distancias_vec, na.rm = TRUE)

cat("\nCabecera más alejada aguas arriba:", cabecera_node, "\n")
cat("Longitud acumulada según grafo:", round(longitud_grafo_m, 2), "m\n")

camino_vertices <- shortest_paths(
  g,
  from = exutorio_node,
  to = cabecera_node,
  mode = "out",
  weights = E(g)$weight,
  output = "both"
)

camino_edges <- camino_vertices$epath[[1]]
segmentos_principales <- E(g)$SEGMENT_ID[camino_edges]

cat("Número de tramos del cauce principal:", length(segmentos_principales), "\n")

# 12. EXTRAER CAUCE PRINCIPAL ====

cauce_principal <- canales %>%
  filter(SEGMENT_ID %in% segmentos_principales) %>%
  mutate(
    longitud_m_geom = as.numeric(st_length(.)),
    cauce_principal = TRUE
  )

Lc_m <- sum(cauce_principal$longitud_m_geom, na.rm = TRUE)
Lc_km <- Lc_m / 1000

if (!is.finite(Lc_m) || Lc_m <= 0) {
  stop("La longitud calculada del cauce principal no es válida.")
}

# 13. EXTRAER PERFIL DE COTAS A LO LARGO DEL CAUCE ====

cauce_union <- st_union(st_geometry(cauce_principal))
cauce_line <- st_line_merge(cauce_union)

puntos <- st_line_sample(
  cauce_line,
  density = 1 / paso_m
)

puntos_sf <- st_sf(
  geometry = st_cast(puntos, "POINT"),
  crs = st_crs(cauce_principal)
)

if (nrow(puntos_sf) == 0) {
  stop("No se han podido generar puntos sobre el cauce principal.")
}

z_extraida <- terra::extract(dtm, vect(puntos_sf))
z_vals <- z_extraida[[2]]
z_vals <- z_vals[is.finite(z_vals)]

if (length(z_vals) == 0) {
  stop("No se han podido extraer cotas válidas del DTM sobre el cauce.")
}

z_max <- max(z_vals, na.rm = TRUE)
z_min <- min(z_vals, na.rm = TRUE)
desnivel_m <- z_max - z_min
Jc <- desnivel_m / Lc_m

# 14. EXPORTAR CAUCE PRINCIPAL ====

exutorio_aprox_sf <- st_as_sf(
  nodos %>% filter(node == exutorio_node),
  coords = c("x", "y"),
  crs = st_crs(canales)
) %>%
  mutate(
    tipo = "exutorio_aproximado",
    z_m = exutorio_z,
    order_usado = order_usado
  )

cabecera_sf <- st_as_sf(
  nodos %>% filter(node == cabecera_node),
  coords = c("x", "y"),
  crs = st_crs(canales)
) %>%
  mutate(tipo = "cabecera_cauce_principal")

guardar_capa_gpkg(cauce_principal, capa_cauce_principal, overwrite = TRUE)
guardar_capa_gpkg(exutorio_aprox_sf, capa_exutorio_aprox, overwrite = TRUE)
guardar_capa_gpkg(cabecera_sf, capa_cabecera_cauce_principal, overwrite = TRUE)
guardar_capa_gpkg(nodos_sf, capa_nodos_red_drenaje, overwrite = TRUE)

# 15. TABLAS DE SALIDA ====

morfometria_detalle <- tibble::tibble(
  metodo = "camino_ascendente_desde_exutorio_order_max",
  order_max = order_max,
  order_usado_exutorio = order_usado,
  exutorio_node = exutorio_node,
  cabecera_node = cabecera_node,
  n_tramos = nrow(cauce_principal),
  Lc_m = Lc_m,
  Lc_km = Lc_km,
  longitud_grafo_m = longitud_grafo_m,
  z_exutorio_m = exutorio_z,
  z_max_m = z_max,
  z_min_m = z_min,
  desnivel_m = desnivel_m,
  Jc = Jc,
  pendiente_pct = 100 * Jc,
  paso_muestreo_m = paso_m,
  n_puntos_muestreo = length(z_vals)
)

write_csv(
  morfometria_detalle,
  salida_morfometria_detalle
)

morfometria_006 <- tibble::tibble(
  Lc_km = Lc_km,
  Jc = Jc,
  I1_Id = I1_Id_defecto
)

write_csv(
  morfometria_006,
  salida_morfometria_006
)
# 16. RESUMEN ====

cat("\n====================================================\n")
cat("SCRIPT 004 FINALIZADO\n")
cat("====================================================\n")

cat("\nGeoPackage del proyecto:\n")
cat(gpkg_proyecto, "\n")

cat("\nCapas usadas/generadas:\n")
cat("Entrada:", capa_canales, "\n")
cat("Salida :", capa_cauce_principal, "\n")
cat("Salida :", capa_exutorio_aprox, "\n")
cat("Salida :", capa_cabecera_cauce_principal, "\n")
cat("Salida :", capa_nodos_red_drenaje, "\n")

cat("\nMorfometría detallada:\n")
cat(salida_morfometria_detalle, "\n")

cat("\nTabla para script 006:\n")
cat(salida_morfometria_006, "\n")

cat("\nResultados principales:\n")
cat("ORDER máximo =", order_max, "\n")
cat("ORDER usado para exutorio =", order_usado, "\n")
cat("Exutorio =", exutorio_node, "\n")
cat("Cabecera =", cabecera_node, "\n")
cat("Lc =", round(Lc_km, 4), "km\n")
cat("Zmax =", round(z_max, 2), "m\n")
cat("Zmin =", round(z_min, 2), "m\n")
cat("Desnivel =", round(desnivel_m, 2), "m\n")
cat("Jc =", round(Jc, 5), "\n")
cat("Pendiente =", round(100 * Jc, 3), "%\n")

cat("\nCapas actualmente disponibles en el GeoPackage:\n")
print(listar_capas_gpkg())

cat("\nATENCION: revisa visualmente la capa '", capa_cauce_principal, "' en QGIS.\n", sep = "")
cat("El metodo busca el camino ascendente mas largo desde el exutorio aproximado.\n")

cat("====================================================\n")