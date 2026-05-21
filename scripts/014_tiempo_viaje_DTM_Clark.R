############################################################
#
# 014_tiempo_viaje_DTM_Clark.R
#
# Objetivo:
# - Construir una aproximacion geomorfologica de la curva
#   tiempo-area necesaria para el hidrograma unitario de Clark.
# - Usar el DTM hidrologicamente corregido y la cuenca delimitada
#   almacenada en el GeoPackage unico del proyecto.
# - Calcular direcciones D8, tiempos de viaje acumulados hacia el
#   exutorio y una curva tiempo-area A(t).
# - Exportar raster de tiempo de viaje, tabla tiempo-area, resumen
#   y figuras diagnosticas.
#
# Nota conceptual:
# - Este script NO calcula todavia el HU de Clark. Prepara su entrada.
# - El siguiente script, 015_HU_Clark.R, aplicara el almacenamiento
#   lineal de Clark sobre esta curva tiempo-area.
#
############################################################


# 0. PAQUETES ====

paquetes <- c("terra", "sf", "dplyr", "readr", "ggplot2", "tidyr", "tibble")

instalar <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(instalar) > 0) install.packages(instalar)

library(terra)
library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(tibble)


# 1. CONFIGURACION GENERAL ====

if (file.exists("scripts/000_rutas_y_capas.R")) {
  source("scripts/000_rutas_y_capas.R")
} else {
  stop("No se encuentra scripts/000_rutas_y_capas.R")
}

dir.create("datos/procesados", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/figuras", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/mapas", recursive = TRUE, showWarnings = FALSE)


# 2. RUTAS Y CAPAS ====

# DTM hidrologicamente corregido.
archivo_dtm <- "salidas/mapas/DTM_Burnt_Filled_Clipped.tif"

# Capas vectoriales en el GeoPackage unico.
capa_cuenca <- capas_gpkg$cuenca
capa_exutorio <- capas_gpkg$exutorio

# Tabla del script 006 para leer Tc.
# Se usa la version normativa 5.2-IC, coherente con el metodo racional.
archivo_006 <- "salidas/tablas/006_lluvia_IDF_tiempo_concentracion.csv"

salida_tiempo_raster <- "salidas/mapas/014_tiempo_viaje_Clark_h.tif"
salida_curva_tiempo_area <- "salidas/tablas/014_curva_tiempo_area_Clark.csv"
salida_resumen <- "salidas/tablas/014_resumen_tiempo_area_Clark.csv"
salida_fig_curva <- "salidas/figuras/014_curva_tiempo_area_Clark.png"
salida_fig_hist <- "salidas/figuras/014_histograma_tiempos_Clark.png"


# 3. PARAMETROS EDITABLES ====

# Si TRUE, los tiempos calculados desde el DTM se reescalan para que
# el tiempo maximo coincida con el Tc calculado en el script 006.
# Esto hace que la geometria del DTM controle la forma de la curva
# tiempo-area, mientras Tc fija la escala temporal global.
ajustar_tiempo_maximo_a_Tc <- TRUE

# Si el script 006 tiene varios periodos de retorno, Tc deberia ser unico.
# En caso contrario se usa el primer valor no nulo.
columna_Tc_006 <- "tc_h"

# Velocidad de flujo usada antes del posible reescalado a Tc.
# Opciones:
# - "constante"       : velocidad constante en todas las celdas.
# - "pendiente_sqrt"  : v = coef_velocidad * sqrt(S), limitada por v_min/v_max.
metodo_velocidad <- "pendiente_sqrt"
velocidad_constante_m_s <- 0.40
coef_velocidad <- 1.0
v_min_m_s <- 0.05
v_max_m_s <- 2.00
pendiente_minima <- 0.001

# Paso temporal para la curva tiempo-area.
# Por coherencia con los hietogramas/HU previos, se deja inicialmente en 30 min.
dt_min <- 30

# Tratamiento de areas planas o celdas sin descenso D8.
# Si TRUE, permite flujo en pendiente cero, lo que es util con DTM rellenados.
permitir_flujo_en_planos <- TRUE

# Si TRUE, los pixeles no conectados al exutorio se excluyen de la curva tiempo-area.
# Si FALSE, el script se detiene cuando hay un porcentaje bajo de celdas conectadas.
permitir_celdas_no_conectadas <- TRUE

# Umbral minimo de celdas conectadas. Solo detiene el script si permitir_celdas_no_conectadas = FALSE.
porcentaje_minimo_conectado <- 50

# Advertencia de rendimiento. El algoritmo D8 en R puede ser lento en rasters grandes.
max_celdas_advertencia <- 1e6

# Si TRUE, guarda el raster de tiempo de viaje. Puede ser pesado.
guardar_raster_tiempo <- TRUE


# 4. FUNCIONES AUXILIARES ====

leer_Tc_desde_006 <- function(archivo_006, columna_Tc_006 = "tc_h") {
  if (!file.exists(archivo_006)) {
    stop("No se encuentra el archivo del script 006: ", archivo_006)
  }

  x <- read_csv(archivo_006, show_col_types = FALSE)

  if (!columna_Tc_006 %in% names(x)) {
    stop(
      "No se encuentra la columna ", columna_Tc_006,
      " en ", archivo_006, ". Columnas disponibles: ",
      paste(names(x), collapse = ", ")
    )
  }

  tc <- unique(as.numeric(x[[columna_Tc_006]]))
  tc <- tc[is.finite(tc) & tc > 0]

  if (length(tc) == 0) stop("No hay valores validos de Tc en el script 006.")

  if (length(tc) > 1) {
    warning(
      "Hay varios valores de Tc en el archivo 006. Se usara el primero: ",
      round(tc[1], 4), " h"
    )
  }

  tc[1]
}


matriz_vecino <- function(z, dr, dc) {
  nr <- nrow(z)
  nc <- ncol(z)
  out <- matrix(NA_real_, nr, nc)

  r_origen <- max(1, 1 - dr):min(nr, nr - dr)
  c_origen <- max(1, 1 - dc):min(nc, nc - dc)
  r_dest <- r_origen + dr
  c_dest <- c_origen + dc

  out[r_origen, c_origen] <- z[r_dest, c_dest]
  out
}


indice_vecino <- function(nr, nc, dr, dc) {
  idx <- matrix(seq_len(nr * nc), nr, nc, byrow = TRUE)
  out <- matrix(NA_integer_, nr, nc)

  r_origen <- max(1, 1 - dr):min(nr, nr - dr)
  c_origen <- max(1, 1 - dc):min(nc, nc - dc)
  r_dest <- r_origen + dr
  c_dest <- c_origen + dc

  out[r_origen, c_origen] <- idx[r_dest, c_dest]
  out
}


calcular_D8_y_tiempo_segmento <- function(z, res_x, res_y) {
  nr <- nrow(z)
  nc <- ncol(z)

  dirs <- tibble(
    dr = c(-1, -1,  0, 1, 1, 1,  0, -1),
    dc = c( 0,  1,  1, 1, 0,-1, -1, -1)
  ) %>%
    mutate(
      distancia_m = sqrt((dc * res_x)^2 + (dr * res_y)^2)
    )

  n <- nr * nc
  mejor_pendiente <- rep(-Inf, n)
  mejor_indice <- rep(NA_integer_, n)
  mejor_distancia <- rep(NA_real_, n)

  z_vec <- as.vector(t(z))
  validas <- is.finite(z_vec)

  for (k in seq_len(nrow(dirs))) {
    zn <- matriz_vecino(z, dirs$dr[k], dirs$dc[k])
    idxn <- indice_vecino(nr, nc, dirs$dr[k], dirs$dc[k])

    zn_vec <- as.vector(t(zn))
    idxn_vec <- as.vector(t(idxn))

    dz <- z_vec - zn_vec
    pendiente <- dz / dirs$distancia_m[k]

    if (permitir_flujo_en_planos) {
      candidato <- validas & is.finite(zn_vec) & pendiente >= 0
    } else {
      candidato <- validas & is.finite(zn_vec) & pendiente > 0
    }

    mejora <- candidato & pendiente > mejor_pendiente
    mejor_pendiente[mejora] <- pendiente[mejora]
    mejor_indice[mejora] <- idxn_vec[mejora]
    mejor_distancia[mejora] <- dirs$distancia_m[k]
  }

  mejor_pendiente[!is.finite(mejor_pendiente)] <- NA_real_
  mejor_pendiente[is.finite(mejor_pendiente)] <- pmax(
    mejor_pendiente[is.finite(mejor_pendiente)],
    pendiente_minima
  )

  if (metodo_velocidad == "constante") {
    velocidad <- rep(velocidad_constante_m_s, n)
  } else if (metodo_velocidad == "pendiente_sqrt") {
    velocidad <- coef_velocidad * sqrt(mejor_pendiente)
    velocidad <- pmin(pmax(velocidad, v_min_m_s), v_max_m_s)
  } else {
    stop("metodo_velocidad no reconocido: ", metodo_velocidad)
  }

  tiempo_segmento_h <- mejor_distancia / velocidad / 3600

  tiempo_segmento_h[!validas | !is.finite(tiempo_segmento_h)] <- NA_real_
  mejor_indice[!validas] <- NA_integer_

  list(
    downstream = mejor_indice,
    tiempo_segmento_h = tiempo_segmento_h,
    pendiente = mejor_pendiente,
    velocidad_m_s = velocidad
  )
}


calcular_tiempos_hacia_salida <- function(downstream, tiempo_segmento_h, outlet_idx, validas) {
  n <- length(downstream)
  tiempo <- rep(NA_real_, n)
  estado <- rep(0L, n)

  if (!is.finite(outlet_idx) || outlet_idx < 1 || outlet_idx > n || !validas[outlet_idx]) {
    stop("El indice de exutorio no es valido o no cae sobre una celda valida.")
  }

  tiempo[outlet_idx] <- 0
  estado[outlet_idx] <- 2L

  resolver <- function(i) {
    if (!validas[i]) return(NA_real_)
    if (estado[i] == 2L) return(tiempo[i])

    if (estado[i] == 1L) {
      # Ciclo en zona plana. Se corta asignando NA.
      tiempo[i] <<- NA_real_
      estado[i] <<- 2L
      return(NA_real_)
    }

    estado[i] <<- 1L
    j <- downstream[i]

    if (is.na(j) || !validas[j]) {
      tiempo[i] <<- NA_real_
    } else {
      tj <- resolver(j)

      if (is.finite(tj) && is.finite(tiempo_segmento_h[i])) {
        tiempo[i] <<- tiempo_segmento_h[i] + tj
      } else {
        tiempo[i] <<- NA_real_
      }
    }

    estado[i] <<- 2L
    tiempo[i]
  }

  ids <- which(validas)

  for (i in ids) {
    if (estado[i] == 0L) resolver(i)
  }

  tiempo
}


buscar_outlet_idx <- function(dtm_mask, exutorio_sf = NULL) {
  z <- as.matrix(dtm_mask, wide = TRUE)
  nr <- nrow(z)
  nc <- ncol(z)
  validas_mat <- is.finite(z)

  if (!is.null(exutorio_sf) && nrow(exutorio_sf) > 0) {
    ex <- st_transform(exutorio_sf, crs(dtm_mask))
    ex_vect <- vect(ex)
    xy <- crds(ex_vect)

    celda <- cellFromXY(dtm_mask, xy[1, , drop = FALSE])

    if (length(celda) > 0 && is.finite(celda[1])) {
      z_vec <- values(dtm_mask, mat = FALSE)

      if (is.finite(z_vec[celda[1]])) {
        return(as.integer(celda[1]))
      }

      # Si el punto cae en NoData por borde/mascara, se busca la celda valida mas cercana.
      valid_cells <- which(is.finite(z_vec))
      xy_valid <- xyFromCell(dtm_mask, valid_cells)
      d2 <- (xy_valid[, 1] - xy[1, 1])^2 + (xy_valid[, 2] - xy[1, 2])^2
      return(as.integer(valid_cells[which.min(d2)]))
    }
  }

  borde <- matrix(FALSE, nr, nc)
  borde[1, ] <- TRUE
  borde[nr, ] <- TRUE
  borde[, 1] <- TRUE
  borde[, nc] <- TRUE

  candidatos <- which(as.vector(t(validas_mat & borde)))

  if (length(candidatos) == 0) {
    stop("No se pudo estimar el exutorio: no hay celdas validas en el borde.")
  }

  z_vec <- as.vector(t(z))
  candidatos[which.min(z_vec[candidatos])]
}


construir_curva_tiempo_area <- function(tiempo_h, area_celda_m2, dt_min) {
  dt_h <- dt_min / 60
  t_valid <- tiempo_h[is.finite(tiempo_h) & tiempo_h >= 0]

  if (length(t_valid) == 0) {
    stop("No hay tiempos de viaje validos para construir A(t).")
  }

  tmax <- ceiling(max(t_valid) / dt_h) * dt_h
  cortes <- seq(0, tmax + dt_h, by = dt_h)

  histo <- hist(
    t_valid,
    breaks = cortes,
    plot = FALSE,
    right = TRUE,
    include.lowest = TRUE
  )

  area_intervalo_km2 <- histo$counts * area_celda_m2 / 1e6
  area_acumulada_km2 <- cumsum(area_intervalo_km2)
  area_total_km2 <- sum(area_intervalo_km2)

  if (!is.finite(area_total_km2) || area_total_km2 <= 0) {
    stop("La curva tiempo-area tiene area total nula o no valida.")
  }

  tibble(
    intervalo = seq_along(area_intervalo_km2),
    tiempo_inicio_h = cortes[-length(cortes)],
    tiempo_fin_h = cortes[-1],
    tiempo_centro_h = (tiempo_inicio_h + tiempo_fin_h) / 2,
    area_intervalo_km2 = area_intervalo_km2,
    area_acumulada_km2 = area_acumulada_km2,
    fraccion_area_intervalo = area_intervalo_km2 / area_total_km2,
    fraccion_area_acumulada = area_acumulada_km2 / area_total_km2
  )
}


# 5. LEER ENTRADAS ====

if (!file.exists(archivo_dtm)) {
  stop("No se encuentra el DTM hidrologico: ", archivo_dtm)
}

if (!existe_capa_gpkg(capa_cuenca)) {
  stop(
    "No existe la capa de cuenca '", capa_cuenca, "' en el GeoPackage.\n",
    "Ejecuta antes el script 002_importar_resultados_QGIS_a_gpkg.R.\n",
    "Capas disponibles:\n",
    paste(listar_capas_gpkg()$name, collapse = ", ")
  )
}

tc_h <- leer_Tc_desde_006(archivo_006, columna_Tc_006)

dtm <- rast(archivo_dtm)

if (is.na(crs(dtm))) {
  stop("El DTM no tiene CRS definido.")
}

cuenca <- leer_capa_gpkg(capa_cuenca)
cuenca <- st_zm(cuenca, drop = TRUE, what = "ZM")
cuenca <- st_make_valid(cuenca)
cuenca <- st_transform(cuenca, crs(dtm))

exutorio <- NULL

if (existe_capa_gpkg(capa_exutorio)) {
  exutorio <- leer_capa_gpkg(capa_exutorio)
  exutorio <- st_zm(exutorio, drop = TRUE, what = "ZM")
  exutorio <- st_make_valid(exutorio)
  exutorio <- st_transform(exutorio, crs(dtm))
} else {
  warning(
    "No existe la capa de exutorio '", capa_exutorio,
    "' en el GeoPackage. Se estimara como la celda valida de menor cota en el borde."
  )
}

cuenca_vect <- vect(cuenca)
dtm_mask <- crop(dtm, cuenca_vect)
dtm_mask <- mask(dtm_mask, cuenca_vect)

n_validas <- global(!is.na(dtm_mask), "sum", na.rm = TRUE)[1, 1]

if (!is.finite(n_validas) || n_validas <= 0) {
  stop("La mascara de cuenca no contiene celdas validas del DTM.")
}

if (n_validas > max_celdas_advertencia) {
  warning(
    "El raster contiene ", n_validas, " celdas validas. ",
    "El calculo D8 puede tardar. Si es necesario, recorta la cuenca o ",
    "usa una resolucion mas gruesa para una primera prueba."
  )
}


# 6. CALCULAR D8 Y TIEMPOS DE VIAJE ====

z <- as.matrix(dtm_mask, wide = TRUE)
validas <- is.finite(as.vector(t(z)))
res_xy <- res(dtm_mask)
area_celda_m2 <- res_xy[1] * res_xy[2]

outlet_idx <- buscar_outlet_idx(dtm_mask, exutorio)

cat("\nCalculando direcciones D8 y tiempos de segmento...\n")
d8 <- calcular_D8_y_tiempo_segmento(z, res_xy[1], res_xy[2])

cat("Calculando tiempos acumulados hacia el exutorio...\n")
tiempo_h <- calcular_tiempos_hacia_salida(
  downstream = d8$downstream,
  tiempo_segmento_h = d8$tiempo_segmento_h,
  outlet_idx = outlet_idx,
  validas = validas
)

n_tiempos_validos <- sum(is.finite(tiempo_h))
porcentaje_conectado <- 100 * n_tiempos_validos / n_validas

if (n_tiempos_validos == 0) {
  stop(
    "No se obtuvieron tiempos validos hacia el exutorio. ",
    "Revise el DTM, la capa de exutorio, la conectividad D8 o el tratamiento de planos."
  )
}

if (!permitir_celdas_no_conectadas && porcentaje_conectado < porcentaje_minimo_conectado) {
  stop(
    "Solo el ", round(porcentaje_conectado, 2),
    "% de las celdas validas conecta con el exutorio. ",
    "Revise el exutorio o el DTM."
  )
}

if (porcentaje_conectado < 100) {
  warning(
    "Solo el ", round(porcentaje_conectado, 2),
    "% de las celdas validas conecta con el exutorio. ",
    "La curva tiempo-area se construira con las celdas conectadas."
  )
}

# Reescalado a Tc para que max(t) = Tc.
tiempo_h_bruto <- tiempo_h
factor_ajuste_Tc <- NA_real_

if (ajustar_tiempo_maximo_a_Tc) {
  tmax_bruto <- max(tiempo_h_bruto, na.rm = TRUE)

  if (!is.finite(tmax_bruto) || tmax_bruto <= 0) {
    stop("No se puede ajustar a Tc: el tiempo maximo bruto no es valido.")
  }

  factor_ajuste_Tc <- tc_h / tmax_bruto
  tiempo_h <- tiempo_h_bruto * factor_ajuste_Tc
}


# 7. RASTER DE TIEMPO DE VIAJE ====

r_tiempo <- dtm_mask
values(r_tiempo) <- tiempo_h
names(r_tiempo) <- "tiempo_viaje_h"

if (guardar_raster_tiempo) {
  writeRaster(
    r_tiempo,
    salida_tiempo_raster,
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW")
  )
}


# 8. CURVA TIEMPO-AREA ====

curva_ta <- construir_curva_tiempo_area(
  tiempo_h = tiempo_h,
  area_celda_m2 = area_celda_m2,
  dt_min = dt_min
)

curva_ta <- curva_ta %>%
  mutate(
    fraccion_area_acumulada = pmin(fraccion_area_acumulada, 1)
  )

area_total_km2 <- sum(curva_ta$area_intervalo_km2, na.rm = TRUE)

resumen <- tibble(
  archivo_dtm = archivo_dtm,
  gpkg_proyecto = gpkg_proyecto,
  capa_cuenca = capa_cuenca,
  capa_exutorio = ifelse(existe_capa_gpkg(capa_exutorio), capa_exutorio, NA_character_),
  n_celdas_validas_DTM = n_validas,
  n_celdas_con_tiempo_valido = n_tiempos_validos,
  porcentaje_celdas_con_tiempo_valido = porcentaje_conectado,
  resolucion_x_m = res_xy[1],
  resolucion_y_m = res_xy[2],
  area_celda_m2 = area_celda_m2,
  area_total_Clark_km2 = area_total_km2,
  tc_h_006 = tc_h,
  metodo_velocidad = metodo_velocidad,
  velocidad_constante_m_s = velocidad_constante_m_s,
  coef_velocidad = coef_velocidad,
  v_min_m_s = v_min_m_s,
  v_max_m_s = v_max_m_s,
  pendiente_minima = pendiente_minima,
  ajustar_tiempo_maximo_a_Tc = ajustar_tiempo_maximo_a_Tc,
  factor_ajuste_Tc = factor_ajuste_Tc,
  tiempo_min_h = min(tiempo_h, na.rm = TRUE),
  tiempo_medio_h = mean(tiempo_h, na.rm = TRUE),
  tiempo_mediano_h = median(tiempo_h, na.rm = TRUE),
  tiempo_max_h = max(tiempo_h, na.rm = TRUE),
  dt_min = dt_min,
  n_intervalos_tiempo_area = nrow(curva_ta)
)


# 9. EXPORTAR TABLAS ====

write_csv(curva_ta, salida_curva_tiempo_area)
write_csv(resumen, salida_resumen)


# 10. FIGURAS ====

g_curva <- ggplot(curva_ta, aes(x = tiempo_fin_h, y = fraccion_area_acumulada)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Curva tiempo-area para el metodo de Clark",
    subtitle = paste0(
      "Area conectada = ", round(area_total_km2, 2), " km² | Tc = ",
      round(tc_h, 2), " h | dt = ", dt_min, " min"
    ),
    x = "Tiempo de viaje acumulado hasta el exutorio (h)",
    y = "Fraccion acumulada de area"
  ) +
  theme_minimal()

g_hist <- ggplot(curva_ta, aes(x = tiempo_inicio_h, y = area_intervalo_km2)) +
  geom_col(width = dt_min / 60, align = "edge", color = "grey30") +
  labs(
    title = "Histograma tiempo-area para el metodo de Clark",
    subtitle = paste0(
      "Area por intervalo de tiempo | dt = ", dt_min, " min"
    ),
    x = "Tiempo de viaje acumulado hasta el exutorio (h)",
    y = "Area del intervalo (km²)"
  ) +
  theme_minimal()

ggsave(salida_fig_curva, g_curva, width = 8.5, height = 5.5, dpi = 300)
ggsave(salida_fig_hist, g_hist, width = 8.5, height = 5.5, dpi = 300)

print(g_curva)
print(g_hist)


# 11. RESUMEN EN CONSOLA ====

cat("\n====================================================\n")
cat("SCRIPT 014 FINALIZADO\n")
cat("====================================================\n")

cat("\nEntradas:\n")
cat("DTM:", archivo_dtm, "\n")
cat("GeoPackage:", gpkg_proyecto, "\n")
cat("Cuenca:", capa_cuenca, "\n")
cat("Exutorio:", ifelse(existe_capa_gpkg(capa_exutorio), capa_exutorio, "estimado como menor cota en borde"), "\n")
cat("Tc usado desde 006:", round(tc_h, 3), "h\n")

cat("\nConfiguracion:\n")
cat("metodo_velocidad =", metodo_velocidad, "\n")
cat("ajustar_tiempo_maximo_a_Tc =", ajustar_tiempo_maximo_a_Tc, "\n")
cat("dt_min =", dt_min, "min\n")

cat("\nResumen principal:\n")
print(
  resumen %>%
    select(
      area_total_Clark_km2,
      tc_h_006,
      tiempo_medio_h,
      tiempo_mediano_h,
      tiempo_max_h,
      porcentaje_celdas_con_tiempo_valido,
      factor_ajuste_Tc
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4)))
)

cat("\nTablas generadas:\n")
cat(salida_curva_tiempo_area, "\n")
cat(salida_resumen, "\n")

cat("\nRaster generado:\n")
cat(ifelse(guardar_raster_tiempo, salida_tiempo_raster, "no guardado"), "\n")

cat("\nFiguras generadas:\n")
cat(salida_fig_curva, "\n")
cat(salida_fig_hist, "\n")
cat("====================================================\n")
