############################################################
#
# 015_hidrograma_unitario_Clark.R
#
# Objetivo:
# - Leer la curva tiempo-area generada por el script 014
# - Construir el hidrograma unitario de Clark
# - Aplicar traslacion mediante histograma tiempo-area
# - Aplicar almacenamiento lineal S = K Q
# - Exportar el HU de Clark como q_unitario_m3_s_por_mm
# - Preparar la entrada para la convolucion del script 016
#
############################################################


# 0. PAQUETES ====

paquetes <- c("dplyr", "readr", "ggplot2", "tidyr", "stringr", "tibble")

instalar <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(instalar) > 0) install.packages(instalar)

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(stringr)
library(tibble)


# 1. CONFIGURACION GENERAL ====

if (file.exists("scripts/00_configuracion.R")) {
  source("scripts/00_configuracion.R")
}

if (file.exists("scripts/000_configuracion.R")) {
  source("scripts/000_configuracion.R")
}

dir.create("salidas/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/figuras", recursive = TRUE, showWarnings = FALSE)


# 2. RUTAS ====

archivo_curva_014 <- "salidas/tablas/014_curva_tiempo_area_Clark.csv"
archivo_resumen_014 <- "salidas/tablas/014_resumen_tiempo_area_Clark.csv"

archivos_010_candidatos <- c(
  "salidas/tablas/010_lluvia_efectiva_SCS_CN_largo.csv",
  "salidas/tablas/010_lluvia_efectiva_largo.csv",
  "salidas/tablas/010_hietograma_efectivo_largo.csv"
)

salida_HU_largo <- "salidas/tablas/015_HU_Clark_largo.csv"
salida_HU_base <- "salidas/tablas/015_HU_Clark_base.csv"
salida_resumen <- "salidas/tablas/015_HU_Clark_resumen.csv"
salida_componentes <- "salidas/tablas/015_HU_Clark_componentes.csv"

salida_figura_HU <- "salidas/figuras/015_HU_Clark.png"
salida_figura_componentes <- "salidas/figuras/015_HU_Clark_componentes.png"


# 3. PARAMETROS EDITABLES ====

# Paso temporal del HU. Si se deja NULL, se intenta leer del 010 y despues del 014.
dt_min_manual <- NULL

# Parametro de almacenamiento lineal de Clark, K o R.
# Opciones:
# - "fraccion_tc": K_h = K_fraccion_tc * Tc_h
# - "valor_manual": K_h = K_h_manual
modo_K <- "fraccion_tc"
K_fraccion_tc <- 0.60
K_h_manual <- 1.0

# Duracion adicional de la cola de recesion.
factor_cola_K <- 6
factor_cola_Tc <- 3
num_pasos_minimos_cola <- 6
num_pasos_maximos_extra <- 200
umbral_cola_fraccion_Qp <- 0.001

# Correccion final de volumen.
corregir_volumen <- TRUE

# Si TRUE, duplica el HU base para todos los T_anios encontrados en el script 010.
duplicar_por_periodo_retorno <- TRUE


# 4. FUNCIONES AUXILIARES ====

primer_archivo_existente <- function(rutas, obligatorio = FALSE, etiqueta = "archivo") {
  existe <- rutas[file.exists(rutas)]
  if (length(existe) > 0) return(existe[1])
  if (obligatorio) {
    stop(
      "No se encontro ", etiqueta, ". Candidatos:\n",
      paste(rutas, collapse = "\n")
    )
  }
  return(NA_character_)
}

obtener_valor_resumen <- function(resumen, candidatas, defecto = NA_real_) {
  col <- candidatas[candidatas %in% names(resumen)]
  if (length(col) == 0) return(defecto)
  val <- suppressWarnings(as.numeric(resumen[[col[1]]][1]))
  if (!is.finite(val)) return(defecto)
  val
}

extraer_dt_min_010 <- function(lluvia_010) {
  if (is.null(lluvia_010)) return(NA_real_)
  if (!"dt_min" %in% names(lluvia_010)) return(NA_real_)
  val <- unique(suppressWarnings(as.numeric(lluvia_010$dt_min)))
  val <- val[is.finite(val) & val > 0]
  if (length(val) == 0) return(NA_real_)
  val[1]
}

valor_valido <- function(x) {
  length(x) == 1 && is.finite(x) && x > 0
}

calcular_K_h <- function(tc_h) {
  if (modo_K == "fraccion_tc") {
    K_h <- K_fraccion_tc * tc_h
  } else if (modo_K == "valor_manual") {
    K_h <- K_h_manual
  } else {
    stop("modo_K no reconocido: ", modo_K)
  }
  if (!is.finite(K_h) || K_h <= 0) stop("K_h no es valido.")
  K_h
}

normalizar_histograma_tiempo_area <- function(curva, A_km2, dt_h) {
  nms <- names(curva)
  
  col_t <- c(
    "tiempo_h", "tiempo_fin_h", "tiempo_viaje_h", "tiempo_acumulado_h",
    "tiempo_viaje_acumulado_h", "t_h"
  )
  col_t <- col_t[col_t %in% nms]
  if (length(col_t) == 0) {
    curva$tiempo_h <- seq(dt_h, by = dt_h, length.out = nrow(curva))
    col_t <- "tiempo_h"
  } else {
    col_t <- col_t[1]
  }
  
  curva <- curva %>%
    mutate(tiempo_h = as.numeric(.data[[col_t]])) %>%
    arrange(tiempo_h)
  
  if ("area_intervalo_km2" %in% nms) {
    curva$area_intervalo_km2_calc <- as.numeric(curva$area_intervalo_km2)
  } else if ("area_km2_intervalo" %in% nms) {
    curva$area_intervalo_km2_calc <- as.numeric(curva$area_km2_intervalo)
  } else if ("fraccion_intervalo" %in% nms) {
    curva$area_intervalo_km2_calc <- as.numeric(curva$fraccion_intervalo) * A_km2
  } else if ("fraccion_area_intervalo" %in% nms) {
    curva$area_intervalo_km2_calc <- as.numeric(curva$fraccion_area_intervalo) * A_km2
  } else {
    col_acum <- c(
      "fraccion_acumulada_area", "fraccion_area_acumulada", "fraccion_acumulada",
      "area_acumulada_fraccion", "area_acumulada_km2", "A_acumulada_km2"
    )
    col_acum <- col_acum[col_acum %in% nms]
    if (length(col_acum) == 0) {
      stop(
        "No se reconoce la estructura de ", archivo_curva_014, ".\n",
        "Debe contener area_intervalo_km2, fraccion_intervalo o una columna acumulada.\n",
        "Columnas disponibles: ", paste(nms, collapse = ", ")
      )
    }
    col_acum <- col_acum[1]
    acum <- as.numeric(curva[[col_acum]])
    if (max(acum, na.rm = TRUE) <= 1.5) {
      fraccion_acum <- acum
    } else {
      fraccion_acum <- acum / A_km2
    }
    fraccion_acum <- pmin(pmax(fraccion_acum, 0), 1)
    curva$area_intervalo_km2_calc <- c(fraccion_acum[1], diff(fraccion_acum)) * A_km2
  }
  
  curva <- curva %>%
    mutate(
      area_intervalo_km2_calc = if_else(
        is.finite(area_intervalo_km2_calc) & area_intervalo_km2_calc > 0,
        area_intervalo_km2_calc,
        0
      )
    )
  
  suma_area <- sum(curva$area_intervalo_km2_calc, na.rm = TRUE)
  if (!is.finite(suma_area) || suma_area <= 0) {
    stop("El histograma tiempo-area tiene area nula o no valida.")
  }
  
  curva <- curva %>%
    mutate(
      area_intervalo_km2 = area_intervalo_km2_calc * A_km2 / suma_area,
      fraccion_intervalo = area_intervalo_km2 / A_km2,
      fraccion_acumulada = cumsum(fraccion_intervalo),
      tiempo_inicio_h = pmax(tiempo_h - dt_h, 0),
      tiempo_centro_h = tiempo_h - dt_h / 2
    )
  
  curva
}

construir_HU_Clark_base <- function(hist_ta, A_km2, tc_h, dt_h, K_h) {
  dt_s <- dt_h * 3600
  volumen_unitario_m3 <- A_km2 * 1000 # 1 mm sobre 1 km2 = 1000 m3
  
  I_m3_s <- hist_ta$area_intervalo_km2 * 1000 / dt_s
  
  n_extra <- ceiling(max(
    factor_cola_K * K_h / dt_h,
    factor_cola_Tc * tc_h / dt_h,
    num_pasos_minimos_cola
  ))
  n_extra <- min(n_extra, num_pasos_maximos_extra)
  
  I_ext <- c(I_m3_s, rep(0, n_extra))
  tiempo_h <- seq(0, by = dt_h, length.out = length(I_ext))
  
  alpha <- exp(-dt_h / K_h)
  O <- numeric(length(I_ext))
  O[1] <- I_ext[1] * (1 - alpha)
  if (length(I_ext) > 1) {
    for (i in 2:length(I_ext)) {
      O[i] <- O[i - 1] * alpha + I_ext[i] * (1 - alpha)
    }
  }
  
  Qp_tmp <- max(O, na.rm = TRUE)
  if (is.finite(Qp_tmp) && Qp_tmp > 0) {
    idx_pico <- which.max(O)
    idx_cola <- which(O < umbral_cola_fraccion_Qp * Qp_tmp & seq_along(O) > idx_pico)
    if (length(idx_cola) > 0) {
      idx_fin <- max(idx_cola[1], idx_pico + num_pasos_minimos_cola)
      idx_fin <- min(idx_fin, length(O))
      O <- O[seq_len(idx_fin)]
      I_ext <- I_ext[seq_len(idx_fin)]
      tiempo_h <- tiempo_h[seq_len(idx_fin)]
    }
  }
  
  volumen_HU_m3 <- sum(O, na.rm = TRUE) * dt_s
  factor_correccion <- 1
  if (corregir_volumen && is.finite(volumen_HU_m3) && volumen_HU_m3 > 0) {
    factor_correccion <- volumen_unitario_m3 / volumen_HU_m3
    O <- O * factor_correccion
    volumen_HU_m3 <- sum(O, na.rm = TRUE) * dt_s
  }
  
  tibble(
    paso = seq_along(tiempo_h),
    tiempo_h = tiempo_h,
    dt_h = dt_h,
    dt_min = dt_h * 60,
    A_km2 = A_km2,
    tc_h = tc_h,
    K_h = K_h,
    I_traslacion_m3_s_por_mm = I_ext,
    q_unitario_m3_s_por_mm = O,
    volumen_unitario_objetivo_m3 = volumen_unitario_m3,
    volumen_HU_m3 = volumen_HU_m3,
    factor_correccion_volumen = factor_correccion
  )
}


# 5. COMPROBAR ENTRADAS ====

if (!file.exists(archivo_curva_014)) {
  stop("No se encuentra la curva tiempo-area del script 014: ", archivo_curva_014)
}

if (!file.exists(archivo_resumen_014)) {
  warning("No se encuentra el resumen del script 014: ", archivo_resumen_014,
          ". Se intentaran inferir A, Tc y dt desde la curva.")
}


# 6. LEER DATOS ====

curva_014 <- read_csv(archivo_curva_014, show_col_types = FALSE)

resumen_014 <- if (file.exists(archivo_resumen_014)) {
  read_csv(archivo_resumen_014, show_col_types = FALSE)
} else {
  tibble()
}

archivo_010 <- primer_archivo_existente(
  archivos_010_candidatos,
  obligatorio = FALSE,
  etiqueta = "lluvia efectiva del script 010"
)

lluvia_010 <- NULL
if (!is.na(archivo_010)) {
  lluvia_010 <- read_csv(archivo_010, show_col_types = FALSE)
}

#-----------------------------------------------------------
# Lectura opcional de dt desde 010 y 014
#-----------------------------------------------------------
# Se define aqui para evitar errores si alguno de los archivos no existe
# o si no contiene la columna dt_min.

dt_min_010 <- extraer_dt_min_010(lluvia_010)


# 7. PARAMETROS HIDROLOGICOS DESDE 014 ====

A_km2 <- obtener_valor_resumen(
  resumen_014,
  c("area_total_Clark_km2", "A_km2", "area_km2", "area_total_km2", "area_cuenca_km2"),
  defecto = NA_real_
)

tc_h <- obtener_valor_resumen(
  resumen_014,
  c("tc_h_006", "tc_h", "Tc_h", "tiempo_concentracion_h"),
  defecto = NA_real_
)

dt_min_014 <- obtener_valor_resumen(
  resumen_014,
  c("dt_min", "intervalo_min", "paso_min"),
  defecto = NA_real_
)

# Inferencias si el resumen no contiene los datos.
if (!is.finite(A_km2)) {
  posibles_A <- c("area_total_Clark_km2", "A_km2", "area_total_km2", "area_cuenca_km2")
  col_A <- posibles_A[posibles_A %in% names(curva_014)]
  if (length(col_A) > 0) A_km2 <- suppressWarnings(as.numeric(curva_014[[col_A[1]]][1]))
}

if (!is.finite(tc_h)) {
  posibles_tc <- c("tc_h_006", "tc_h", "Tc_h", "tiempo_concentracion_h")
  col_tc <- posibles_tc[posibles_tc %in% names(curva_014)]
  if (length(col_tc) > 0) tc_h <- suppressWarnings(as.numeric(curva_014[[col_tc[1]]][1]))
}

if (!is.finite(dt_min_014)) {
  posibles_dt <- c("dt_min", "intervalo_min", "paso_min")
  col_dt <- posibles_dt[posibles_dt %in% names(curva_014)]
  if (length(col_dt) > 0) dt_min_014 <- suppressWarnings(as.numeric(curva_014[[col_dt[1]]][1]))
}

# Inferir dt desde los tiempos de la curva si es necesario.
if (!is.finite(dt_min_014)) {
  col_t <- c("tiempo_h", "tiempo_fin_h", "tiempo_viaje_h", "tiempo_acumulado_h", "tiempo_viaje_acumulado_h")
  col_t <- col_t[col_t %in% names(curva_014)]
  if (length(col_t) > 0) {
    tt <- sort(unique(suppressWarnings(as.numeric(curva_014[[col_t[1]]]))))
    dif <- diff(tt)
    dif <- dif[is.finite(dif) & dif > 0]
    if (length(dif) > 0) dt_min_014 <- median(dif) * 60
  }
}

#-----------------------------------------------------------
# Resolucion robusta de dt
#-----------------------------------------------------------
# Prioridad:
# 1) dt_min_manual si el usuario lo fija
# 2) dt_min del script 010, para mantener coherencia con la convolucion
# 3) dt_min del script 014
# 4) 30 min como respaldo

dt_min <- if (valor_valido(dt_min_manual)) {
  dt_min_manual
} else if (valor_valido(dt_min_010)) {
  dt_min_010
} else if (valor_valido(dt_min_014)) {
  dt_min_014
} else {
  30
}

dt_h <- dt_min / 60

if (!is.finite(A_km2) || A_km2 <= 0) {
  stop(
    "No se pudo determinar A_km2 de forma valida.\n",
    "Columnas disponibles en 014_resumen: ", paste(names(resumen_014), collapse = ", "), "\n",
    "Columnas disponibles en 014_curva: ", paste(names(curva_014), collapse = ", ")
  )
}
if (!is.finite(tc_h) || tc_h <= 0) {
  stop(
    "No se pudo determinar tc_h de forma valida.\n",
    "Columnas disponibles en 014_resumen: ", paste(names(resumen_014), collapse = ", "), "\n",
    "Columnas disponibles en 014_curva: ", paste(names(curva_014), collapse = ", ")
  )
}
if (!is.finite(dt_h) || dt_h <= 0) stop("No se pudo determinar dt_h de forma valida.")

K_h <- calcular_K_h(tc_h)


# 8. HISTOGRAMA TIEMPO-AREA ====

hist_ta <- normalizar_histograma_tiempo_area(
  curva = curva_014,
  A_km2 = A_km2,
  dt_h = dt_h
)


# 9. CONSTRUIR HU DE CLARK ====

HU_base <- construir_HU_Clark_base(
  hist_ta = hist_ta,
  A_km2 = A_km2,
  tc_h = tc_h,
  dt_h = dt_h,
  K_h = K_h
)

Qp_Clark <- max(HU_base$q_unitario_m3_s_por_mm, na.rm = TRUE)
tiempo_pico_h <- HU_base$tiempo_h[which.max(HU_base$q_unitario_m3_s_por_mm)][1]
volumen_unitario_objetivo_m3 <- A_km2 * 1000
volumen_HU_m3 <- sum(HU_base$q_unitario_m3_s_por_mm, na.rm = TRUE) * dt_h * 3600
error_volumen_pct <- 100 * (volumen_HU_m3 - volumen_unitario_objetivo_m3) / volumen_unitario_objetivo_m3

HU_base <- HU_base %>%
  mutate(
    q_adim = if (Qp_Clark > 0) {
      q_unitario_m3_s_por_mm / Qp_Clark
    } else {
      rep(0, n())
    },
    t_adim = if (tiempo_pico_h > 0) {
      tiempo_h / tiempo_pico_h
    } else {
      rep(0, n())
    }
  )


# 10. DUPLICAR POR PERIODOS DE RETORNO SI EXISTEN ====

T_disponibles <- NULL
if (duplicar_por_periodo_retorno && !is.null(lluvia_010) && "T_anios" %in% names(lluvia_010)) {
  T_disponibles <- sort(unique(suppressWarnings(as.numeric(lluvia_010$T_anios))))
  T_disponibles <- T_disponibles[is.finite(T_disponibles)]
}

if (is.null(T_disponibles) || length(T_disponibles) == 0) {
  HU_largo <- HU_base %>% mutate(T_anios = NA_real_)
} else {
  HU_largo <- bind_rows(lapply(T_disponibles, function(Ti) {
    HU_base %>% mutate(T_anios = Ti)
  })) %>%
    relocate(T_anios, .before = paso)
}


# 11. RESUMEN ====

resumen <- tibble(
  A_km2 = A_km2,
  tc_h = tc_h,
  tc_min = tc_h * 60,
  dt_min = dt_min,
  dt_h = dt_h,
  dt_min_origen = case_when(
    valor_valido(dt_min_manual) ~ "manual",
    valor_valido(dt_min_010) ~ "script_010",
    valor_valido(dt_min_014) ~ "script_014",
    TRUE ~ "defecto_30_min"
  ),
  modo_K = modo_K,
  K_fraccion_tc = if_else(modo_K == "fraccion_tc", K_fraccion_tc, NA_real_),
  K_h = K_h,
  K_min = K_h * 60,
  n_intervalos_tiempo_area = nrow(hist_ta),
  n_pasos_HU = nrow(HU_base),
  duracion_HU_h = max(HU_base$tiempo_h, na.rm = TRUE),
  tiempo_pico_h = tiempo_pico_h,
  Qp_unitario_Clark_m3_s_por_mm = Qp_Clark,
  volumen_unitario_objetivo_m3 = volumen_unitario_objetivo_m3,
  volumen_HU_m3 = volumen_HU_m3,
  error_volumen_pct = error_volumen_pct,
  factor_correccion_volumen = HU_base$factor_correccion_volumen[1],
  periodos_retorno_duplicados = ifelse(
    is.null(T_disponibles) || length(T_disponibles) == 0,
    "ninguno",
    paste(T_disponibles, collapse = ", ")
  )
)

componentes <- HU_base %>%
  select(
    paso, tiempo_h, dt_h, dt_min, A_km2, tc_h, K_h,
    I_traslacion_m3_s_por_mm,
    q_unitario_m3_s_por_mm,
    q_adim, t_adim
  )


# 12. EXPORTAR TABLAS ====

write_csv(HU_largo, salida_HU_largo)
write_csv(HU_base, salida_HU_base)
write_csv(resumen, salida_resumen)
write_csv(componentes, salida_componentes)


# 13. FIGURAS ====

HU_plot <- HU_base %>%
  select(tiempo_h, I_traslacion_m3_s_por_mm, q_unitario_m3_s_por_mm) %>%
  pivot_longer(
    cols = c(I_traslacion_m3_s_por_mm, q_unitario_m3_s_por_mm),
    names_to = "Serie",
    values_to = "Q_m3_s_por_mm"
  ) %>%
  mutate(
    Serie = recode(
      Serie,
      I_traslacion_m3_s_por_mm = "Traslacion tiempo-area",
      q_unitario_m3_s_por_mm = "HU Clark con almacenamiento"
    )
  )

g1 <- ggplot(HU_base, aes(x = tiempo_h, y = q_unitario_m3_s_por_mm)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  labs(
    title = "Hidrograma unitario de Clark",
    subtitle = paste0(
      "A = ", round(A_km2, 3), " km² | Tc = ", round(tc_h, 3),
      " h | K = ", round(K_h, 3), " h | dt = ", round(dt_min), " min"
    ),
    x = "Tiempo desde el inicio del HU (h)",
    y = "q unitario (m³/s por mm)"
  ) +
  theme_minimal()

ggsave(salida_figura_HU, g1, width = 9, height = 6, dpi = 300)


g2 <- ggplot(HU_plot, aes(x = tiempo_h, y = Q_m3_s_por_mm, linetype = Serie)) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Componentes del hidrograma unitario de Clark",
    subtitle = "Traslacion por tiempo-area y almacenamiento lineal",
    x = "Tiempo desde el inicio del HU (h)",
    y = "q unitario (m³/s por mm)",
    linetype = "Serie"
  ) +
  theme_minimal()

ggsave(salida_figura_componentes, g2, width = 9, height = 6, dpi = 300)

print(g1)
print(g2)


# 14. RESUMEN EN CONSOLA ====

cat("\n====================================================\n")
cat("SCRIPT 015 FINALIZADO\n")
cat("====================================================\n")

cat("\nEntradas:\n")
cat(archivo_curva_014, "\n")
if (file.exists(archivo_resumen_014)) cat(archivo_resumen_014, "\n")
if (!is.na(archivo_010)) cat(archivo_010, "\n")

cat("\nConfiguracion Clark:\n")
cat("A =", round(A_km2, 4), "km2\n")
cat("Tc =", round(tc_h, 4), "h\n")
cat("dt =", round(dt_min, 3), "min\n")
cat("origen dt =", resumen$dt_min_origen[1], "\n")
cat("modo_K =", modo_K, "\n")
cat("K =", round(K_h, 4), "h\n")
cat("corregir_volumen =", corregir_volumen, "\n")

cat("\nResultados HU Clark:\n")
cat("Qp unitario =", round(Qp_Clark, 4), "m3/s por mm\n")
cat("Tiempo al pico =", round(tiempo_pico_h, 4), "h\n")
cat("Duracion HU =", round(max(HU_base$tiempo_h), 4), "h\n")
cat("Error volumen =", round(error_volumen_pct, 5), "%\n")

cat("\nTablas generadas:\n")
cat(salida_HU_largo, "\n")
cat(salida_HU_base, "\n")
cat(salida_resumen, "\n")
cat(salida_componentes, "\n")

cat("\nFiguras generadas:\n")
cat(salida_figura_HU, "\n")
cat(salida_figura_componentes, "\n")

cat("\nResumen:\n")
print(resumen %>% mutate(across(where(is.numeric), ~ round(.x, 4))))
cat("====================================================\n")
