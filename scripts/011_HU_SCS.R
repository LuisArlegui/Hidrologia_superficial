############################################################
#
# 011_HU_SCS.R
#
# Objetivo:
# - Leer la lluvia efectiva generada por el script 010
# - Leer el area de cuenca calculada en el script 005
# - Calcular Tlag, Tp y Qp unitario del hidrograma unitario SCS
# - Generar el hidrograma unitario adimensional SCS escalado a la cuenca
# - Exportar la tabla tiempo_h, q_unitario_m3_s_por_mm para convolucion
#
############################################################


# 0. PAQUETES ====

paquetes <- c("dplyr", "readr", "ggplot2", "tidyr")

instalar <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(instalar) > 0) install.packages(instalar)

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)


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

archivo_lluvia_efectiva_010 <- "salidas/tablas/010_lluvia_efectiva_SCS_CN_largo.csv"
archivo_resumen_010 <- "salidas/tablas/010_lluvia_efectiva_SCS_CN_resumen.csv"
archivo_parametros_005 <- "salidas/tablas/parametros_hidrologicos_resumen.csv"

salida_HU_largo <- "salidas/tablas/011_HU_SCS_largo.csv"
salida_HU_resumen <- "salidas/tablas/011_HU_SCS_resumen.csv"
salida_HU_adimensional <- "salidas/tablas/011_HU_SCS_adimensional.csv"
salida_figura_HU <- "salidas/figuras/011_HU_SCS.png"
salida_figura_HU_adim <- "salidas/figuras/011_HU_SCS_adimensional.png"


# 3. PARAMETROS EDITABLES ====

# Periodos de retorno que se desean procesar.
# Si se deja NULL, se usan todos los T_anios disponibles en el script 010.
T_seleccionados <- NULL
# Ejemplo:
# T_seleccionados <- c(10, 25, 100, 500)

# Relacion SCS habitual entre tiempo de retraso y tiempo de concentracion.
# Valor clasico aproximado: Tlag = 0.6 * Tc.
factor_Tlag_Tc <- 0.60

# Duracion efectiva del bloque de lluvia.
# Opciones:
# - "dt_010" -> usa el dt_h del hietograma efectivo del script 010
# - valor numerico en horas, por ejemplo 0.5
D_efectiva_h <- "dt_010"

# Paso temporal de salida del hidrograma unitario.
# Opciones:
# - "dt_010" -> usa el mismo paso temporal que el hietograma efectivo
# - valor numerico en minutos, por ejemplo 15
paso_salida_min <- "dt_010"

# Tiempo maximo del HU como multiplo de Tp.
# La tabla adimensional SCS llega normalmente hasta t/Tp = 5.
factor_tmax_Tp <- 5

# Correccion de volumen.
# Si TRUE, escala ligeramente las ordenadas interpoladas para que el volumen
# bajo el HU sea exactamente 1 mm sobre el area de cuenca.
corregir_volumen_unitario <- TRUE

# Coeficiente metrico del HU-SCS triangular/adimensional:
# Qp = 0.208 * A_km2 * Pe_mm / Tp_h
# Para Pe = 1 mm: Qp_unitario = 0.208 * A_km2 / Tp_h
coef_Qp_SCS <- 0.208


# 4. COMPROBAR ARCHIVOS DE ENTRADA ====

if (!file.exists(archivo_lluvia_efectiva_010)) {
  stop("No se encuentra la tabla larga del script 010: ", archivo_lluvia_efectiva_010)
}

if (!file.exists(archivo_resumen_010)) {
  stop("No se encuentra la tabla resumen del script 010: ", archivo_resumen_010)
}

if (!file.exists(archivo_parametros_005)) {
  stop("No se encuentra la tabla resumen del script 005: ", archivo_parametros_005)
}


# 5. LEER DATOS ====

lluvia_efectiva <- read_csv(archivo_lluvia_efectiva_010, show_col_types = FALSE)
resumen_010 <- read_csv(archivo_resumen_010, show_col_types = FALSE)
parametros_005 <- read_csv(archivo_parametros_005, show_col_types = FALSE)

campos_lluvia <- c("T_anios", "dt_h", "dt_min", "tc_h")
faltan_lluvia <- setdiff(campos_lluvia, names(lluvia_efectiva))

if (length(faltan_lluvia) > 0) {
  stop(
    "Faltan campos necesarios en el archivo 010: ",
    paste(faltan_lluvia, collapse = ", "),
    "\nCampos disponibles: ", paste(names(lluvia_efectiva), collapse = ", ")
  )
}

if (!("area_cuenca_km2" %in% names(parametros_005))) {
  stop(
    "El archivo del script 005 debe contener el campo area_cuenca_km2.\n",
    "Campos disponibles: ", paste(names(parametros_005), collapse = ", ")
  )
}

lluvia_efectiva <- lluvia_efectiva %>%
  mutate(
    T_anios = as.numeric(T_anios),
    dt_h = as.numeric(dt_h),
    dt_min = as.numeric(dt_min),
    tc_h = as.numeric(tc_h)
  )

A_km2 <- as.numeric(parametros_005$area_cuenca_km2[1])

if (!is.finite(A_km2) || A_km2 <= 0) stop("area_cuenca_km2 no es valida.")
if (any(!is.finite(lluvia_efectiva$T_anios))) stop("T_anios contiene valores no validos.")
if (any(!is.finite(lluvia_efectiva$dt_h) | lluvia_efectiva$dt_h <= 0)) stop("dt_h contiene valores no validos.")
if (any(!is.finite(lluvia_efectiva$tc_h) | lluvia_efectiva$tc_h <= 0)) stop("tc_h contiene valores no validos.")

if (!is.null(T_seleccionados)) {
  lluvia_efectiva <- lluvia_efectiva %>%
    filter(T_anios %in% T_seleccionados)
  resumen_010 <- resumen_010 %>%
    filter(T_anios %in% T_seleccionados)
  
  if (nrow(lluvia_efectiva) == 0) {
    stop("Ninguno de los T_seleccionados existe en la tabla del script 010.")
  }
}


# 6. TABLA ADIMENSIONAL SCS ====

# Ordenadas adimensionales habituales del hidrograma unitario SCS.
# x = t/Tp; y = q/Qp.
HU_adimensional <- tibble::tibble(
  t_Tp = c(
    0.0, 0.1, 0.2, 0.3, 0.4,
    0.5, 0.6, 0.7, 0.8, 0.9,
    1.0, 1.1, 1.2, 1.3, 1.4,
    1.5, 1.6, 1.7, 1.8, 1.9,
    2.0, 2.2, 2.4, 2.6, 2.8,
    3.0, 3.5, 4.0, 4.5, 5.0
  ),
  q_Qp = c(
    0.000, 0.030, 0.100, 0.190, 0.310,
    0.470, 0.660, 0.820, 0.930, 0.990,
    1.000, 0.990, 0.930, 0.860, 0.780,
    0.680, 0.560, 0.460, 0.390, 0.330,
    0.280, 0.207, 0.147, 0.107, 0.077,
    0.055, 0.020, 0.007, 0.002, 0.000
  )
)

write_csv(HU_adimensional, salida_HU_adimensional)


# 7. FUNCIONES AUXILIARES ====

obtener_D_efectiva_h <- function(dt_h_010) {
  if (is.character(D_efectiva_h) && D_efectiva_h == "dt_010") {
    return(dt_h_010)
  }
  
  D <- suppressWarnings(as.numeric(D_efectiva_h))
  if (!is.finite(D) || D <= 0) {
    stop("D_efectiva_h debe ser 'dt_010' o un numero positivo en horas.")
  }
  return(D)
}

obtener_paso_salida_h <- function(dt_h_010) {
  if (is.character(paso_salida_min) && paso_salida_min == "dt_010") {
    return(dt_h_010)
  }
  
  paso_min <- suppressWarnings(as.numeric(paso_salida_min))
  if (!is.finite(paso_min) || paso_min <= 0) {
    stop("paso_salida_min debe ser 'dt_010' o un numero positivo en minutos.")
  }
  return(paso_min / 60)
}

integrar_trapecios <- function(x, y) {
  if (length(x) < 2) return(0)
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2, na.rm = TRUE)
}

construir_HU_T <- function(T_anios_i, tc_h_i, dt_h_010_i) {
  if (!is.finite(factor_Tlag_Tc) || factor_Tlag_Tc <= 0) {
    stop("factor_Tlag_Tc debe ser positivo.")
  }
  
  D_h <- obtener_D_efectiva_h(dt_h_010_i)
  paso_h <- obtener_paso_salida_h(dt_h_010_i)
  
  Tlag_h <- factor_Tlag_Tc * tc_h_i
  Tp_h <- D_h / 2 + Tlag_h
  Qp_unitario_teorico <- coef_Qp_SCS * A_km2 / Tp_h
  
  if (!is.finite(Tp_h) || Tp_h <= 0) stop("Tp_h no es valido.")
  if (!is.finite(Qp_unitario_teorico) || Qp_unitario_teorico <= 0) stop("Qp unitario no es valido.")
  
  tmax_h <- max(factor_tmax_Tp * Tp_h, max(HU_adimensional$t_Tp) * Tp_h)
  tiempos_h <- seq(0, tmax_h, by = paso_h)
  
  if (tail(tiempos_h, 1) < tmax_h) {
    tiempos_h <- c(tiempos_h, tmax_h)
  }
  
  t_Tp <- tiempos_h / Tp_h
  q_Qp <- approx(
    x = HU_adimensional$t_Tp,
    y = HU_adimensional$q_Qp,
    xout = t_Tp,
    rule = 2,
    yleft = 0,
    yright = 0
  )$y
  
  q_unitario <- Qp_unitario_teorico * q_Qp
  
  volumen_objetivo_m3_por_mm <- 1000 * A_km2
  volumen_calculado_m3_por_mm <- integrar_trapecios(tiempos_h, q_unitario) * 3600
  factor_correccion_volumen <- 1
  
  if (corregir_volumen_unitario) {
    if (!is.finite(volumen_calculado_m3_por_mm) || volumen_calculado_m3_por_mm <= 0) {
      stop("No se puede corregir el volumen del HU: volumen calculado no valido.")
    }
    factor_correccion_volumen <- volumen_objetivo_m3_por_mm / volumen_calculado_m3_por_mm
    q_unitario <- q_unitario * factor_correccion_volumen
  }
  
  volumen_final_m3_por_mm <- integrar_trapecios(tiempos_h, q_unitario) * 3600
  Qp_unitario_final <- max(q_unitario, na.rm = TRUE)
  tiempo_pico_h <- tiempos_h[which.max(q_unitario)][1]
  
  hu <- tibble::tibble(
    T_anios = T_anios_i,
    tiempo_h = tiempos_h,
    t_Tp = t_Tp,
    q_Qp = q_Qp,
    q_unitario_m3_s_por_mm = q_unitario,
    A_km2 = A_km2,
    tc_h = tc_h_i,
    D_h = D_h,
    Tlag_h = Tlag_h,
    Tp_h = Tp_h,
    Qp_unitario_teorico_m3_s_por_mm = Qp_unitario_teorico,
    Qp_unitario_final_m3_s_por_mm = Qp_unitario_final,
    factor_correccion_volumen = factor_correccion_volumen,
    volumen_objetivo_m3_por_mm = volumen_objetivo_m3_por_mm,
    volumen_final_m3_por_mm = volumen_final_m3_por_mm
  )
  
  resumen <- tibble::tibble(
    T_anios = T_anios_i,
    A_km2 = A_km2,
    tc_h = tc_h_i,
    tc_min = tc_h_i * 60,
    D_h = D_h,
    D_min = D_h * 60,
    Tlag_h = Tlag_h,
    Tlag_min = Tlag_h * 60,
    Tp_h = Tp_h,
    Tp_min = Tp_h * 60,
    tiempo_pico_h = tiempo_pico_h,
    Qp_unitario_teorico_m3_s_por_mm = Qp_unitario_teorico,
    Qp_unitario_final_m3_s_por_mm = Qp_unitario_final,
    factor_correccion_volumen = factor_correccion_volumen,
    volumen_objetivo_m3_por_mm = volumen_objetivo_m3_por_mm,
    volumen_final_m3_por_mm = volumen_final_m3_por_mm,
    error_volumen_pct = 100 * (volumen_final_m3_por_mm - volumen_objetivo_m3_por_mm) / volumen_objetivo_m3_por_mm,
    factor_Tlag_Tc = factor_Tlag_Tc,
    coef_Qp_SCS = coef_Qp_SCS,
    corregir_volumen_unitario = corregir_volumen_unitario
  )
  
  list(hu = hu, resumen = resumen)
}


# 8. CALCULAR HU-SCS ====

parametros_por_T <- lluvia_efectiva %>%
  group_by(T_anios) %>%
  summarise(
    tc_h = first(tc_h),
    dt_h = first(dt_h),
    dt_min = first(dt_min),
    .groups = "drop"
  ) %>%
  arrange(T_anios)

lista_HU <- lapply(seq_len(nrow(parametros_por_T)), function(i) {
  construir_HU_T(
    T_anios_i = parametros_por_T$T_anios[i],
    tc_h_i = parametros_por_T$tc_h[i],
    dt_h_010_i = parametros_por_T$dt_h[i]
  )
})

HU_largo <- bind_rows(lapply(lista_HU, `[[`, "hu"))
HU_resumen <- bind_rows(lapply(lista_HU, `[[`, "resumen"))


# 9. EXPORTAR TABLAS ====

write_csv(HU_largo, salida_HU_largo)
write_csv(HU_resumen, salida_HU_resumen)


# 10. FIGURAS ====

HU_plot <- HU_largo %>%
  mutate(T_label = paste0("T = ", T_anios, " años"))

g_HU <- ggplot(HU_plot, aes(x = tiempo_h, y = q_unitario_m3_s_por_mm)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ T_label, scales = "free_y") +
  labs(
    title = "Hidrograma unitario SCS",
    subtitle = paste0(
      "A = ", round(A_km2, 3), " km²",
      " | Tlag = ", round(unique(HU_resumen$Tlag_h)[1], 3), " h",
      " | Tp = ", round(unique(HU_resumen$Tp_h)[1], 3), " h"
    ),
    x = "Tiempo desde el inicio del HU (h)",
    y = "q unitario (m³/s por mm)"
  ) +
  theme_minimal()

ggsave(
  salida_figura_HU,
  g_HU,
  width = 10,
  height = 7,
  dpi = 300
)

print(g_HU)

g_HU_adim <- ggplot(HU_adimensional, aes(x = t_Tp, y = q_Qp)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(
    title = "Hidrograma unitario adimensional SCS",
    x = "t / Tp",
    y = "q / Qp"
  ) +
  theme_minimal()

ggsave(
  salida_figura_HU_adim,
  g_HU_adim,
  width = 7,
  height = 5,
  dpi = 300
)

print(g_HU_adim)


# 11. RESUMEN EN CONSOLA ====

cat("\n====================================================\n")
cat("SCRIPT 011 FINALIZADO\n")
cat("====================================================\n")

cat("\nArchivos de entrada:\n")
cat(archivo_lluvia_efectiva_010, "\n")
cat(archivo_parametros_005, "\n")

cat("\nConfiguracion HU-SCS:\n")
cat("A =", round(A_km2, 4), "km2\n")
cat("factor_Tlag_Tc =", factor_Tlag_Tc, "\n")
cat("D_efectiva_h =", as.character(D_efectiva_h), "\n")
cat("paso_salida_min =", as.character(paso_salida_min), "\n")
cat("factor_tmax_Tp =", factor_tmax_Tp, "\n")
cat("corregir_volumen_unitario =", corregir_volumen_unitario, "\n")

cat("\nPeriodos de retorno procesados:\n")
cat(paste(sort(unique(HU_largo$T_anios)), collapse = ", "), "años\n")

cat("\nTablas generadas:\n")
cat(salida_HU_largo, "\n")
cat(salida_HU_resumen, "\n")
cat(salida_HU_adimensional, "\n")

cat("\nFiguras generadas:\n")
cat(salida_figura_HU, "\n")
cat(salida_figura_HU_adim, "\n")

cat("\nResumen HU-SCS:\n")
print(
  HU_resumen %>%
    mutate(across(where(is.numeric), ~ round(.x, 4)))
)

cat("====================================================\n")
