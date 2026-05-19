############################################################
#
# 007_metodo_racional_Q.R
#
# Objetivo:
# - Leer área de cuenca del script 005
# - Leer P0 ponderado del archivo estadisticos_P0.csv
# - Leer intensidades I(T,tc) del script 006
# - Calcular beta, P0 corregido, C y Q_T
# - Aplicar el método racional según Norma 5.2-IC
#
############################################################

# 0. PAQUETES ====

paquetes <- c("dplyr", "readr", "ggplot2")

instalar <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(instalar) > 0) install.packages(instalar)

library(dplyr)
library(readr)
library(ggplot2)


# 1. CONFIGURACION GENERAL ====

if (file.exists("scripts/00_configuracion.R")) {
  source("scripts/00_configuracion.R")
}

dir.create("salidas/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/figuras", recursive = TRUE, showWarnings = FALSE)


# 2. RUTAS ====

archivo_param_005 <- "salidas/tablas/parametros_hidrologicos_resumen.csv"
archivo_estat_P0  <- "salidas/tablas/estadisticos_P0.csv"
archivo_idf_006   <- "salidas/tablas/006_lluvia_IDF_tiempo_concentracion.csv"

salida_q <- "salidas/tablas/007_caudales_metodo_racional.csv"

salida_fig_q <- "salidas/figuras/007_Q_vs_T.png"
salida_fig_c <- "salidas/figuras/007_C_vs_T.png"


# 3. PARAMETROS EDITABLES ====

# Región de la tabla 2.5 de la Norma 5.2-IC
region_norma <- "93"

# Para la formulación:
# beta = (beta_m - Delta_50) * F_T
usar_delta <- "Delta50"


# 4. TABLA 2.5 NORMA 5.2-IC ====

# Primera versión: región 93.
# Se puede ampliar después con más regiones si interesa.

tabla_beta <- tibble::tibble(
  region = c("93"),
  beta_m = c(1.70),
  Delta50 = c(0.20),
  Delta67 = c(0.25),
  Delta90 = c(0.45),
  FT_2 = c(0.77),
  FT_5 = c(0.92),
  FT_25 = c(1.00),
  FT_100 = c(1.00),
  FT_500 = c(1.00)
)


# 5. COMPROBAR ARCHIVOS Y LEER DATOS ====

if (!file.exists(archivo_param_005)) {
  stop("No se encuentra el archivo del script 005: ", archivo_param_005)
}

if (!file.exists(archivo_estat_P0)) {
  stop("No se encuentra el archivo de estadísticos P0: ", archivo_estat_P0)
}

if (!file.exists(archivo_idf_006)) {
  stop("No se encuentra el archivo del script 006: ", archivo_idf_006)
}

param_005 <- read_csv(archivo_param_005, show_col_types = FALSE)
estat_P0  <- read_csv(archivo_estat_P0, show_col_types = FALSE)
idf_006   <- read_csv(archivo_idf_006, show_col_types = FALSE)


# 6. COMPROBACIONES ====

campos_005 <- c("area_cuenca_km2")
faltan_005 <- setdiff(campos_005, names(param_005))

if (length(faltan_005) > 0) {
  stop(
    "Faltan campos en parametros_hidrologicos_resumen.csv: ",
    paste(faltan_005, collapse = ", ")
  )
}

campos_P0 <- c("PO_ponderado_mm")
faltan_P0 <- setdiff(campos_P0, names(estat_P0))

if (length(faltan_P0) > 0) {
  stop(
    "Faltan campos en estadisticos_P0.csv: ",
    paste(faltan_P0, collapse = ", ")
  )
}

campos_006 <- c("T_anios", "Pd_mm", "KA", "I_mm_h", "Kt")
faltan_006 <- setdiff(campos_006, names(idf_006))

if (length(faltan_006) > 0) {
  stop(
    "Faltan campos en 006_lluvia_IDF_tiempo_concentracion.csv: ",
    paste(faltan_006, collapse = ", ")
  )
}

if (!(region_norma %in% tabla_beta$region)) {
  stop("La región ", region_norma, " no está incluida en tabla_beta.")
}

if (!(usar_delta %in% names(tabla_beta))) {
  stop(
    "El campo indicado en usar_delta no existe en tabla_beta: ",
    usar_delta
  )
}


# 7. EXTRAER PARAMETROS ====

A_km2 <- param_005$area_cuenca_km2[1]
P0i_mm <- estat_P0$PO_ponderado_mm[1]

if (!is.finite(A_km2) || A_km2 <= 0) {
  stop("Área de cuenca no válida.")
}

if (!is.finite(P0i_mm) || P0i_mm <= 0) {
  stop("P0 ponderado no válido.")
}

beta_reg <- tabla_beta %>%
  filter(region == region_norma)


# 8. FUNCIONES AUXILIARES ====

obtener_FT <- function(T, beta_reg) {
  
  if (T %in% c(2, 5, 25, 100, 500)) {
    campo <- paste0("FT_", T)
    return(as.numeric(beta_reg[[campo]]))
  }
  
  # Interpolación lineal en log10(T) para periodos intermedios
  T_base <- c(2, 5, 25, 100, 500)
  FT_base <- c(
    beta_reg$FT_2,
    beta_reg$FT_5,
    beta_reg$FT_25,
    beta_reg$FT_100,
    beta_reg$FT_500
  )
  
  if (T < min(T_base) || T > max(T_base)) {
    warning("T = ", T, " fuera del rango de la tabla 2.5. Se extrapola.")
  }
  
  FT <- approx(
    x = log10(T_base),
    y = FT_base,
    xout = log10(T),
    rule = 2
  )$y
  
  return(as.numeric(FT))
}


calcular_C_52IC <- function(Pd_mm, KA, P0_mm) {
  
  x <- (Pd_mm * KA) / P0_mm
  
  C <- ifelse(
    x > 1,
    ((x - 1) * (x + 23)) / ((x + 11)^2),
    0
  )
  
  return(C)
}


# 9. CALCULO DE beta, P0, C y Q ====

Delta_usada <- beta_reg[[usar_delta]][1]

resultado_q <- idf_006 %>%
  rowwise() %>%
  mutate(
    region_norma = region_norma,
    beta_m = beta_reg$beta_m[1],
    Delta_usada = Delta_usada,
    FT = obtener_FT(T_anios, beta_reg),
    beta = (beta_m - Delta_usada) * FT,
    P0i_mm = P0i_mm,
    P0_mm = P0i_mm * beta,
    C = calcular_C_52IC(Pd_mm, KA, P0_mm),
    
    # Método racional 5.2-IC
    # I en mm/h, A en km2, Q en m3/s
    Q_m3_s = (I_mm_h * C * A_km2 * Kt) / 3.6
  ) %>%
  ungroup()


# 10. EXPORTAR TABLA ====

write_csv(resultado_q, salida_q)


# 11. FIGURAS ====

g_q <- ggplot(resultado_q, aes(x = T_anios, y = Q_m3_s)) +
  geom_line() +
  geom_point() +
  scale_x_log10(breaks = resultado_q$T_anios) +
  labs(
    title = "Caudal punta por el método racional",
    subtitle = paste0("Región 5.2-IC: ", region_norma),
    x = "Periodo de retorno, T (años)",
    y = expression(Q[T]~"(m"^3*"/s)")
  ) +
  theme_gray()

g_c <- ggplot(resultado_q, aes(x = T_anios, y = C)) +
  geom_line() +
  geom_point() +
  scale_x_log10(breaks = resultado_q$T_anios) +
  labs(
    title = "Coeficiente de escorrentía C",
    subtitle = paste0("Región 5.2-IC: ", region_norma),
    x = "Periodo de retorno, T (años)",
    y = "C"
  ) +
  theme_gray()

ggsave(salida_fig_q, plot = g_q, width = 8, height = 5, dpi = 300)
ggsave(salida_fig_c, plot = g_c, width = 8, height = 5, dpi = 300)

print(g_q)
print(g_c)


# 12. RESUMEN EN CONSOLA ====

cat("\n====================================================\n")
cat("SCRIPT 007 FINALIZADO\n")
cat("====================================================\n")

cat("\nParámetros generales:\n")
cat("Región norma =", region_norma, "\n")
cat("A =", round(A_km2, 4), "km2\n")
cat("P0i ponderado =", round(P0i_mm, 3), "mm\n")
cat("beta_m =", round(beta_reg$beta_m[1], 3), "\n")
cat("Delta usada =", usar_delta, "=", round(Delta_usada, 3), "\n")

cat("\nTabla generada:\n")
cat(salida_q, "\n")

cat("\nFiguras generadas:\n")
cat(salida_fig_q, "\n")
cat(salida_fig_c, "\n")

cat("\nPrimeras filas:\n")
print(
  resultado_q %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
)

cat("====================================================\n")