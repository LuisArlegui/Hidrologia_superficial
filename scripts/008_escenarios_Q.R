############################################################
#
# 008_escenarios_Q.R
#
# Objetivo:
# - Evaluar escenarios de gestión/modificación de la cuenca
# - Recalcular tc, I(T,tc), P0, C y Q
# - Comparar cada escenario con el escenario base
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

dir.create("salidas/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/figuras", recursive = TRUE, showWarnings = FALSE)


# 2. RUTAS ====

archivo_param_005 <- "salidas/tablas/parametros_hidrologicos_resumen.csv"
archivo_estat_P0  <- "salidas/tablas/estadisticos_P0.csv"
archivo_morf      <- "salidas/tablas/morfometria_cauce_principal.csv"
archivo_lluvias   <- "salidas/tablas/lluvias_diseno_comparacion.csv"

salida_escenarios <- "salidas/tablas/008_escenarios_Q.csv"
salida_resumen    <- "salidas/tablas/008_resumen_escenarios_Q.csv"

salida_fig_Q      <- "salidas/figuras/008_Q_vs_T_escenarios.png"
salida_fig_red    <- "salidas/figuras/008_reduccion_Q_escenarios.png"
salida_fig_C      <- "salidas/figuras/008_C_vs_T_escenarios.png"


# 3. PARAMETROS EDITABLES ====

metodo_Pd <- "SQRT_ETmax"

region_norma <- "93"
usar_delta <- "Delta50"

I1_Id_base <- 10


# 4. TABLA BETA 5.2-IC ====

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


# 5. DEFINICION DE ESCENARIOS ====

# factor_P0:
#   >1 aumenta el umbral de escorrentía; simula más infiltración/retención.
#
# factor_Jc:
#   <1 reduce pendiente efectiva del cauce/cuenca; simula abancalamiento,
#   terrazas, medidas de disipación, etc.
#
# factor_Lc:
#   >1 aumenta recorrido hidráulico efectivo.
#
# factor_I1_Id:
#   modifica torrencialidad local.
#
# factor_laminacion_Q:
#   <1 reduce directamente Q por almacenamiento/laminación,
#   por ejemplo depósitos de tormenta o humedales.

escenarios <- tibble::tibble(
  escenario = c(
    "Base",
    "Reforestacion_moderada",
    "Reforestacion_intensa",
    "Abancalamiento",
    "Depositos_tormenta",
    "Combinado" # intenta ser escenario de "maxima internvecion razonable"
  ),
  factor_P0 = c(1.00, 1.10, 1.25, 1.10, 1.00, 1.35),
  factor_Jc = c(1.00, 0.95, 0.90, 0.70, 1.00, 0.65),
  factor_Lc = c(1.00, 1.03, 1.05, 1.15, 1.00, 1.20),
  factor_I1_Id = c(1.00, 1.00, 1.00, 1.00, 1.00, 1.00),
  factor_laminacion_Q = c(1.00, 1.00, 1.00, 1.00, 0.80, 0.70)
)


# 6. COMPROBAR Y LEER ENTRADAS ====

archivos <- c(
  archivo_param_005,
  archivo_estat_P0,
  archivo_morf,
  archivo_lluvias
)

faltan_archivos <- archivos[!file.exists(archivos)]

if (length(faltan_archivos) > 0) {
  stop(
    "Faltan archivos de entrada:\n",
    paste(faltan_archivos, collapse = "\n")
  )
}

param_005 <- read_csv(archivo_param_005, show_col_types = FALSE)
estat_P0  <- read_csv(archivo_estat_P0, show_col_types = FALSE)
morf      <- read_csv(archivo_morf, show_col_types = FALSE)
lluvias   <- read_csv(archivo_lluvias, show_col_types = FALSE)


# 7. COMPROBACIONES DE CAMPOS ====

if (!("area_cuenca_km2" %in% names(param_005))) {
  stop("Falta area_cuenca_km2 en parametros_hidrologicos_resumen.csv")
}

if (!("PO_ponderado_mm" %in% names(estat_P0))) {
  stop("Falta PO_ponderado_mm en estadisticos_P0.csv")
}

if (!all(c("Lc_km", "Jc") %in% names(morf))) {
  stop("Faltan Lc_km o Jc en morfometria_cauce_principal.csv")
}

if (!all(c("T", metodo_Pd) %in% names(lluvias))) {
  stop("Faltan T o el método de Pd seleccionado en lluvias_diseno_comparacion.csv")
}

if (!(region_norma %in% tabla_beta$region)) {
  stop("La región seleccionada no existe en tabla_beta.")
}

if (!(usar_delta %in% names(tabla_beta))) {
  stop("El campo usar_delta no existe en tabla_beta.")
}


# 8. PARAMETROS BASE ====

A_km2_base <- param_005$area_cuenca_km2[1]
P0i_base   <- estat_P0$PO_ponderado_mm[1]
Lc_base    <- morf$Lc_km[1]
Jc_base    <- morf$Jc[1]

beta_reg <- tabla_beta %>% filter(region == region_norma)
Delta_usada <- beta_reg[[usar_delta]][1]

if (!is.finite(A_km2_base) || A_km2_base <= 0) stop("Área no válida.")
if (!is.finite(P0i_base) || P0i_base <= 0) stop("P0i no válido.")
if (!is.finite(Lc_base) || Lc_base <= 0) stop("Lc no válido.")
if (!is.finite(Jc_base) || Jc_base <= 0) stop("Jc no válido.")


# 9. FUNCIONES AUXILIARES ====

obtener_FT <- function(T, beta_reg) {
  
  if (T %in% c(2, 5, 25, 100, 500)) {
    campo <- paste0("FT_", T)
    return(as.numeric(beta_reg[[campo]]))
  }
  
  T_base <- c(2, 5, 25, 100, 500)
  FT_base <- c(
    beta_reg$FT_2,
    beta_reg$FT_5,
    beta_reg$FT_25,
    beta_reg$FT_100,
    beta_reg$FT_500
  )
  
  approx(
    x = log10(T_base),
    y = FT_base,
    xout = log10(T),
    rule = 2
  )$y
}


calcular_tc_52IC <- function(Lc_km, Jc) {
  0.3 * Lc_km^0.76 * Jc^(-0.19)
}


calcular_KA <- function(A_km2) {
  ifelse(
    A_km2 < 1,
    1,
    1 - log10(A_km2) / 15
  )
}


calcular_C_52IC <- function(Pd_mm, KA, P0_mm) {
  
  x <- (Pd_mm * KA) / P0_mm
  
  ifelse(
    x > 1,
    ((x - 1) * (x + 23)) / ((x + 11)^2),
    0
  )
}


# 10. PREPARAR LLUVIAS ====

lluvias_sel <- lluvias %>%
  transmute(
    T_anios = T,
    Pd_mm = .data[[metodo_Pd]]
  )


# 11. CALCULO POR ESCENARIOS ====

resultado_esc <- tidyr::crossing(
  escenarios,
  lluvias_sel
) %>%
  rowwise() %>%
  mutate(
    A_km2 = A_km2_base,
    
    P0i_mm = P0i_base * factor_P0,
    Lc_km = Lc_base * factor_Lc,
    Jc = Jc_base * factor_Jc,
    I1_Id = I1_Id_base * factor_I1_Id,
    
    tc_h = calcular_tc_52IC(Lc_km, Jc),
    tc_min = tc_h * 60,
    
    KA = calcular_KA(A_km2),
    
    FT = obtener_FT(T_anios, beta_reg),
    beta_m = beta_reg$beta_m[1],
    Delta_usada = Delta_usada,
    beta = (beta_m - Delta_usada) * FT,
    P0_mm = P0i_mm * beta,
    
    Id_mm_h = Pd_mm * KA / 24,
    Fa = I1_Id^(3.5287 - 2.5287 * tc_h^0.1),
    Fint = Fa,
    I_mm_h = Id_mm_h * Fint,
    
    Kt = 1 + tc_h^1.25 / (tc_h^1.25 + 14),
    
    C = calcular_C_52IC(Pd_mm, KA, P0_mm),
    
    Q_bruto_m3_s = (I_mm_h * C * A_km2 * Kt) / 3.6,
    Q_m3_s = Q_bruto_m3_s * factor_laminacion_Q
  ) %>%
  ungroup()


# 12. REDUCCION RESPECTO AL ESCENARIO BASE ====

base_Q <- resultado_esc %>%
  filter(escenario == "Base") %>%
  select(T_anios, Q_base_m3_s = Q_m3_s)

resultado_esc <- resultado_esc %>%
  left_join(base_Q, by = "T_anios") %>%
  mutate(
    reduccion_Q_pct = 100 * (Q_base_m3_s - Q_m3_s) / Q_base_m3_s
  )


# 13. RESUMEN POR ESCENARIO ====

resumen_esc <- resultado_esc %>%
  group_by(escenario) %>%
  summarise(
    Q_T2 = Q_m3_s[T_anios == 2][1],
    Q_T10 = Q_m3_s[T_anios == 10][1],
    Q_T25 = Q_m3_s[T_anios == 25][1],
    Q_T100 = Q_m3_s[T_anios == 100][1],
    Q_T500 = Q_m3_s[T_anios == 500][1],
    reduccion_media_pct = mean(reduccion_Q_pct, na.rm = TRUE),
    reduccion_max_pct = max(reduccion_Q_pct, na.rm = TRUE),
    .groups = "drop"
  )


# 14. EXPORTAR TABLAS ====

write_csv(resultado_esc, salida_escenarios)
write_csv(resumen_esc, salida_resumen)


# ==========================================================
# 15. FIGURAS
# ==========================================================

# Orden deseado de escenarios
niveles_escenarios <- c(
  "Base",
  "Reforestacion_moderada",
  "Reforestacion_intensa",
  "Abancalamiento",
  "Depositos_tormenta",
  "Combinado"
)

resultado_esc$escenario <- factor(
  resultado_esc$escenario,
  levels = niveles_escenarios
)

# Colores
colores_escenarios <- c(
  "Base" = "black",
  "Reforestacion_moderada" = "forestgreen",
  "Reforestacion_intensa" = "darkgreen",
  "Abancalamiento" = "sienna4",
  "Depositos_tormenta" = "royalblue",
  "Combinado" = "red3"
)

# Tipos de línea
tipos_escenarios <- c(
  "Base" = "solid",
  "Reforestacion_moderada" = "dashed",
  "Reforestacion_intensa" = "dotdash",
  "Abancalamiento" = "longdash",
  "Depositos_tormenta" = "twodash",
  "Combinado" = "dotted"
)


# ----------------------------------------------------------
# Q vs T
# ----------------------------------------------------------

g_Q <- ggplot(
  resultado_esc,
  aes(
    x = T_anios,
    y = Q_m3_s,
    color = escenario,
    linetype = escenario,
    shape= escenario
  )
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = colores_escenarios) +
  scale_linetype_manual(values = tipos_escenarios) +
  scale_x_log10(
    breaks = sort(unique(resultado_esc$T_anios))
  ) +
  labs(
    title = "Caudal punta por escenario",
    x = "Periodo de retorno, T (años)",
    y = expression(Q[T]~"(m"^3*"/s)"),
    color = "Escenario",
    linetype = "Escenario"
  ) +
  theme_gray()


# ----------------------------------------------------------
# Reducción porcentual
# ----------------------------------------------------------

g_red <- resultado_esc %>%
  filter(escenario != "Base") %>%
  ggplot(
    aes(
      x = T_anios,
      y = reduccion_Q_pct,
      color = escenario,
      linetype = escenario,
      shape= escenario
    )
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.5,
    color = "black"
  ) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = colores_escenarios) +
  scale_linetype_manual(values = tipos_escenarios) +
  scale_x_log10(
    breaks = sort(unique(resultado_esc$T_anios))
  ) +
  labs(
    title = "Reducción porcentual de Q respecto al escenario base",
    x = "Periodo de retorno, T (años)",
    y = "Reducción de Q (%)",
    color = "Escenario",
    linetype = "Escenario"
  ) +
  theme_gray()


# ----------------------------------------------------------
# C vs T
# ----------------------------------------------------------

g_C <- ggplot(
  resultado_esc,
  aes(
    x = T_anios,
    y = C,
    color = escenario,
    linetype = escenario,
    shape= escenario
  )
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = colores_escenarios) +
  scale_linetype_manual(values = tipos_escenarios) +
  scale_x_log10(
    breaks = sort(unique(resultado_esc$T_anios))
  ) +
  labs(
    title = "Coeficiente de escorrentía por escenario",
    x = "Periodo de retorno, T (años)",
    y = "C",
    color = "Escenario",
    linetype = "Escenario"
  ) +
  theme_gray()


# ----------------------------------------------------------
# Guardar figuras
# ----------------------------------------------------------

ggsave(salida_fig_Q, g_Q, width = 8, height = 5, dpi = 300)

ggsave(salida_fig_red, g_red, width = 8, height = 5, dpi = 300)

ggsave(salida_fig_C, g_C, width = 8, height = 5, dpi = 300)

print(g_Q)
print(g_red)
print(g_C)


# 16. RESUMEN EN CONSOLA ====

cat("\n====================================================\n")
cat("SCRIPT 008 FINALIZADO\n")
cat("====================================================\n")

cat("\nEscenarios evaluados:\n")
print(escenarios)

cat("\nParámetros base:\n")
cat("A =", round(A_km2_base, 4), "km2\n")
cat("P0i =", round(P0i_base, 3), "mm\n")
cat("Lc =", round(Lc_base, 4), "km\n")
cat("Jc =", round(Jc_base, 5), "\n")
cat("I1/Id =", I1_Id_base, "\n")

cat("\nTablas generadas:\n")
cat(salida_escenarios, "\n")
cat(salida_resumen, "\n")

cat("\nFiguras generadas:\n")
cat(salida_fig_Q, "\n")
cat(salida_fig_red, "\n")
cat(salida_fig_C, "\n")

cat("\nResumen por escenario:\n")
print(
  resumen_esc %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
)

cat("====================================================\n")