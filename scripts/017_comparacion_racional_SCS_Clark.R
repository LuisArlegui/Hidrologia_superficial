############################################################
#
# 017_comparacion_racional_SCS_Clark.R
#
# Objetivo:
# - Comparar los tres procedimientos de estimacion de caudal punta:
#     1) Metodo racional
#     2) Hidrograma unitario SCS
#     3) Hidrograma unitario de Clark
# - Usar las salidas ya generadas en los scripts anteriores
# - Calcular ratios, diferencias absolutas y diferencias porcentuales
# - Comparar tiempos al pico y volumenes de escorrentia cuando esten disponibles
# - Exportar tablas y figuras resumen
#
############################################################


# 0. PAQUETES ====

paquetes <- c("dplyr", "readr", "ggplot2", "tidyr", "stringr", "scales")

instalar <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(instalar) > 0) install.packages(instalar)

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)


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

archivos_racional_candidatos <- c(
  "salidas/tablas/008_escenarios_Q.csv",
  "salidas/tablas/007_metodo_racional_Q.csv",
  "salidas/tablas/007_resultados_metodo_racional.csv",
  "salidas/tablas/013_comparacion_racional_vs_HU.csv"
)

archivo_SCS <- "salidas/tablas/012_hidrograma_final_resumen.csv"
archivo_Clark <- "salidas/tablas/016_hidrograma_Clark_resumen.csv"

salida_comparacion <- "salidas/tablas/017_comparacion_racional_SCS_Clark.csv"
salida_larga <- "salidas/tablas/017_comparacion_racional_SCS_Clark_larga.csv"
salida_resumen_ratios <- "salidas/tablas/017_resumen_ratios_metodos.csv"

salida_fig_Qp <- "salidas/figuras/017_Qp_tres_metodos.png"
salida_fig_ratio <- "salidas/figuras/017_ratios_tres_metodos.png"
salida_fig_tiempo_pico <- "salidas/figuras/017_tiempo_pico_SCS_Clark.png"
salida_fig_volumen <- "salidas/figuras/017_volumen_SCS_Clark.png"


# 3. PARAMETROS EDITABLES ====

# Si TRUE, usa escala logaritmica en el eje X de los periodos de retorno.
usar_x_log10 <- TRUE

# Si TRUE, muestra etiquetas numericas sobre las curvas de Qp.
etiquetar_Qp <- FALSE

# Nombres que se mostraran en las figuras.
nombre_racional <- "Racional"
nombre_SCS <- "HU-SCS"
nombre_Clark <- "HU-Clark"


# 4. FUNCIONES AUXILIARES ====

primer_archivo_existente <- function(candidatos, obligatorio = TRUE, etiqueta = "archivo") {
  existe <- candidatos[file.exists(candidatos)]
  if (length(existe) > 0) return(existe[1])

  if (obligatorio) {
    stop(
      "No se encontro ", etiqueta, ". Candidatos:\n",
      paste(candidatos, collapse = "\n")
    )
  }

  return(NA_character_)
}

buscar_columna <- function(tabla, candidatos, etiqueta, obligatorio = TRUE) {
  cols <- intersect(candidatos, names(tabla))

  if (length(cols) == 0) {
    mensaje <- paste0(
      "No se encontro columna para ", etiqueta, ".\n",
      "Candidatas: ", paste(candidatos, collapse = ", "), "\n",
      "Columnas disponibles: ", paste(names(tabla), collapse = ", ")
    )

    if (obligatorio) stop(mensaje) else warning(mensaje)
    return(NA_character_)
  }

  return(cols[1])
}

extraer_columna_num <- function(tabla, candidatos, etiqueta, obligatorio = TRUE) {
  col <- buscar_columna(tabla, candidatos, etiqueta, obligatorio = obligatorio)
  if (is.na(col)) return(rep(NA_real_, nrow(tabla)))
  suppressWarnings(as.numeric(tabla[[col]]))
}

normalizar_T <- function(x) {
  suppressWarnings(as.numeric(x))
}

preparar_racional <- function(tabla) {
  col_T <- buscar_columna(
    tabla,
    c("T_anios", "T", "Tr", "T_retorno", "periodo_retorno", "Periodo_retorno", "T_return"),
    "periodo de retorno racional"
  )

  col_Q <- buscar_columna(
    tabla,
    c(
      "Qp_racional_m3_s", "Q_racional_m3_s", "Q_m3_s", "Qp_m3_s",
      "Qmax_m3_s", "Q_pico_m3_s", "Q_diseno_m3_s", "Q_T_m3_s"
    ),
    "caudal racional"
  )

  out <- tibble(
    T_anios = normalizar_T(tabla[[col_T]]),
    Qp_racional_m3_s = suppressWarnings(as.numeric(tabla[[col_Q]]))
  )

  # Variables opcionales
  C_col <- intersect(c("C", "C_racional", "coef_escorrentia", "coef_escorrentia_racional"), names(tabla))
  if (length(C_col) > 0) out$C_racional <- suppressWarnings(as.numeric(tabla[[C_col[1]]]))

  I_col <- intersect(c("I_mm_h", "intensidad_mm_h", "I_T_tc_mm_h", "intensidad_diseno_mm_h"), names(tabla))
  if (length(I_col) > 0) out$I_racional_mm_h <- suppressWarnings(as.numeric(tabla[[I_col[1]]]))

  A_col <- intersect(c("A_km2", "area_km2", "Area_km2", "area_cuenca_km2"), names(tabla))
  if (length(A_col) > 0) out$A_racional_km2 <- suppressWarnings(as.numeric(tabla[[A_col[1]]]))

  tc_col <- intersect(c("tc_h", "Tc_h", "TC_h", "tiempo_concentracion_h"), names(tabla))
  if (length(tc_col) > 0) out$tc_racional_h <- suppressWarnings(as.numeric(tabla[[tc_col[1]]]))

  out %>%
    filter(is.finite(T_anios), is.finite(Qp_racional_m3_s)) %>%
    distinct(T_anios, .keep_all = TRUE)
}

preparar_HU <- function(tabla, metodo = c("SCS", "Clark")) {
  metodo <- match.arg(metodo)

  col_T <- buscar_columna(
    tabla,
    c("T_anios", "T", "Tr", "T_retorno", "periodo_retorno", "Periodo_retorno"),
    paste("periodo de retorno", metodo)
  )

  col_Q <- buscar_columna(
    tabla,
    c(
      "Qp_total_m3_s", "Qp_directo_m3_s", "Qp_m3_s", "Qmax_m3_s",
      "Q_pico_m3_s", "Qp_HU_m3_s", "Qp_Clark_m3_s", "Q_pico_total_m3_s"
    ),
    paste("caudal punta", metodo)
  )

  col_A <- buscar_columna(
    tabla,
    c("A_km2", "area_km2", "Area_km2", "area_total_Clark_km2", "area_cuenca_km2"),
    paste("area", metodo),
    obligatorio = FALSE
  )

  col_dt <- buscar_columna(
    tabla,
    c("dt_min", "Delta_t_min", "paso_min"),
    paste("dt", metodo),
    obligatorio = FALSE
  )

  col_P <- buscar_columna(
    tabla,
    c("P_total_mm", "P_total_HU_mm", "P_bruta_total_mm", "lluvia_total_mm"),
    paste("lluvia total", metodo),
    obligatorio = FALSE
  )

  col_Pe <- buscar_columna(
    tabla,
    c("Pe_total_mm", "Pe_total_HU_mm", "Pef_total_mm", "lluvia_efectiva_total_mm"),
    paste("lluvia efectiva", metodo),
    obligatorio = FALSE
  )

  col_Ce <- buscar_columna(
    tabla,
    c("coef_escorrentia_evento", "C_evento", "coef_evento", "coef_escorrentia"),
    paste("coeficiente de escorrentia", metodo),
    obligatorio = FALSE
  )

  col_tp <- buscar_columna(
    tabla,
    c("tiempo_pico_h", "Tp_h", "t_pico_h", "tiempo_Qp_h"),
    paste("tiempo al pico", metodo),
    obligatorio = FALSE
  )

  col_vol <- buscar_columna(
    tabla,
    c("volumen_total_m3", "volumen_directo_m3", "volumen_objetivo_m3", "V_total_m3"),
    paste("volumen", metodo),
    obligatorio = FALSE
  )

  col_err <- buscar_columna(
    tabla,
    c("error_volumen_directo_pct", "error_volumen_pct"),
    paste("error volumen", metodo),
    obligatorio = FALSE
  )

  pref <- if (metodo == "SCS") "SCS" else "Clark"

  out <- tibble(
    T_anios = normalizar_T(tabla[[col_T]]),
    !!paste0("Qp_", pref, "_m3_s") := suppressWarnings(as.numeric(tabla[[col_Q]])),
    !!paste0("A_", pref, "_km2") := if (!is.na(col_A)) suppressWarnings(as.numeric(tabla[[col_A]])) else NA_real_,
    !!paste0("dt_", pref, "_min") := if (!is.na(col_dt)) suppressWarnings(as.numeric(tabla[[col_dt]])) else NA_real_,
    !!paste0("P_total_", pref, "_mm") := if (!is.na(col_P)) suppressWarnings(as.numeric(tabla[[col_P]])) else NA_real_,
    !!paste0("Pe_total_", pref, "_mm") := if (!is.na(col_Pe)) suppressWarnings(as.numeric(tabla[[col_Pe]])) else NA_real_,
    !!paste0("C_evento_", pref) := if (!is.na(col_Ce)) suppressWarnings(as.numeric(tabla[[col_Ce]])) else NA_real_,
    !!paste0("tiempo_pico_", pref, "_h") := if (!is.na(col_tp)) suppressWarnings(as.numeric(tabla[[col_tp]])) else NA_real_,
    !!paste0("volumen_", pref, "_m3") := if (!is.na(col_vol)) suppressWarnings(as.numeric(tabla[[col_vol]])) else NA_real_,
    !!paste0("error_volumen_", pref, "_pct") := if (!is.na(col_err)) suppressWarnings(as.numeric(tabla[[col_err]])) else NA_real_
  )

  out %>%
    filter(is.finite(T_anios)) %>%
    distinct(T_anios, .keep_all = TRUE)
}


# 5. LEER ARCHIVOS ====

archivo_racional <- primer_archivo_existente(
  archivos_racional_candidatos,
  obligatorio = TRUE,
  etiqueta = "tabla del metodo racional"
)

if (!file.exists(archivo_SCS)) stop("No se encuentra el archivo SCS: ", archivo_SCS)
if (!file.exists(archivo_Clark)) stop("No se encuentra el archivo Clark: ", archivo_Clark)

racional_raw <- read_csv(archivo_racional, show_col_types = FALSE)
SCS_raw <- read_csv(archivo_SCS, show_col_types = FALSE)
Clark_raw <- read_csv(archivo_Clark, show_col_types = FALSE)

cat("\nArchivo racional usado:\n", archivo_racional, "\n", sep = "")
cat("Archivo HU-SCS usado:\n", archivo_SCS, "\n", sep = "")
cat("Archivo HU-Clark usado:\n", archivo_Clark, "\n", sep = "")


# 6. NORMALIZAR TABLAS ====

racional <- preparar_racional(racional_raw)
SCS <- preparar_HU(SCS_raw, metodo = "SCS")
Clark <- preparar_HU(Clark_raw, metodo = "Clark")

comparacion <- racional %>%
  full_join(SCS, by = "T_anios") %>%
  full_join(Clark, by = "T_anios") %>%
  arrange(T_anios)


# 7. CALCULOS COMPARATIVOS ====

comparacion <- comparacion %>%
  mutate(
    ratio_SCS_racional = Qp_SCS_m3_s / Qp_racional_m3_s,
    ratio_Clark_racional = Qp_Clark_m3_s / Qp_racional_m3_s,
    ratio_Clark_SCS = Qp_Clark_m3_s / Qp_SCS_m3_s,

    dif_SCS_menos_racional_m3_s = Qp_SCS_m3_s - Qp_racional_m3_s,
    dif_Clark_menos_racional_m3_s = Qp_Clark_m3_s - Qp_racional_m3_s,
    dif_Clark_menos_SCS_m3_s = Qp_Clark_m3_s - Qp_SCS_m3_s,

    incremento_SCS_respecto_racional_pct = 100 * (ratio_SCS_racional - 1),
    incremento_Clark_respecto_racional_pct = 100 * (ratio_Clark_racional - 1),
    incremento_Clark_respecto_SCS_pct = 100 * (ratio_Clark_SCS - 1),

    delta_t_pico_Clark_menos_SCS_h = tiempo_pico_Clark_h - tiempo_pico_SCS_h,
    ratio_volumen_Clark_SCS = volumen_Clark_m3 / volumen_SCS_m3,
    dif_volumen_Clark_menos_SCS_m3 = volumen_Clark_m3 - volumen_SCS_m3
  )

comparacion_larga_Q <- comparacion %>%
  select(T_anios, Qp_racional_m3_s, Qp_SCS_m3_s, Qp_Clark_m3_s) %>%
  pivot_longer(
    cols = starts_with("Qp_"),
    names_to = "metodo",
    values_to = "Qp_m3_s"
  ) %>%
  mutate(
    metodo = recode(
      metodo,
      "Qp_racional_m3_s" = nombre_racional,
      "Qp_SCS_m3_s" = nombre_SCS,
      "Qp_Clark_m3_s" = nombre_Clark
    ),
    metodo = factor(metodo, levels = c(nombre_racional, nombre_SCS, nombre_Clark))
  )

ratios_largo <- comparacion %>%
  select(T_anios, ratio_SCS_racional, ratio_Clark_racional, ratio_Clark_SCS) %>%
  pivot_longer(
    cols = starts_with("ratio_"),
    names_to = "comparacion",
    values_to = "ratio"
  ) %>%
  mutate(
    comparacion = recode(
      comparacion,
      "ratio_SCS_racional" = "HU-SCS / Racional",
      "ratio_Clark_racional" = "HU-Clark / Racional",
      "ratio_Clark_SCS" = "HU-Clark / HU-SCS"
    )
  )

resumen_ratios <- ratios_largo %>%
  group_by(comparacion) %>%
  summarise(
    ratio_min = min(ratio, na.rm = TRUE),
    ratio_medio = mean(ratio, na.rm = TRUE),
    ratio_max = max(ratio, na.rm = TRUE),
    .groups = "drop"
  )


# 8. EXPORTAR TABLAS ====

write_csv(comparacion, salida_comparacion)
write_csv(comparacion_larga_Q, salida_larga)
write_csv(resumen_ratios, salida_resumen_ratios)


# 9. FIGURAS ====

g_Q <- ggplot(comparacion_larga_Q, aes(x = T_anios, y = Qp_m3_s, group = metodo, linetype = metodo)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  labs(
    title = "Comparacion de caudal punta entre metodos",
    subtitle = "Metodo racional, hidrograma unitario SCS e hidrograma unitario de Clark",
    x = "Periodo de retorno, T (años)",
    y = expression(Q[p]~(m^3/s)),
    linetype = "Metodo"
  ) +
  theme_minimal()

if (usar_x_log10) {
  g_Q <- g_Q + scale_x_log10(breaks = sort(unique(comparacion_larga_Q$T_anios)))
}

if (etiquetar_Qp) {
  g_Q <- g_Q + geom_text(aes(label = round(Qp_m3_s, 1)), vjust = -0.7, size = 3)
}

ggsave(salida_fig_Qp, g_Q, width = 10, height = 6, dpi = 300)


g_ratio <- ggplot(ratios_largo, aes(x = T_anios, y = ratio, group = comparacion, linetype = comparacion)) +
  geom_hline(yintercept = 1, linewidth = 0.5, alpha = 0.6) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  labs(
    title = "Ratios entre metodos",
    subtitle = "Valores menores que 1 indican caudales menores en el numerador",
    x = "Periodo de retorno, T (años)",
    y = "Ratio de caudal punta",
    linetype = "Comparacion"
  ) +
  theme_minimal()

if (usar_x_log10) {
  g_ratio <- g_ratio + scale_x_log10(breaks = sort(unique(ratios_largo$T_anios)))
}

ggsave(salida_fig_ratio, g_ratio, width = 10, height = 6, dpi = 300)


if (any(is.finite(comparacion$tiempo_pico_SCS_h)) || any(is.finite(comparacion$tiempo_pico_Clark_h))) {
  tiempos_largo <- comparacion %>%
    select(T_anios, tiempo_pico_SCS_h, tiempo_pico_Clark_h) %>%
    pivot_longer(
      cols = starts_with("tiempo_pico"),
      names_to = "metodo",
      values_to = "tiempo_pico_h"
    ) %>%
    mutate(
      metodo = recode(
        metodo,
        "tiempo_pico_SCS_h" = nombre_SCS,
        "tiempo_pico_Clark_h" = nombre_Clark
      )
    )

  g_tp <- ggplot(tiempos_largo, aes(x = T_anios, y = tiempo_pico_h, group = metodo, linetype = metodo)) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    geom_point(size = 2, na.rm = TRUE) +
    labs(
      title = "Tiempo al pico en los metodos con hidrograma",
      x = "Periodo de retorno, T (años)",
      y = "Tiempo al pico (h)",
      linetype = "Metodo"
    ) +
    theme_minimal()

  if (usar_x_log10) {
    g_tp <- g_tp + scale_x_log10(breaks = sort(unique(tiempos_largo$T_anios)))
  }

  ggsave(salida_fig_tiempo_pico, g_tp, width = 10, height = 6, dpi = 300)
}


if (any(is.finite(comparacion$volumen_SCS_m3)) || any(is.finite(comparacion$volumen_Clark_m3))) {
  volumen_largo <- comparacion %>%
    select(T_anios, volumen_SCS_m3, volumen_Clark_m3) %>%
    pivot_longer(
      cols = starts_with("volumen"),
      names_to = "metodo",
      values_to = "volumen_m3"
    ) %>%
    mutate(
      metodo = recode(
        metodo,
        "volumen_SCS_m3" = nombre_SCS,
        "volumen_Clark_m3" = nombre_Clark
      )
    )

  g_vol <- ggplot(volumen_largo, aes(x = T_anios, y = volumen_m3, group = metodo, linetype = metodo)) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    geom_point(size = 2, na.rm = TRUE) +
    labs(
      title = "Volumen de escorrentia directa en los metodos con hidrograma",
      x = "Periodo de retorno, T (años)",
      y = expression(Volumen~(m^3)),
      linetype = "Metodo"
    ) +
    theme_minimal()

  if (usar_x_log10) {
    g_vol <- g_vol + scale_x_log10(breaks = sort(unique(volumen_largo$T_anios)))
  }

  ggsave(salida_fig_volumen, g_vol, width = 10, height = 6, dpi = 300)
}


# 10. SALIDA EN CONSOLA ====

cat("\n====================================================\n")
cat("SCRIPT 017 FINALIZADO\n")
cat("====================================================\n")

cat("\nArchivos de entrada:\n")
cat("Racional:", archivo_racional, "\n")
cat("HU-SCS:", archivo_SCS, "\n")
cat("HU-Clark:", archivo_Clark, "\n")

cat("\nTablas generadas:\n")
cat(salida_comparacion, "\n")
cat(salida_larga, "\n")
cat(salida_resumen_ratios, "\n")

cat("\nFiguras generadas:\n")
cat(salida_fig_Qp, "\n")
cat(salida_fig_ratio, "\n")
if (file.exists(salida_fig_tiempo_pico)) cat(salida_fig_tiempo_pico, "\n")
if (file.exists(salida_fig_volumen)) cat(salida_fig_volumen, "\n")

cat("\nComparacion principal:\n")
print(
  comparacion %>%
    select(
      T_anios,
      Qp_racional_m3_s,
      Qp_SCS_m3_s,
      Qp_Clark_m3_s,
      ratio_SCS_racional,
      ratio_Clark_racional,
      ratio_Clark_SCS,
      tiempo_pico_SCS_h,
      tiempo_pico_Clark_h
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4)))
)

cat("\nResumen de ratios:\n")
print(resumen_ratios %>% mutate(across(where(is.numeric), ~ round(.x, 4))))

cat("====================================================\n")

# Mostrar figuras principales en la ventana grafica
print(g_Q)
print(g_ratio)
