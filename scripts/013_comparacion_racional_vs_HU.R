############################################################
#
# 013_comparacion_racional_vs_HU.R
#
# Objetivo:
# - Comparar los caudales punta obtenidos mediante:
#     1) Método racional / escenarios del bloque 007-008
#     2) Hidrograma unitario SCS del bloque 009-012
# - Usar los nombres reales generados por los scripts v2 anteriores
# - Exportar una tabla comparativa y figuras de diagnóstico
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

if (file.exists("scripts/000_rutas_y_capas.R")) {
  source("scripts/000_rutas_y_capas.R")
}

dir.create("salidas/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/figuras", recursive = TRUE, showWarnings = FALSE)


# 2. RUTAS ====

# Tabla del método racional.
# Preferentemente se usa el resultado del script 008, porque contiene escenarios.
# Si no existe, se usa la tabla normativa base del script 007.
archivo_racional_008 <- "salidas/tablas/008_escenarios_Q.csv"
archivo_racional_007 <- "salidas/tablas/007_caudales_metodo_racional.csv"

# Tabla resumen del hidrograma unitario SCS, generada por el script 012.
archivo_HU_012 <- "salidas/tablas/012_hidrograma_final_resumen.csv"

# Salidas del script 013.
salida_comparacion <- "salidas/tablas/013_comparacion_racional_vs_HU.csv"
salida_resumen     <- "salidas/tablas/013_resumen_comparacion_racional_vs_HU.csv"

salida_fig_Qp      <- "salidas/figuras/013_Qp_racional_vs_HU.png"
salida_fig_ratio   <- "salidas/figuras/013_ratio_HU_vs_racional.png"
salida_fig_dif     <- "salidas/figuras/013_diferencia_Qp_HU_menos_racional.png"
salida_fig_lluvia  <- "salidas/figuras/013_lluvia_escorrentia_Qp_HU.png"


# 3. PARAMETROS EDITABLES ====

# Si el archivo 008 contiene varios escenarios, aquí se selecciona el escenario
# con el que se desea comparar el hidrograma unitario.
# Si se deja NULL, el script intentará usar un escenario llamado "Base".
# Si no existe, usará el primer escenario disponible.
escenario_racional_objetivo <- NULL
# Ejemplo:
# escenario_racional_objetivo <- "Base"

# Escala del eje X para los periodos de retorno.
usar_eje_x_log10 <- TRUE

# Mostrar mensajes detallados de diagnóstico.
mostrar_diagnostico <- TRUE


# 4. FUNCIONES AUXILIARES ====

normalizar_nombres <- function(x) {
  x %>%
    str_replace_all("á", "a") %>%
    str_replace_all("é", "e") %>%
    str_replace_all("í", "i") %>%
    str_replace_all("ó", "o") %>%
    str_replace_all("ú", "u") %>%
    str_replace_all("Á", "A") %>%
    str_replace_all("É", "E") %>%
    str_replace_all("Í", "I") %>%
    str_replace_all("Ó", "O") %>%
    str_replace_all("Ú", "U")
}

buscar_columna <- function(datos, candidatas, etiqueta = "") {
  nombres <- names(datos)
  nombres_norm <- normalizar_nombres(nombres)
  candidatas_norm <- normalizar_nombres(candidatas)

  # Coincidencia exacta.
  pos <- match(candidatas_norm, nombres_norm)
  pos <- pos[!is.na(pos)]
  if (length(pos) > 0) return(nombres[pos[1]])

  # Coincidencia parcial.
  for (cand in candidatas_norm) {
    pos2 <- which(str_detect(nombres_norm, fixed(cand, ignore_case = TRUE)))
    if (length(pos2) > 0) return(nombres[pos2[1]])
  }

  stop(
    "No se encontro una columna obligatoria para ", etiqueta, ".\n",
    "Candidatas: ", paste(candidatas, collapse = ", "), "\n",
    "Columnas disponibles: ", paste(names(datos), collapse = ", ")
  )
}

buscar_columna_opcional <- function(datos, candidatas) {
  tryCatch(
    buscar_columna(datos, candidatas, etiqueta = "columna opcional"),
    error = function(e) NA_character_
  )
}

leer_racional <- function() {
  if (file.exists(archivo_racional_008)) {
    archivo <- archivo_racional_008
  } else if (file.exists(archivo_racional_007)) {
    archivo <- archivo_racional_007
  } else {
    stop(
      "No se encuentra ninguna tabla del método racional.\n",
      "Buscadas:\n",
      archivo_racional_008, "\n",
      archivo_racional_007
    )
  }

  datos <- read_csv(archivo, show_col_types = FALSE)

  if (mostrar_diagnostico) {
    cat("\nArchivo racional usado:\n", archivo, "\n", sep = "")
    cat("Columnas racional disponibles:\n")
    cat(paste(names(datos), collapse = ", "), "\n")
  }

  datos
}


# 5. COMPROBAR Y LEER ARCHIVOS ====

if (!file.exists(archivo_HU_012)) {
  stop("No se encuentra la tabla del script 012: ", archivo_HU_012)
}

racional_raw <- leer_racional()
HU_raw <- read_csv(archivo_HU_012, show_col_types = FALSE)

if (mostrar_diagnostico) {
  cat("\nArchivo HU usado:\n", archivo_HU_012, "\n", sep = "")
  cat("Columnas HU disponibles:\n")
  cat(paste(names(HU_raw), collapse = ", "), "\n")
}


# 6. IDENTIFICAR COLUMNAS DEL HIDROGRAMA UNITARIO ====

col_T_HU <- buscar_columna(
  HU_raw,
  c("T_anios", "T", "Periodo_retorno", "Periodo_retorno_anios"),
  etiqueta = "periodo de retorno HU"
)

col_Q_HU <- buscar_columna(
  HU_raw,
  c(
    "Qp_total_m3_s",
    "Qp_directo_m3_s",
    "Qp_m3_s",
    "Qmax_m3_s",
    "Q_pico_m3_s",
    "Qp_HU_m3_s",
    "caudal_punta_m3_s"
  ),
  etiqueta = "Qp HU"
)

col_P_HU <- buscar_columna_opcional(
  HU_raw,
  c("P_total_mm", "P_mm", "precipitacion_total_mm", "lluvia_total_mm")
)

col_Pe_HU <- buscar_columna_opcional(
  HU_raw,
  c("Pe_total_mm", "Pef_total_mm", "lluvia_efectiva_total_mm", "P_efectiva_mm")
)

col_C_HU <- buscar_columna_opcional(
  HU_raw,
  c("coef_escorrentia_evento", "C_evento", "C", "coef_escorrentia")
)

col_tp_HU <- buscar_columna_opcional(
  HU_raw,
  c("tiempo_pico_h", "t_pico_h", "Tp_h", "tiempo_Qp_h")
)

col_vol_HU <- buscar_columna_opcional(
  HU_raw,
  c("volumen_total_m3", "volumen_directo_m3", "volumen_objetivo_m3", "volumen_m3")
)

HU <- HU_raw %>%
  transmute(
    T_anios = as.numeric(.data[[col_T_HU]]),
    Qp_HU_m3_s = as.numeric(.data[[col_Q_HU]]),
    P_total_HU_mm = if (!is.na(col_P_HU)) as.numeric(.data[[col_P_HU]]) else NA_real_,
    Pe_total_HU_mm = if (!is.na(col_Pe_HU)) as.numeric(.data[[col_Pe_HU]]) else NA_real_,
    C_evento_HU = if (!is.na(col_C_HU)) as.numeric(.data[[col_C_HU]]) else NA_real_,
    tiempo_pico_HU_h = if (!is.na(col_tp_HU)) as.numeric(.data[[col_tp_HU]]) else NA_real_,
    volumen_HU_m3 = if (!is.na(col_vol_HU)) as.numeric(.data[[col_vol_HU]]) else NA_real_
  ) %>%
  filter(is.finite(T_anios), is.finite(Qp_HU_m3_s))


# 7. IDENTIFICAR COLUMNAS DEL METODO RACIONAL ====

col_T_rat <- buscar_columna(
  racional_raw,
  c("T_anios", "T", "Periodo_retorno", "Periodo_retorno_anios"),
  etiqueta = "periodo de retorno racional"
)

col_Q_rat <- buscar_columna(
  racional_raw,
  c(
    "Q_m3_s",
    "Qp_m3_s",
    "Qmax_m3_s",
    "Q_racional_m3_s",
    "Q_base_m3_s",
    "Q_escenario_m3_s",
    "Q_final_m3_s",
    "caudal_m3_s",
    "caudal_punta_m3_s"
  ),
  etiqueta = "Q racional"
)

col_esc_rat <- buscar_columna_opcional(
  racional_raw,
  c("Escenario", "escenario", "nombre_escenario", "Scenario", "scenario")
)

col_C_rat <- buscar_columna_opcional(
  racional_raw,
  c("C", "coef_escorrentia", "C_escorrentia", "C_final", "C_racional")
)

col_I_rat <- buscar_columna_opcional(
  racional_raw,
  c("I_mm_h", "I_tc_mm_h", "intensidad_mm_h", "Intensidad_mm_h")
)

col_P_rat <- buscar_columna_opcional(
  racional_raw,
  c("P_mm", "Pd_mm", "P_total_mm", "precipitacion_mm", "lluvia_mm")
)

col_tc_rat <- buscar_columna_opcional(
  racional_raw,
  c("tc_h", "Tc_h", "tiempo_concentracion_h", "T_c_h")
)

racional_pre <- racional_raw %>%
  mutate(
    T_anios = as.numeric(.data[[col_T_rat]]),
    Qp_racional_m3_s = as.numeric(.data[[col_Q_rat]]),
    Escenario_racional = if (!is.na(col_esc_rat)) as.character(.data[[col_esc_rat]]) else "Unico",
    C_racional = if (!is.na(col_C_rat)) as.numeric(.data[[col_C_rat]]) else NA_real_,
    I_racional_mm_h = if (!is.na(col_I_rat)) as.numeric(.data[[col_I_rat]]) else NA_real_,
    P_racional_mm = if (!is.na(col_P_rat)) as.numeric(.data[[col_P_rat]]) else NA_real_,
    tc_racional_h = if (!is.na(col_tc_rat)) as.numeric(.data[[col_tc_rat]]) else NA_real_
  ) %>%
  filter(is.finite(T_anios), is.finite(Qp_racional_m3_s))

# Selección de escenario racional.
escenarios_disponibles <- unique(racional_pre$Escenario_racional)

if (length(escenarios_disponibles) > 1) {
  if (is.null(escenario_racional_objetivo)) {
    if ("Base" %in% escenarios_disponibles) {
      escenario_usado <- "Base"
    } else if ("base" %in% escenarios_disponibles) {
      escenario_usado <- "base"
    } else {
      escenario_usado <- escenarios_disponibles[1]
    }
  } else {
    escenario_usado <- escenario_racional_objetivo

    if (!escenario_usado %in% escenarios_disponibles) {
      stop(
        "El escenario solicitado no existe en la tabla racional: ", escenario_usado, "\n",
        "Escenarios disponibles: ", paste(escenarios_disponibles, collapse = ", ")
      )
    }
  }

  racional <- racional_pre %>%
    filter(Escenario_racional == escenario_usado)

} else {
  escenario_usado <- escenarios_disponibles[1]
  racional <- racional_pre
}

if (mostrar_diagnostico) {
  cat("\nColumnas identificadas:\n")
  cat("HU: T =", col_T_HU, "; Qp =", col_Q_HU, "\n")
  cat("Racional: T =", col_T_rat, "; Qp =", col_Q_rat, "\n")
  cat("Escenario racional usado:", escenario_usado, "\n")
}


# 8. COMPARACION ====

comparacion <- HU %>%
  inner_join(racional, by = "T_anios") %>%
  mutate(
    escenario_racional_usado = escenario_usado,
    diferencia_Qp_HU_menos_racional_m3_s = Qp_HU_m3_s - Qp_racional_m3_s,
    ratio_HU_racional = Qp_HU_m3_s / Qp_racional_m3_s,
    incremento_HU_respecto_racional_pct = 100 * (Qp_HU_m3_s - Qp_racional_m3_s) / Qp_racional_m3_s,
    diferencia_C_HU_menos_racional = C_evento_HU - C_racional
  ) %>%
  arrange(T_anios)

if (nrow(comparacion) == 0) {
  stop(
    "No hay periodos de retorno comunes entre racional y HU.\n",
    "T racional: ", paste(sort(unique(racional$T_anios)), collapse = ", "), "\n",
    "T HU: ", paste(sort(unique(HU$T_anios)), collapse = ", ")
  )
}

resumen <- comparacion %>%
  summarise(
    escenario_racional_usado = first(escenario_racional_usado),
    n_periodos = n(),
    Qp_racional_min_m3_s = min(Qp_racional_m3_s, na.rm = TRUE),
    Qp_racional_max_m3_s = max(Qp_racional_m3_s, na.rm = TRUE),
    Qp_HU_min_m3_s = min(Qp_HU_m3_s, na.rm = TRUE),
    Qp_HU_max_m3_s = max(Qp_HU_m3_s, na.rm = TRUE),
    ratio_HU_racional_min = min(ratio_HU_racional, na.rm = TRUE),
    ratio_HU_racional_max = max(ratio_HU_racional, na.rm = TRUE),
    ratio_HU_racional_medio = mean(ratio_HU_racional, na.rm = TRUE),
    incremento_pct_medio = mean(incremento_HU_respecto_racional_pct, na.rm = TRUE)
  )


# 9. EXPORTAR TABLAS ====

write_csv(comparacion, salida_comparacion)
write_csv(resumen, salida_resumen)


# 10. FIGURAS ====

comparacion_larga <- comparacion %>%
  select(T_anios, Qp_racional_m3_s, Qp_HU_m3_s) %>%
  pivot_longer(
    cols = c(Qp_racional_m3_s, Qp_HU_m3_s),
    names_to = "Metodo",
    values_to = "Qp_m3_s"
  ) %>%
  mutate(
    Metodo = recode(
      Metodo,
      Qp_racional_m3_s = "Método racional",
      Qp_HU_m3_s = "HU-SCS"
    )
  )

g_Qp <- ggplot(comparacion_larga, aes(x = T_anios, y = Qp_m3_s, color = Metodo)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  labs(
    title = "Comparación de caudal punta",
    subtitle = paste0("Método racional frente a HU-SCS | Escenario racional: ", escenario_usado),
    x = "Periodo de retorno, T (años)",
    y = expression(Q[p]~(m^3/s)),
    color = "Método"
  ) +
  theme_minimal()

if (usar_eje_x_log10) {
  g_Qp <- g_Qp + scale_x_log10(breaks = sort(unique(comparacion$T_anios)))
}

ggsave(salida_fig_Qp, g_Qp, width = 8, height = 5, dpi = 300)


g_ratio <- ggplot(comparacion, aes(x = T_anios, y = ratio_HU_racional)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  labs(
    title = "Relación entre caudal punta HU-SCS y método racional",
    subtitle = "Valores > 1 indican Qp mayor con HU-SCS",
    x = "Periodo de retorno, T (años)",
    y = expression(Q[p,HU] / Q[p,racional])
  ) +
  theme_minimal()

if (usar_eje_x_log10) {
  g_ratio <- g_ratio + scale_x_log10(breaks = sort(unique(comparacion$T_anios)))
}

ggsave(salida_fig_ratio, g_ratio, width = 8, height = 5, dpi = 300)


g_dif <- ggplot(comparacion, aes(x = T_anios, y = diferencia_Qp_HU_menos_racional_m3_s)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_col() +
  labs(
    title = "Diferencia de caudal punta",
    subtitle = expression(Q[p,HU] - Q[p,racional]),
    x = "Periodo de retorno, T (años)",
    y = expression(Delta~Q[p]~(m^3/s))
  ) +
  theme_minimal()

if (usar_eje_x_log10) {
  g_dif <- g_dif + scale_x_log10(breaks = sort(unique(comparacion$T_anios)))
}

ggsave(salida_fig_dif, g_dif, width = 8, height = 5, dpi = 300)


g_lluvia <- comparacion %>%
  select(T_anios, P_total_HU_mm, Pe_total_HU_mm, Qp_HU_m3_s) %>%
  pivot_longer(
    cols = c(P_total_HU_mm, Pe_total_HU_mm, Qp_HU_m3_s),
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  mutate(
    Variable = recode(
      Variable,
      P_total_HU_mm = "Lluvia total HU (mm)",
      Pe_total_HU_mm = "Lluvia efectiva HU (mm)",
      Qp_HU_m3_s = "Qp HU-SCS (m³/s)"
    )
  ) %>%
  ggplot(aes(x = T_anios, y = Valor)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(
    title = "Variables principales del bloque HU-SCS",
    x = "Periodo de retorno, T (años)",
    y = NULL
  ) +
  theme_minimal()

if (usar_eje_x_log10) {
  g_lluvia <- g_lluvia + scale_x_log10(breaks = sort(unique(comparacion$T_anios)))
}

ggsave(salida_fig_lluvia, g_lluvia, width = 9, height = 6, dpi = 300)


# 11. RESUMEN EN CONSOLA ====

cat("\n====================================================\n")
cat("SCRIPT 013 FINALIZADO\n")
cat("====================================================\n")

cat("\nArchivo racional usado:\n")
if (file.exists(archivo_racional_008)) {
  cat(archivo_racional_008, "\n")
} else {
  cat(archivo_racional_007, "\n")
}

cat("\nArchivo HU usado:\n")
cat(archivo_HU_012, "\n")

cat("\nEscenario racional comparado:\n")
cat(escenario_usado, "\n")

cat("\nColumnas principales usadas:\n")
cat("Racional: T =", col_T_rat, "; Qp =", col_Q_rat, "\n")
cat("HU:       T =", col_T_HU, "; Qp =", col_Q_HU, "\n")

cat("\nTabla comparativa generada:\n")
cat(salida_comparacion, "\n")
cat(salida_resumen, "\n")

cat("\nFiguras generadas:\n")
cat(salida_fig_Qp, "\n")
cat(salida_fig_ratio, "\n")
cat(salida_fig_dif, "\n")
cat(salida_fig_lluvia, "\n")

cat("\nComparación principal:\n")
print(
  comparacion %>%
    select(
      T_anios,
      Qp_racional_m3_s,
      Qp_HU_m3_s,
      ratio_HU_racional,
      incremento_HU_respecto_racional_pct,
      P_total_HU_mm,
      Pe_total_HU_mm,
      C_evento_HU,
      tiempo_pico_HU_h
    ) %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
)

cat("====================================================\n")
