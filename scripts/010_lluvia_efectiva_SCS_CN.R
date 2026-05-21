############################################################
#
# 010_lluvia_efectiva_SCS_CN.R
#
# Objetivo:
# - Leer los hietogramas de diseño generados por el script 009
# - Leer los parámetros hidrológicos calculados en el script 005
# - Calcular la lluvia efectiva mediante el método SCS-CN
# - Obtener lluvia acumulada, lluvia efectiva acumulada e incremental
# - Preparar la entrada para el hidrograma unitario SCS
# - Exportar tablas y figuras del hietograma bruto y efectivo
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

archivo_hietogramas_009 <- "salidas/tablas/009_hietogramas_diseno_largo.csv"
archivo_parametros_005 <- "salidas/tablas/005_parametros_hidrologicos_resumen.csv"
archivo_p0_005 <- "salidas/tablas/005_estadisticos_P0.csv"

salida_lluvia_efectiva_largo <- "salidas/tablas/010_lluvia_efectiva_SCS_CN_largo.csv"
salida_lluvia_efectiva_resumen <- "salidas/tablas/010_lluvia_efectiva_SCS_CN_resumen.csv"
salida_figura_hietogramas <- "salidas/figuras/010_hietogramas_bruto_efectivo.png"
salida_figura_acumulada <- "salidas/figuras/010_lluvia_acumulada_bruta_efectiva.png"


# 3. PARAMETROS EDITABLES ====

# Periodos de retorno que se desean procesar.
# Si se deja NULL, se usan todos los T_anios disponibles en el script 009.
T_seleccionados <- NULL
# Ejemplo:
# T_seleccionados <- c(10, 25, 100, 500)

# Modo de obtención de los parámetros SCS-CN.
# Opciones:
# - "CN_medio_005"       -> usa CN_medio del resumen del script 005
# - "CN_manual"          -> usa el valor CN_manual definido más abajo
# - "P0_ponderado_005"   -> usa P0_ponderado_mm del script 005 como abstracción inicial Ia
#                            y deriva S = Ia / lambda_Ia
# - "P0_manual"          -> usa P0_manual_mm como abstracción inicial Ia
#
# Recomendación inicial: "CN_medio_005".
modo_parametros_scs <- "CN_medio_005"

# CN manual, solo si modo_parametros_scs = "CN_manual".
CN_manual <- 75

# P0 manual en mm, solo si modo_parametros_scs = "P0_manual".
# Aquí P0 se interpreta como abstracción inicial Ia.
P0_manual_mm <- 20

# Relación clásica SCS entre abstracción inicial y almacenamiento potencial.
# La formulación clásica usa Ia = 0.2*S.
# Algunos estudios usan valores menores, por ejemplo 0.05.
lambda_Ia <- 0.20

# Condición de humedad antecedente.
# Opciones:
# - "II"  -> CN normal, sin corrección
# - "I"   -> condición seca, reduce CN
# - "III" -> condición húmeda, aumenta CN
#
# Las conversiones usadas son las relaciones habituales NRCS:
# CN_I   = CN_II / (2.281 - 0.01281*CN_II)
# CN_III = CN_II / (0.427 + 0.00573*CN_II)
condicion_humedad <- "II"

# Pequeña tolerancia numérica para evitar incrementos efectivos negativos
# por redondeos.
tolerancia_mm <- 1e-9


# 4. COMPROBAR ARCHIVOS DE ENTRADA ====

if (!file.exists(archivo_hietogramas_009)) {
  stop(
    "No se encuentra la tabla del script 009: ", archivo_hietogramas_009,
    "\nEjecuta antes 009_hietograma_diseno.R."
  )
}

if (modo_parametros_scs %in% c("CN_medio_005", "P0_ponderado_005") &&
    !file.exists(archivo_parametros_005)) {
  stop("No se encuentra la tabla resumen del script 005: ", archivo_parametros_005)
}

if (modo_parametros_scs == "P0_ponderado_005" && !file.exists(archivo_p0_005)) {
  stop("No se encuentra la tabla de P0 del script 005: ", archivo_p0_005)
}


# 5. LEER HIETOGRAMAS ====

hietogramas <- read_csv(archivo_hietogramas_009, show_col_types = FALSE)

campos_hietograma <- c(
  "T_anios", "bloque", "tiempo_inicio_h", "tiempo_fin_h",
  "tiempo_centro_h", "dt_min", "dt_h", "duracion_total_h",
  "P_incremental_mm"
)

faltan_hietograma <- setdiff(campos_hietograma, names(hietogramas))

if (length(faltan_hietograma) > 0) {
  stop(
    "Faltan campos necesarios en el archivo 009: ",
    paste(faltan_hietograma, collapse = ", "),
    "\nCampos disponibles: ", paste(names(hietogramas), collapse = ", ")
  )
}

hietogramas <- hietogramas %>%
  mutate(
    T_anios = as.numeric(T_anios),
    bloque = as.integer(bloque),
    tiempo_inicio_h = as.numeric(tiempo_inicio_h),
    tiempo_fin_h = as.numeric(tiempo_fin_h),
    tiempo_centro_h = as.numeric(tiempo_centro_h),
    dt_min = as.numeric(dt_min),
    dt_h = as.numeric(dt_h),
    duracion_total_h = as.numeric(duracion_total_h),
    P_incremental_mm = as.numeric(P_incremental_mm)
  )

# Compatibilidad: si el 009 no incluye intensidad_bloque_mm_h, se calcula.
if (!("intensidad_bloque_mm_h" %in% names(hietogramas))) {
  hietogramas <- hietogramas %>%
    mutate(intensidad_bloque_mm_h = P_incremental_mm / dt_h)
} else {
  hietogramas <- hietogramas %>%
    mutate(intensidad_bloque_mm_h = as.numeric(intensidad_bloque_mm_h))
}

if (any(!is.finite(hietogramas$T_anios))) stop("T_anios contiene valores no validos.")
if (any(!is.finite(hietogramas$P_incremental_mm) | hietogramas$P_incremental_mm < 0)) {
  stop("P_incremental_mm contiene valores no validos.")
}

if (!is.null(T_seleccionados)) {
  hietogramas <- hietogramas %>%
    filter(T_anios %in% T_seleccionados)

  if (nrow(hietogramas) == 0) {
    stop("Ninguno de los T_seleccionados existe en la tabla del script 009.")
  }
}


# 6. LEER O DEFINIR PARAMETROS SCS-CN ====

corregir_CN_humedad <- function(CN_II, condicion = "II") {
  if (!is.finite(CN_II) || CN_II <= 0 || CN_II >= 100) {
    stop("CN debe estar entre 0 y 100, sin alcanzar 100.")
  }

  if (condicion == "II") return(CN_II)

  if (condicion == "I") {
    CN_I <- CN_II / (2.281 - 0.01281 * CN_II)
    return(CN_I)
  }

  if (condicion == "III") {
    CN_III <- CN_II / (0.427 + 0.00573 * CN_II)
    return(CN_III)
  }

  stop("condicion_humedad no reconocida: ", condicion)
}

obtener_parametros_scs <- function() {
  if (!is.finite(lambda_Ia) || lambda_Ia <= 0 || lambda_Ia >= 1) {
    stop("lambda_Ia debe estar entre 0 y 1.")
  }

  if (modo_parametros_scs == "CN_medio_005") {
    parametros_005 <- read_csv(archivo_parametros_005, show_col_types = FALSE)

    if (!("CN_medio" %in% names(parametros_005))) {
      stop(
        "No existe el campo CN_medio en ", archivo_parametros_005,
        "\nCampos disponibles: ", paste(names(parametros_005), collapse = ", ")
      )
    }

    CN_II <- as.numeric(parametros_005$CN_medio[1])
    CN <- corregir_CN_humedad(CN_II, condicion_humedad)
    S_mm <- 25400 / CN - 254
    Ia_mm <- lambda_Ia * S_mm

    return(tibble(
      modo_parametros_scs = modo_parametros_scs,
      condicion_humedad = condicion_humedad,
      CN_II = CN_II,
      CN_usado = CN,
      S_mm = S_mm,
      Ia_mm = Ia_mm,
      lambda_Ia = lambda_Ia
    ))
  }

  if (modo_parametros_scs == "CN_manual") {
    CN_II <- as.numeric(CN_manual)
    CN <- corregir_CN_humedad(CN_II, condicion_humedad)
    S_mm <- 25400 / CN - 254
    Ia_mm <- lambda_Ia * S_mm

    return(tibble(
      modo_parametros_scs = modo_parametros_scs,
      condicion_humedad = condicion_humedad,
      CN_II = CN_II,
      CN_usado = CN,
      S_mm = S_mm,
      Ia_mm = Ia_mm,
      lambda_Ia = lambda_Ia
    ))
  }

  if (modo_parametros_scs == "P0_ponderado_005") {
    p0_005 <- read_csv(archivo_p0_005, show_col_types = FALSE)

    if (!("P0_ponderado_mm" %in% names(p0_005))) {
      stop(
        "No existe el campo P0_ponderado_mm en ", archivo_p0_005,
        "\nCampos disponibles: ", paste(names(p0_005), collapse = ", ")
      )
    }

    Ia_mm <- as.numeric(p0_005$P0_ponderado_mm[1])
    if (!is.finite(Ia_mm) || Ia_mm < 0) stop("P0_ponderado_mm no es valido.")

    S_mm <- Ia_mm / lambda_Ia
    CN <- 25400 / (S_mm + 254)

    return(tibble(
      modo_parametros_scs = modo_parametros_scs,
      condicion_humedad = "derivado_desde_P0",
      CN_II = NA_real_,
      CN_usado = CN,
      S_mm = S_mm,
      Ia_mm = Ia_mm,
      lambda_Ia = lambda_Ia
    ))
  }

  if (modo_parametros_scs == "P0_manual") {
    Ia_mm <- as.numeric(P0_manual_mm)
    if (!is.finite(Ia_mm) || Ia_mm < 0) stop("P0_manual_mm no es valido.")

    S_mm <- Ia_mm / lambda_Ia
    CN <- 25400 / (S_mm + 254)

    return(tibble(
      modo_parametros_scs = modo_parametros_scs,
      condicion_humedad = "derivado_desde_P0",
      CN_II = NA_real_,
      CN_usado = CN,
      S_mm = S_mm,
      Ia_mm = Ia_mm,
      lambda_Ia = lambda_Ia
    ))
  }

  stop("modo_parametros_scs no reconocido: ", modo_parametros_scs)
}

parametros_scs <- obtener_parametros_scs()

if (any(!is.finite(parametros_scs$S_mm) | parametros_scs$S_mm < 0)) {
  stop("El almacenamiento potencial S calculado no es valido.")
}

if (any(!is.finite(parametros_scs$Ia_mm) | parametros_scs$Ia_mm < 0)) {
  stop("La abstraccion inicial Ia calculada no es valida.")
}


# 7. FUNCION SCS-CN ====

calcular_Pe_acumulada <- function(P_acum_mm, S_mm, Ia_mm) {
  # Metodo SCS-CN:
  # Pe = 0 si P <= Ia
  # Pe = (P - Ia)^2 / (P - Ia + S) si P > Ia
  Pe <- ifelse(
    P_acum_mm <= Ia_mm,
    0,
    (P_acum_mm - Ia_mm)^2 / (P_acum_mm - Ia_mm + S_mm)
  )

  Pe[!is.finite(Pe)] <- NA_real_
  Pe
}


# 8. CALCULAR LLUVIA EFECTIVA ====

S_mm <- parametros_scs$S_mm[1]
Ia_mm <- parametros_scs$Ia_mm[1]
CN_usado <- parametros_scs$CN_usado[1]
CN_II <- parametros_scs$CN_II[1]

lluvia_efectiva <- hietogramas %>%
  arrange(T_anios, bloque) %>%
  group_by(T_anios) %>%
  mutate(
    P_acumulada_mm = cumsum(P_incremental_mm),
    Pe_acumulada_mm = calcular_Pe_acumulada(P_acumulada_mm, S_mm, Ia_mm),
    Pe_incremental_mm = Pe_acumulada_mm - lag(Pe_acumulada_mm, default = 0),
    Pe_incremental_mm = ifelse(abs(Pe_incremental_mm) < tolerancia_mm, 0, Pe_incremental_mm),
    Pe_incremental_mm = ifelse(Pe_incremental_mm < 0, 0, Pe_incremental_mm),
    intensidad_efectiva_mm_h = Pe_incremental_mm / dt_h,
    coef_escorrentia_acumulado = ifelse(
      P_acumulada_mm > 0,
      Pe_acumulada_mm / P_acumulada_mm,
      0
    ),
    CN_II = CN_II,
    CN_usado = CN_usado,
    S_mm = S_mm,
    Ia_mm = Ia_mm,
    lambda_Ia = lambda_Ia,
    condicion_humedad = parametros_scs$condicion_humedad[1],
    modo_parametros_scs = modo_parametros_scs
  ) %>%
  ungroup()

if (any(is.na(lluvia_efectiva$Pe_acumulada_mm))) {
  stop("Se han generado valores NA en la lluvia efectiva acumulada.")
}


# 9. RESUMEN ====

resumen <- lluvia_efectiva %>%
  group_by(T_anios) %>%
  summarise(
    n_bloques = n(),
    dt_min = first(dt_min),
    duracion_total_h = first(duracion_total_h),
    tc_h = if ("tc_h" %in% names(lluvia_efectiva)) first(tc_h) else NA_real_,
    P_total_mm = sum(P_incremental_mm, na.rm = TRUE),
    Pe_total_mm = sum(Pe_incremental_mm, na.rm = TRUE),
    coef_escorrentia_evento = ifelse(P_total_mm > 0, Pe_total_mm / P_total_mm, 0),
    intensidad_bruta_max_mm_h = max(P_incremental_mm / dt_h, na.rm = TRUE),
    intensidad_efectiva_max_mm_h = max(intensidad_efectiva_mm_h, na.rm = TRUE),
    bloque_inicio_escorrentia = suppressWarnings(min(bloque[Pe_incremental_mm > 0], na.rm = TRUE)),
    tiempo_inicio_escorrentia_h = suppressWarnings(min(tiempo_inicio_h[Pe_incremental_mm > 0], na.rm = TRUE)),
    bloque_pico_efectivo = bloque[which.max(intensidad_efectiva_mm_h)][1],
    tiempo_pico_efectivo_h = tiempo_centro_h[which.max(intensidad_efectiva_mm_h)][1],
    CN_II = first(CN_II),
    CN_usado = first(CN_usado),
    S_mm = first(S_mm),
    Ia_mm = first(Ia_mm),
    lambda_Ia = first(lambda_Ia),
    condicion_humedad = first(condicion_humedad),
    modo_parametros_scs = first(modo_parametros_scs),
    .groups = "drop"
  ) %>%
  mutate(
    bloque_inicio_escorrentia = ifelse(is.infinite(bloque_inicio_escorrentia), NA, bloque_inicio_escorrentia),
    tiempo_inicio_escorrentia_h = ifelse(is.infinite(tiempo_inicio_escorrentia_h), NA, tiempo_inicio_escorrentia_h)
  )


# 10. EXPORTAR TABLAS ====

write_csv(lluvia_efectiva, salida_lluvia_efectiva_largo)
write_csv(resumen, salida_lluvia_efectiva_resumen)


# 11. FIGURAS ====

orden_T <- sort(unique(lluvia_efectiva$T_anios))

lluvia_plot <- lluvia_efectiva %>%
  mutate(
    T_label = factor(
      paste0("T = ", T_anios, " años"),
      levels = paste0("T = ", orden_T, " años")
    )
  ) %>%
  select(
    T_anios, T_label, bloque, tiempo_inicio_h, dt_h,
    intensidad_bruta_mm_h = intensidad_bloque_mm_h,
    intensidad_efectiva_mm_h
  ) %>%
  pivot_longer(
    cols = c(intensidad_bruta_mm_h, intensidad_efectiva_mm_h),
    names_to = "tipo",
    values_to = "intensidad_mm_h"
  ) %>%
  mutate(
    tipo = recode(
      tipo,
      intensidad_bruta_mm_h = "Lluvia bruta",
      intensidad_efectiva_mm_h = "Lluvia efectiva"
    )
  )

g_hiet <- ggplot(
  lluvia_plot,
  aes(x = tiempo_inicio_h, y = intensidad_mm_h)
) +
  geom_col(width = unique(lluvia_efectiva$dt_h)[1], align = "edge", color = "grey30") +
  facet_grid(tipo ~ T_label, scales = "free_y") +
  labs(
    title = "Hietogramas de lluvia bruta y lluvia efectiva SCS-CN",
    subtitle = paste0(
      "CN usado = ", round(CN_usado, 2),
      " | S = ", round(S_mm, 2), " mm",
      " | Ia = ", round(Ia_mm, 2), " mm"
    ),
    x = "Tiempo desde el inicio de la tormenta (h)",
    y = "Intensidad del bloque (mm/h)"
  ) +
  theme_minimal()

ggsave(
  salida_figura_hietogramas,
  g_hiet,
  width = 12,
  height = 7,
  dpi = 300
)

print(g_hiet)

acum_plot <- lluvia_efectiva %>%
  mutate(
    T_label = factor(
      paste0("T = ", T_anios, " años"),
      levels = paste0("T = ", orden_T, " años")
    )
  ) %>%
  select(
    T_anios, T_label, tiempo_fin_h,
    P_acumulada_mm, Pe_acumulada_mm
  ) %>%
  pivot_longer(
    cols = c(P_acumulada_mm, Pe_acumulada_mm),
    names_to = "tipo",
    values_to = "P_mm"
  ) %>%
  mutate(
    tipo = recode(
      tipo,
      P_acumulada_mm = "Lluvia acumulada bruta",
      Pe_acumulada_mm = "Lluvia acumulada efectiva"
    )
  )

g_acum <- ggplot(
  acum_plot,
  aes(x = tiempo_fin_h, y = P_mm, linetype = tipo)
) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ T_label, ncol = 3, scales = "free_y") +
  labs(
    title = "Lluvia acumulada bruta y efectiva",
    subtitle = paste0(
      "Método SCS-CN | modo parámetros: ", modo_parametros_scs,
      " | condición humedad: ", parametros_scs$condicion_humedad[1]
    ),
    x = "Tiempo desde el inicio de la tormenta (h)",
    y = "Precipitación acumulada (mm)",
    linetype = "Serie"
  ) +
  theme_minimal()

ggsave(
  salida_figura_acumulada,
  g_acum,
  width = 10,
  height = 7,
  dpi = 300
)

print(g_acum)


# 12. RESUMEN EN CONSOLA ====

cat("\n====================================================\n")
cat("SCRIPT 010 FINALIZADO\n")
cat("====================================================\n")

cat("\nArchivos de entrada:\n")
cat(archivo_hietogramas_009, "\n")
cat(archivo_parametros_005, "\n")
if (modo_parametros_scs == "P0_ponderado_005") cat(archivo_p0_005, "\n")

cat("\nConfiguracion SCS-CN:\n")
cat("modo_parametros_scs =", modo_parametros_scs, "\n")
cat("condicion_humedad =", condicion_humedad, "\n")
cat("lambda_Ia =", lambda_Ia, "\n")
cat("CN_II =", round(CN_II, 3), "\n")
cat("CN_usado =", round(CN_usado, 3), "\n")
cat("S =", round(S_mm, 3), "mm\n")
cat("Ia =", round(Ia_mm, 3), "mm\n")

cat("\nPeriodos de retorno procesados:\n")
cat(paste(sort(unique(lluvia_efectiva$T_anios)), collapse = ", "), "años\n")

cat("\nTablas generadas:\n")
cat(salida_lluvia_efectiva_largo, "\n")
cat(salida_lluvia_efectiva_resumen, "\n")

cat("\nFiguras generadas:\n")
cat(salida_figura_hietogramas, "\n")
cat(salida_figura_acumulada, "\n")

cat("\nResumen:\n")
print(
  resumen %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
)

cat("====================================================\n")
