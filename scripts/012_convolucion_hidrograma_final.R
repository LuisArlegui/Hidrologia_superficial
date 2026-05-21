############################################################
#
# 012_convolucion_hidrograma_final.R
#
# Objetivo:
# - Leer la lluvia efectiva incremental generada por el script 010
# - Leer el hidrograma unitario SCS generado por el script 011
# - Aplicar la convolución discreta Pe(t) * HU(t)
# - Obtener el hidrograma final de avenida Q(t) para cada periodo de retorno
# - Calcular caudal punta, tiempo al pico, volumen y comprobación de balance
# - Exportar tablas y figuras para análisis y comparación posterior
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

archivo_lluvia_efectiva_010 <- "salidas/tablas/010_lluvia_efectiva_SCS_CN_largo.csv"
archivo_resumen_010 <- "salidas/tablas/010_lluvia_efectiva_SCS_CN_resumen.csv"
archivo_HU_011 <- "salidas/tablas/011_HU_SCS_largo.csv"
archivo_HU_resumen_011 <- "salidas/tablas/011_HU_SCS_resumen.csv"

salida_hidrograma_final <- "salidas/tablas/012_hidrograma_final_largo.csv"
salida_resumen_final <- "salidas/tablas/012_hidrograma_final_resumen.csv"
salida_componentes_convolucion <- "salidas/tablas/012_componentes_convolucion.csv"

salida_figura_hidrogramas <- "salidas/figuras/012_hidrogramas_finales.png"
salida_figura_hietograma_hidrograma <- "salidas/figuras/012_hietograma_efectivo_hidrograma.png"


# 3. PARAMETROS EDITABLES ====

# Periodos de retorno que se desean procesar.
# Si se deja NULL, se usan todos los T_anios disponibles.
T_seleccionados <- NULL
# Ejemplo:
# T_seleccionados <- c(10, 25, 100, 500)

# Caudal base añadido al hidrograma directo.
# Para avenidas de diseño suele dejarse en 0.
Q_base_m3_s <- 0

# Exportar o no las contribuciones individuales de cada bloque de lluvia efectiva.
# Puede generar tablas grandes si hay muchos periodos o paso temporal muy fino.
exportar_componentes <- TRUE

# Tolerancia para comparar pasos temporales.
tolerancia_tiempo_h <- 1e-8

# Redondeo temporal para evitar problemas de unión por pequeñas diferencias numéricas.
digitos_tiempo <- 8


# 4. COMPROBAR ARCHIVOS DE ENTRADA ====

archivos_necesarios <- c(
  archivo_lluvia_efectiva_010,
  archivo_resumen_010,
  archivo_HU_011,
  archivo_HU_resumen_011
)

faltan_archivos <- archivos_necesarios[!file.exists(archivos_necesarios)]

if (length(faltan_archivos) > 0) {
  stop(
    "No se encuentran los siguientes archivos de entrada:\n",
    paste(faltan_archivos, collapse = "\n")
  )
}


# 5. LEER DATOS ====

lluvia_efectiva <- read_csv(archivo_lluvia_efectiva_010, show_col_types = FALSE)
resumen_010 <- read_csv(archivo_resumen_010, show_col_types = FALSE)
HU <- read_csv(archivo_HU_011, show_col_types = FALSE)
HU_resumen <- read_csv(archivo_HU_resumen_011, show_col_types = FALSE)

campos_lluvia <- c(
  "T_anios", "bloque", "tiempo_inicio_h", "tiempo_fin_h",
  "tiempo_centro_h", "dt_h", "dt_min", "P_incremental_mm",
  "Pe_incremental_mm", "intensidad_efectiva_mm_h"
)

faltan_lluvia <- setdiff(campos_lluvia, names(lluvia_efectiva))

if (length(faltan_lluvia) > 0) {
  stop(
    "Faltan campos necesarios en el archivo 010: ",
    paste(faltan_lluvia, collapse = ", "),
    "\nCampos disponibles: ", paste(names(lluvia_efectiva), collapse = ", ")
  )
}

campos_HU <- c("T_anios", "tiempo_h", "q_unitario_m3_s_por_mm")
faltan_HU <- setdiff(campos_HU, names(HU))

if (length(faltan_HU) > 0) {
  stop(
    "Faltan campos necesarios en el archivo 011: ",
    paste(faltan_HU, collapse = ", "),
    "\nCampos disponibles: ", paste(names(HU), collapse = ", ")
  )
}

lluvia_efectiva <- lluvia_efectiva %>%
  mutate(
    T_anios = as.numeric(T_anios),
    bloque = as.integer(bloque),
    tiempo_inicio_h = as.numeric(tiempo_inicio_h),
    tiempo_fin_h = as.numeric(tiempo_fin_h),
    tiempo_centro_h = as.numeric(tiempo_centro_h),
    dt_h = as.numeric(dt_h),
    dt_min = as.numeric(dt_min),
    P_incremental_mm = as.numeric(P_incremental_mm),
    Pe_incremental_mm = as.numeric(Pe_incremental_mm),
    intensidad_efectiva_mm_h = as.numeric(intensidad_efectiva_mm_h)
  )

HU <- HU %>%
  mutate(
    T_anios = as.numeric(T_anios),
    tiempo_h = as.numeric(tiempo_h),
    q_unitario_m3_s_por_mm = as.numeric(q_unitario_m3_s_por_mm)
  )

if (any(!is.finite(lluvia_efectiva$T_anios))) stop("T_anios contiene valores no validos en 010.")
if (any(!is.finite(lluvia_efectiva$dt_h) | lluvia_efectiva$dt_h <= 0)) stop("dt_h no es valido en 010.")
if (any(!is.finite(lluvia_efectiva$Pe_incremental_mm) | lluvia_efectiva$Pe_incremental_mm < 0)) {
  stop("Pe_incremental_mm contiene valores no validos en 010.")
}
if (any(!is.finite(HU$tiempo_h) | HU$tiempo_h < 0)) stop("tiempo_h no es valido en 011.")
if (any(!is.finite(HU$q_unitario_m3_s_por_mm) | HU$q_unitario_m3_s_por_mm < 0)) {
  stop("q_unitario_m3_s_por_mm contiene valores no validos en 011.")
}

if (!is.finite(Q_base_m3_s) || Q_base_m3_s < 0) {
  stop("Q_base_m3_s debe ser un numero no negativo.")
}

if (!is.null(T_seleccionados)) {
  lluvia_efectiva <- lluvia_efectiva %>% filter(T_anios %in% T_seleccionados)
  resumen_010 <- resumen_010 %>% filter(T_anios %in% T_seleccionados)
  HU <- HU %>% filter(T_anios %in% T_seleccionados)
  HU_resumen <- HU_resumen %>% filter(T_anios %in% T_seleccionados)

  if (nrow(lluvia_efectiva) == 0) {
    stop("Ninguno de los T_seleccionados existe en el archivo 010.")
  }
}

T_lluvia <- sort(unique(lluvia_efectiva$T_anios))
T_HU <- sort(unique(HU$T_anios))
T_comunes <- intersect(T_lluvia, T_HU)

if (length(T_comunes) == 0) {
  stop("No hay periodos de retorno comunes entre los archivos 010 y 011.")
}

if (length(setdiff(T_lluvia, T_HU)) > 0) {
  warning("Hay T_anios presentes en 010 pero no en 011: ", paste(setdiff(T_lluvia, T_HU), collapse = ", "))
}

if (length(setdiff(T_HU, T_lluvia)) > 0) {
  warning("Hay T_anios presentes en 011 pero no en 010: ", paste(setdiff(T_HU, T_lluvia), collapse = ", "))
}

lluvia_efectiva <- lluvia_efectiva %>% filter(T_anios %in% T_comunes)
HU <- HU %>% filter(T_anios %in% T_comunes)


# 6. FUNCIONES AUXILIARES ====

integrar_trapecios <- function(x, y) {
  if (length(x) < 2) return(0)
  sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2, na.rm = TRUE)
}

obtener_paso_unico <- function(x, nombre = "paso") {
  xu <- unique(round(x, digitos_tiempo))
  xu <- xu[is.finite(xu)]
  if (length(xu) != 1) {
    stop(nombre, " no es unico. Valores encontrados: ", paste(xu, collapse = ", "))
  }
  as.numeric(xu[1])
}

preparar_HU_en_malla <- function(hu_T, dt_h, tmax_HU_h) {
  tiempos_HU <- seq(0, tmax_HU_h, by = dt_h)

  if (tail(tiempos_HU, 1) < tmax_HU_h - tolerancia_tiempo_h) {
    tiempos_HU <- c(tiempos_HU, tmax_HU_h)
  }

  q_HU <- approx(
    x = hu_T$tiempo_h,
    y = hu_T$q_unitario_m3_s_por_mm,
    xout = tiempos_HU,
    rule = 2,
    yleft = 0,
    yright = 0
  )$y

  tibble(
    tiempo_HU_h = tiempos_HU,
    q_unitario_m3_s_por_mm = q_HU
  )
}

convolucion_T <- function(T_i) {
  lluvia_T <- lluvia_efectiva %>%
    filter(T_anios == T_i) %>%
    arrange(bloque)

  hu_T_original <- HU %>%
    filter(T_anios == T_i) %>%
    arrange(tiempo_h)

  if (nrow(lluvia_T) == 0) stop("No hay lluvia efectiva para T = ", T_i)
  if (nrow(hu_T_original) == 0) stop("No hay HU-SCS para T = ", T_i)

  dt_h <- obtener_paso_unico(lluvia_T$dt_h, paste0("dt_h para T = ", T_i))
  dt_min <- obtener_paso_unico(lluvia_T$dt_min, paste0("dt_min para T = ", T_i))

  # El HU de 011 normalmente ya tiene el mismo paso que la lluvia efectiva.
  # Aun asi, se interpola a la malla de la lluvia efectiva para que la convolucion sea estable.
  tmax_HU_h <- max(hu_T_original$tiempo_h, na.rm = TRUE)
  hu_T <- preparar_HU_en_malla(hu_T_original, dt_h, tmax_HU_h)

  n_lluvia <- nrow(lluvia_T)
  n_HU <- nrow(hu_T)
  n_total <- n_lluvia + n_HU - 1

  tiempos_h <- seq(0, by = dt_h, length.out = n_total)
  Q_directo <- rep(0, n_total)

  lista_componentes <- vector("list", n_lluvia)

  for (j in seq_len(n_lluvia)) {
    Pe_j <- lluvia_T$Pe_incremental_mm[j]
    bloque_j <- lluvia_T$bloque[j]
    inicio_j <- lluvia_T$tiempo_inicio_h[j]

    contribucion_j <- Pe_j * hu_T$q_unitario_m3_s_por_mm
    indices <- j:(j + n_HU - 1)
    Q_directo[indices] <- Q_directo[indices] + contribucion_j

    if (exportar_componentes) {
      lista_componentes[[j]] <- tibble(
        T_anios = T_i,
        bloque_lluvia = bloque_j,
        tiempo_inicio_bloque_h = inicio_j,
        Pe_incremental_mm = Pe_j,
        tiempo_relativo_HU_h = hu_T$tiempo_HU_h,
        tiempo_h = round(inicio_j + hu_T$tiempo_HU_h, digitos_tiempo),
        Q_componente_m3_s = contribucion_j
      )
    }
  }

  Q_total <- Q_directo + Q_base_m3_s

  hidrograma <- tibble(
    T_anios = T_i,
    tiempo_h = round(tiempos_h, digitos_tiempo),
    Q_directo_m3_s = Q_directo,
    Q_base_m3_s = Q_base_m3_s,
    Q_total_m3_s = Q_total,
    dt_h = dt_h,
    dt_min = dt_min
  )

  # Añadir la lluvia efectiva sobre la misma malla temporal para facilitar graficos.
  lluvia_malla <- tibble(
    T_anios = T_i,
    tiempo_h = round(lluvia_T$tiempo_inicio_h, digitos_tiempo),
    bloque = lluvia_T$bloque,
    P_incremental_mm = lluvia_T$P_incremental_mm,
    Pe_incremental_mm = lluvia_T$Pe_incremental_mm,
    intensidad_efectiva_mm_h = lluvia_T$intensidad_efectiva_mm_h
  )

  hidrograma <- hidrograma %>%
    left_join(lluvia_malla, by = c("T_anios", "tiempo_h")) %>%
    mutate(
      bloque = ifelse(is.na(bloque), NA_integer_, bloque),
      P_incremental_mm = ifelse(is.na(P_incremental_mm), 0, P_incremental_mm),
      Pe_incremental_mm = ifelse(is.na(Pe_incremental_mm), 0, Pe_incremental_mm),
      intensidad_efectiva_mm_h = ifelse(is.na(intensidad_efectiva_mm_h), 0, intensidad_efectiva_mm_h)
    )

  Pe_total_mm <- sum(lluvia_T$Pe_incremental_mm, na.rm = TRUE)
  P_total_mm <- sum(lluvia_T$P_incremental_mm, na.rm = TRUE)

  A_km2 <- if ("A_km2" %in% names(hu_T_original)) {
    unique(hu_T_original$A_km2)[1]
  } else {
    NA_real_
  }

  volumen_objetivo_m3 <- if (is.finite(A_km2)) Pe_total_mm * 1000 * A_km2 else NA_real_
  volumen_directo_m3 <- integrar_trapecios(hidrograma$tiempo_h, hidrograma$Q_directo_m3_s) * 3600
  volumen_total_m3 <- integrar_trapecios(hidrograma$tiempo_h, hidrograma$Q_total_m3_s) * 3600

  Qp_directo <- max(hidrograma$Q_directo_m3_s, na.rm = TRUE)
  Qp_total <- max(hidrograma$Q_total_m3_s, na.rm = TRUE)
  tiempo_pico_h <- hidrograma$tiempo_h[which.max(hidrograma$Q_total_m3_s)][1]

  resumen <- tibble(
    T_anios = T_i,
    metodo = "HU_SCS",
    A_km2 = A_km2,
    dt_min = dt_min,
    dt_h = dt_h,
    n_bloques_lluvia = n_lluvia,
    duracion_lluvia_h = max(lluvia_T$tiempo_fin_h, na.rm = TRUE),
    duracion_HU_h = max(hu_T$tiempo_HU_h, na.rm = TRUE),
    duracion_hidrograma_h = max(hidrograma$tiempo_h, na.rm = TRUE),
    P_total_mm = P_total_mm,
    Pe_total_mm = Pe_total_mm,
    coef_escorrentia_evento = ifelse(P_total_mm > 0, Pe_total_mm / P_total_mm, 0),
    Qp_directo_m3_s = Qp_directo,
    Q_base_m3_s = Q_base_m3_s,
    Qp_total_m3_s = Qp_total,
    tiempo_pico_h = tiempo_pico_h,
    volumen_objetivo_m3 = volumen_objetivo_m3,
    volumen_directo_m3 = volumen_directo_m3,
    volumen_total_m3 = volumen_total_m3,
    error_volumen_directo_pct = ifelse(
      is.finite(volumen_objetivo_m3) && volumen_objetivo_m3 > 0,
      100 * (volumen_directo_m3 - volumen_objetivo_m3) / volumen_objetivo_m3,
      NA_real_
    )
  )

  componentes <- if (exportar_componentes) bind_rows(lista_componentes) else tibble()

  list(
    hidrograma = hidrograma,
    resumen = resumen,
    componentes = componentes
  )
}


# 7. APLICAR CONVOLUCION ====

lista_resultados <- lapply(T_comunes, convolucion_T)

hidrograma_final <- bind_rows(lapply(lista_resultados, `[[`, "hidrograma"))
resumen_final <- bind_rows(lapply(lista_resultados, `[[`, "resumen"))
componentes_convolucion <- bind_rows(lapply(lista_resultados, `[[`, "componentes"))


# 8. EXPORTAR TABLAS ====

write_csv(hidrograma_final, salida_hidrograma_final)
write_csv(resumen_final, salida_resumen_final)

if (exportar_componentes) {
  write_csv(componentes_convolucion, salida_componentes_convolucion)
}


# 9. FIGURAS ====

orden_T <- sort(unique(hidrograma_final$T_anios))

hidrograma_plot <- hidrograma_final %>%
  mutate(
    T_label = factor(
      paste0("T = ", T_anios, " años"),
      levels = paste0("T = ", orden_T, " años")
    )
  )

g_hid <- ggplot(hidrograma_plot, aes(x = tiempo_h, y = Q_total_m3_s)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ T_label, ncol = 3, scales = "free_y") +
  labs(
    title = "Hidrogramas finales de avenida",
    subtitle = "Convolución discreta de lluvia efectiva SCS-CN e hidrograma unitario SCS",
    x = "Tiempo desde el inicio de la tormenta (h)",
    y = "Q total (m³/s)"
  ) +
  theme_minimal()

ggsave(
  salida_figura_hidrogramas,
  g_hid,
  width = 10,
  height = 7,
  dpi = 300
)

print(g_hid)

# Figura combinada: hietograma efectivo e hidrograma.
# Se escala la intensidad efectiva para superponerla con Q solo como ayuda visual.
combo_plot <- hidrograma_final %>%
  group_by(T_anios) %>%
  mutate(
    Qmax = max(Q_total_m3_s, na.rm = TRUE),
    Imax = max(intensidad_efectiva_mm_h, na.rm = TRUE),
    intensidad_efectiva_escalada = ifelse(Imax > 0, intensidad_efectiva_mm_h / Imax * Qmax, 0),
    T_label = factor(
      paste0("T = ", T_anios, " años"),
      levels = paste0("T = ", orden_T, " años")
    )
  ) %>%
  ungroup()

g_combo <- ggplot(combo_plot, aes(x = tiempo_h)) +
  geom_col(
    aes(y = intensidad_efectiva_escalada),
    width = unique(hidrograma_final$dt_h)[1],
    align = "edge",
    alpha = 0.35,
    color = "grey40"
  ) +
  geom_line(aes(y = Q_total_m3_s), linewidth = 0.8) +
  facet_wrap(~ T_label, ncol = 3, scales = "free_y") +
  labs(
    title = "Hietograma efectivo e hidrograma final",
    subtitle = "Las barras de lluvia efectiva están escaladas para facilitar la comparación visual",
    x = "Tiempo desde el inicio de la tormenta (h)",
    y = "Q (m³/s); lluvia efectiva escalada"
  ) +
  theme_minimal()

ggsave(
  salida_figura_hietograma_hidrograma,
  g_combo,
  width = 10,
  height = 7,
  dpi = 300
)

print(g_combo)


# 10. RESUMEN EN CONSOLA ====

cat("\n====================================================\n")
cat("SCRIPT 012 FINALIZADO\n")
cat("====================================================\n")

cat("\nArchivos de entrada:\n")
cat(archivo_lluvia_efectiva_010, "\n")
cat(archivo_HU_011, "\n")

cat("\nConfiguracion de convolucion:\n")
cat("Q_base_m3_s =", Q_base_m3_s, "\n")
cat("exportar_componentes =", exportar_componentes, "\n")

cat("\nPeriodos de retorno procesados:\n")
cat(paste(sort(unique(hidrograma_final$T_anios)), collapse = ", "), "años\n")

cat("\nTablas generadas:\n")
cat(salida_hidrograma_final, "\n")
cat(salida_resumen_final, "\n")
if (exportar_componentes) cat(salida_componentes_convolucion, "\n")

cat("\nFiguras generadas:\n")
cat(salida_figura_hidrogramas, "\n")
cat(salida_figura_hietograma_hidrograma, "\n")

cat("\nResumen hidrogramas finales:\n")
print(
  resumen_final %>%
    mutate(across(where(is.numeric), ~ round(.x, 4)))
)

cat("====================================================\n")
