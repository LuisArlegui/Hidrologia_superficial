############################################################
#
# 016_convolucion_Clark.R
#
# Objetivo:
# - Leer la lluvia efectiva incremental generada por el script 010
# - Leer el hidrograma unitario de Clark generado por el script 015
# - Aplicar la convolución discreta Pe(t) * HU_Clark(t)
# - Obtener el hidrograma final de avenida según Clark para cada periodo de retorno
# - Calcular caudal punta, tiempo al pico, volumen y comprobación de balance
# - Exportar tablas y figuras para comparación posterior con racional y HU-SCS
#
############################################################


# 0. PAQUETES ====

paquetes <- c("dplyr", "readr", "ggplot2", "tidyr", "stringr")

instalar <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(instalar) > 0) install.packages(instalar)

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(stringr)


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
archivo_HU_Clark_015 <- "salidas/tablas/015_HU_Clark_largo.csv"
archivo_HU_Clark_base_015 <- "salidas/tablas/015_HU_Clark_base.csv"
archivo_HU_Clark_resumen_015 <- "salidas/tablas/015_HU_Clark_resumen.csv"

salida_hidrograma_Clark <- "salidas/tablas/016_hidrograma_Clark_largo.csv"
salida_resumen_Clark <- "salidas/tablas/016_hidrograma_Clark_resumen.csv"
salida_componentes_Clark <- "salidas/tablas/016_componentes_convolucion_Clark.csv"
salida_figura_hidrogramas <- "salidas/figuras/016_hidrogramas_Clark.png"
salida_figura_hietograma_hidrograma <- "salidas/figuras/016_hietograma_efectivo_hidrograma_Clark.png"


# 3. PARAMETROS EDITABLES ====

# Periodos de retorno que se desean procesar.
# Si se deja NULL, se usan todos los T_anios disponibles en el script 010.
T_seleccionados <- NULL
# Ejemplo:
# T_seleccionados <- c(10, 25, 100, 500)

# Orden visual de los periodos de retorno en los facet_wrap.
# Se organiza por columnas de magnitud creciente:
# T=2,5,10 | T=25,50,100 | T=200,500
orden_T_plot <- c(2, 25, 200,
                  5, 50, 500,
                  10, 100)

# Caudal base añadido al hidrograma directo.
# Para avenidas de diseño suele dejarse en 0.
Q_base_m3_s <- 0

# Exportar contribuciones individuales de cada bloque de lluvia efectiva.
exportar_componentes <- TRUE

# Tolerancias y redondeos temporales.
tolerancia_tiempo_h <- 1e-8
digitos_tiempo <- 8


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
  NA_character_
}

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

valor_numerico_primero <- function(tabla, candidatas, defecto = NA_real_) {
  cols <- candidatas[candidatas %in% names(tabla)]
  if (length(cols) == 0) return(defecto)
  val <- suppressWarnings(as.numeric(tabla[[cols[1]]][1]))
  if (!is.finite(val)) return(defecto)
  val
}

crear_T_label <- function(T_anios, orden_T = orden_T_plot) {
  niveles_preferidos <- paste0("T = ", orden_T, " años")
  etiquetas <- paste0("T = ", T_anios, " años")

  # Si aparecen periodos no contemplados en orden_T_plot, se añaden al final en orden numérico.
  T_extra <- sort(setdiff(unique(T_anios), orden_T))
  niveles_extra <- paste0("T = ", T_extra, " años")

  factor(etiquetas, levels = c(niveles_preferidos, niveles_extra))
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

  tibble::tibble(
    tiempo_HU_h = tiempos_HU,
    q_unitario_m3_s_por_mm = q_HU
  )
}


# 5. COMPROBAR Y LEER DATOS ====

archivo_HU_Clark <- primer_archivo_existente(
  c(archivo_HU_Clark_015, archivo_HU_Clark_base_015),
  obligatorio = TRUE,
  etiqueta = "HU de Clark del script 015"
)

archivos_necesarios <- c(
  archivo_lluvia_efectiva_010,
  archivo_resumen_010,
  archivo_HU_Clark,
  archivo_HU_Clark_resumen_015
)

faltan_archivos <- archivos_necesarios[!file.exists(archivos_necesarios)]

if (length(faltan_archivos) > 0) {
  stop(
    "No se encuentran los siguientes archivos de entrada:\n",
    paste(faltan_archivos, collapse = "\n")
  )
}

lluvia_efectiva <- read_csv(archivo_lluvia_efectiva_010, show_col_types = FALSE)
resumen_010 <- read_csv(archivo_resumen_010, show_col_types = FALSE)
HU_Clark <- read_csv(archivo_HU_Clark, show_col_types = FALSE)
HU_Clark_resumen <- read_csv(archivo_HU_Clark_resumen_015, show_col_types = FALSE)

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

campos_HU <- c("tiempo_h", "q_unitario_m3_s_por_mm")
faltan_HU <- setdiff(campos_HU, names(HU_Clark))

if (length(faltan_HU) > 0) {
  stop(
    "Faltan campos necesarios en el HU de Clark 015: ",
    paste(faltan_HU, collapse = ", "),
    "\nCampos disponibles: ", paste(names(HU_Clark), collapse = ", ")
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

HU_Clark <- HU_Clark %>%
  mutate(
    tiempo_h = as.numeric(tiempo_h),
    q_unitario_m3_s_por_mm = as.numeric(q_unitario_m3_s_por_mm)
  )

# Si el archivo 015_HU_Clark_largo.csv trae T_anios, se usa.
# Si no, el HU base se replica para todos los T del 010.
if ("T_anios" %in% names(HU_Clark)) {
  HU_Clark <- HU_Clark %>% mutate(T_anios = as.numeric(T_anios))
} else {
  HU_Clark <- HU_Clark %>% mutate(T_anios = NA_real_)
}

if (any(!is.finite(lluvia_efectiva$T_anios))) stop("T_anios contiene valores no validos en 010.")
if (any(!is.finite(lluvia_efectiva$dt_h) | lluvia_efectiva$dt_h <= 0)) stop("dt_h no es valido en 010.")
if (any(!is.finite(lluvia_efectiva$Pe_incremental_mm) | lluvia_efectiva$Pe_incremental_mm < 0)) {
  stop("Pe_incremental_mm contiene valores no validos en 010.")
}
if (any(!is.finite(HU_Clark$tiempo_h) | HU_Clark$tiempo_h < 0)) stop("tiempo_h no es valido en 015.")
if (any(!is.finite(HU_Clark$q_unitario_m3_s_por_mm) | HU_Clark$q_unitario_m3_s_por_mm < 0)) {
  stop("q_unitario_m3_s_por_mm contiene valores no validos en 015.")
}

if (!is.finite(Q_base_m3_s) || Q_base_m3_s < 0) {
  stop("Q_base_m3_s debe ser un numero no negativo.")
}

if (!is.null(T_seleccionados)) {
  lluvia_efectiva <- lluvia_efectiva %>% filter(T_anios %in% T_seleccionados)
  resumen_010 <- resumen_010 %>% filter(T_anios %in% T_seleccionados)
  if (any(is.finite(HU_Clark$T_anios))) {
    HU_Clark <- HU_Clark %>% filter(T_anios %in% T_seleccionados | is.na(T_anios))
  }
  if (nrow(lluvia_efectiva) == 0) {
    stop("Ninguno de los T_seleccionados existe en el archivo 010.")
  }
}

T_lluvia <- sort(unique(lluvia_efectiva$T_anios))
T_HU_validos <- sort(unique(HU_Clark$T_anios[is.finite(HU_Clark$T_anios)]))

if (length(T_HU_validos) == 0) {
  # HU base: se replica para todos los T.
  T_comunes <- T_lluvia
} else {
  T_comunes <- intersect(T_lluvia, T_HU_validos)
  if (length(T_comunes) == 0) {
    stop("No hay periodos de retorno comunes entre 010 y 015.")
  }
  if (length(setdiff(T_lluvia, T_HU_validos)) > 0) {
    warning("Hay T_anios presentes en 010 pero no en 015: ", paste(setdiff(T_lluvia, T_HU_validos), collapse = ", "))
  }
}

lluvia_efectiva <- lluvia_efectiva %>% filter(T_anios %in% T_comunes)

# Area de cuenca desde el resumen del 015. Si faltase, se intenta leer de las columnas del HU.
A_km2_global <- valor_numerico_primero(
  HU_Clark_resumen,
  c("A_km2", "area_total_Clark_km2", "area_km2", "Area_km2"),
  defecto = NA_real_
)

if (!is.finite(A_km2_global) && "A_km2" %in% names(HU_Clark)) {
  A_km2_vals <- suppressWarnings(as.numeric(HU_Clark$A_km2))
  A_km2_vals <- A_km2_vals[is.finite(A_km2_vals)]
  if (length(A_km2_vals) > 0) A_km2_global <- A_km2_vals[1]
}

if (!is.finite(A_km2_global) || A_km2_global <= 0) {
  warning("No se pudo determinar A_km2 desde 015. El balance de volumen quedara como NA.")
  A_km2_global <- NA_real_
}


# 6. CONVOLUCION POR PERIODO DE RETORNO ====

convolucion_Clark_T <- function(T_i) {
  lluvia_T <- lluvia_efectiva %>%
    filter(T_anios == T_i) %>%
    arrange(bloque)

  if (length(T_HU_validos) == 0) {
    hu_T_original <- HU_Clark %>% arrange(tiempo_h)
  } else {
    hu_T_original <- HU_Clark %>%
      filter(T_anios == T_i) %>%
      arrange(tiempo_h)
  }

  if (nrow(hu_T_original) == 0) {
    stop("No se encontro HU Clark para T = ", T_i)
  }

  dt_h <- obtener_paso_unico(lluvia_T$dt_h, paste0("dt_h para T = ", T_i))
  dt_min <- obtener_paso_unico(lluvia_T$dt_min, paste0("dt_min para T = ", T_i))

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
      lista_componentes[[j]] <- tibble::tibble(
        T_anios = T_i,
        metodo = "Clark",
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

  hidrograma <- tibble::tibble(
    T_anios = T_i,
    metodo = "Clark",
    tiempo_h = round(tiempos_h, digitos_tiempo),
    Q_directo_m3_s = Q_directo,
    Q_base_m3_s = Q_base_m3_s,
    Q_total_m3_s = Q_total,
    dt_h = dt_h,
    dt_min = dt_min
  )

  lluvia_malla <- tibble::tibble(
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
  volumen_objetivo_m3 <- if (is.finite(A_km2_global)) Pe_total_mm * 1000 * A_km2_global else NA_real_
  volumen_directo_m3 <- integrar_trapecios(hidrograma$tiempo_h, hidrograma$Q_directo_m3_s) * 3600
  volumen_total_m3 <- integrar_trapecios(hidrograma$tiempo_h, hidrograma$Q_total_m3_s) * 3600

  Qp_directo <- max(hidrograma$Q_directo_m3_s, na.rm = TRUE)
  Qp_total <- max(hidrograma$Q_total_m3_s, na.rm = TRUE)
  tiempo_pico_h <- hidrograma$tiempo_h[which.max(hidrograma$Q_total_m3_s)][1]

  resumen <- tibble::tibble(
    T_anios = T_i,
    metodo = "Clark",
    A_km2 = A_km2_global,
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

  componentes <- if (exportar_componentes) bind_rows(lista_componentes) else tibble::tibble()

  list(
    hidrograma = hidrograma,
    resumen = resumen,
    componentes = componentes
  )
}


# 7. APLICAR CONVOLUCION ====

lista_resultados <- lapply(T_comunes, convolucion_Clark_T)

hidrograma_Clark <- bind_rows(lapply(lista_resultados, `[[`, "hidrograma"))
resumen_Clark <- bind_rows(lapply(lista_resultados, `[[`, "resumen"))
componentes_Clark <- bind_rows(lapply(lista_resultados, `[[`, "componentes"))


# 8. EXPORTAR TABLAS ====

write_csv(hidrograma_Clark, salida_hidrograma_Clark)
write_csv(resumen_Clark, salida_resumen_Clark)

if (exportar_componentes) {
  write_csv(componentes_Clark, salida_componentes_Clark)
}


# 9. FIGURAS ====

# Orden común y estable de paneles. Al ser un factor, facet_wrap respeta este orden.
hidrograma_plot <- hidrograma_Clark %>%
  mutate(
    T_label = crear_T_label(T_anios)
  )

g_hid <- ggplot(hidrograma_plot, aes(x = tiempo_h, y = Q_total_m3_s)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ T_label, ncol = 3, scales = "free_y") +
  labs(
    title = "Hidrogramas finales de avenida - Clark",
    subtitle = "Convolución discreta de lluvia efectiva SCS-CN e hidrograma unitario de Clark",
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

combo_plot <- hidrograma_Clark %>%
  group_by(T_anios) %>%
  mutate(
    Qmax = max(Q_total_m3_s, na.rm = TRUE),
    Imax = max(intensidad_efectiva_mm_h, na.rm = TRUE),
    intensidad_efectiva_escalada = ifelse(Imax > 0, intensidad_efectiva_mm_h / Imax * Qmax, 0),
    T_label = crear_T_label(T_anios)
  ) %>%
  ungroup()

g_combo <- ggplot(combo_plot, aes(x = tiempo_h)) +
  geom_col(
    aes(y = intensidad_efectiva_escalada),
    width = unique(hidrograma_Clark$dt_h)[1],
    align = "edge",
    alpha = 0.35,
    color = "grey40"
  ) +
  geom_line(aes(y = Q_total_m3_s), linewidth = 0.8) +
  facet_wrap(~ T_label, ncol = 3, scales = "free_y") +
  labs(
    title = "Hietograma efectivo e hidrograma final - Clark",
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
cat("SCRIPT 016 FINALIZADO\n")
cat("====================================================\n")

cat("\nArchivos de entrada:\n")
cat(archivo_lluvia_efectiva_010, "\n")
cat(archivo_HU_Clark, "\n")
cat(archivo_HU_Clark_resumen_015, "\n")

cat("\nConfiguracion de convolucion Clark:\n")
cat("Q_base_m3_s =", Q_base_m3_s, "\n")
cat("exportar_componentes =", exportar_componentes, "\n")
cat("A_km2 =", round(A_km2_global, 4), "\n")

cat("\nPeriodos de retorno procesados:\n")
cat(paste(sort(unique(hidrograma_Clark$T_anios)), collapse = ", "), "años\n")

cat("\nTablas generadas:\n")
cat(salida_hidrograma_Clark, "\n")
cat(salida_resumen_Clark, "\n")
if (exportar_componentes) cat(salida_componentes_Clark, "\n")

cat("\nFiguras generadas:\n")
cat(salida_figura_hidrogramas, "\n")
cat(salida_figura_hietograma_hidrograma, "\n")

cat("\nResumen hidrogramas Clark:\n")
print(
  resumen_Clark %>%
    mutate(across(where(is.numeric), ~ round(.x, 4)))
)

cat("====================================================\n")
