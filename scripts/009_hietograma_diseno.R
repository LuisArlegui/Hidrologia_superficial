############################################################
#
# 009_hietograma_diseno.R
#
# Objetivo:
# - Leer la tabla generada por el script 006
# - Construir hietogramas de diseño a partir de una lluvia total de diseño
# - Redistribuir temporalmente esa lluvia mediante un patrón adimensional
# - Evitar derivar la tormenta bloque a bloque desde P(t) = I(t) * t
# - Preparar la entrada para el cálculo de lluvia efectiva SCS-CN
# - Exportar tablas y figuras para el bloque de hidrograma unitario
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

# Entrada procedente del script 006 versión Norma 5.2-IC.
archivo_idf_006 <- "salidas/tablas/006_lluvia_IDF_tiempo_concentracion.csv"

salida_hietogramas_largo <- "salidas/tablas/009_hietogramas_diseno_largo.csv"
salida_hietogramas_resumen <- "salidas/tablas/009_hietogramas_diseno_resumen.csv"
salida_figura <- "salidas/figuras/009_hietogramas_diseno.png"


# 3. PARAMETROS EDITABLES ====

# Periodos de retorno que se desean transformar en hietograma.
# Si se deja NULL, se usan todos los T_anios disponibles en el script 006.
T_seleccionados <- NULL
# Ejemplo:
# T_seleccionados <- c(10, 25, 100, 500)

# Intervalo temporal del hietograma, en minutos.
# Para cuencas naturales suele ser más estable usar 15-30 min que 5 min.
dt_min <- 30

# Duracion total de la tormenta de diseño.
# Opciones:
# - "auto" -> duracion_total_h = max(factor_duracion_tc * tc_h, duracion_minima_h)
# - valor numerico, por ejemplo 3 para una tormenta de 3 h.
duracion_total_h <- "auto"
factor_duracion_tc <- 1.5
duracion_minima_h <- 1

# Lluvia total usada para construir el hietograma.
# Opciones:
# - "IDF_duracion_total": P_total = I(T, duracion_total) * duracion_total
# - "Pd_KA":              P_total = Pd * KA
modo_lluvia_total <- "IDF_duracion_total"

# Metodo de redistribucion temporal de la lluvia total.
# Opciones:
# - "bloques_alternos"
# - "triangular"
# - "uniforme"
#
# Nota:
# Este script NO obtiene los incrementos como diff(I(t)*t), porque eso puede
# generar hietogramas artificialmente dominados por un único bloque inicial.
metodo_hietograma <- "bloques_alternos"

# Posicion relativa del bloque de maxima intensidad.
# 0.5 lo coloca aproximadamente en el centro.
posicion_pico <- 0.5

# Concentracion temporal del hietograma de bloques alternos.
# Valores pequeños concentran más lluvia cerca del pico.
# Valores grandes reparten más la lluvia.
# Rango razonable inicial: 0.20 - 0.40.
fraccion_decaimiento_bloques <- 0.30

# Control adicional para evitar hietogramas con un único bloque dominante.
# Limita la fraccion maxima de lluvia total que puede caer en un bloque.
# Rango razonable inicial: 0.10 - 0.25.
fraccion_maxima_bloque <- 0.20

# Exponente y constantes de la formulacion IDF empleada en 5.2-IC.
# Solo se usan para calcular la lluvia TOTAL asociada a la duracion total,
# no para derivar incrementos temporales bloque a bloque.
coef_a <- 3.5287
coef_b <- 2.5287
exponente_t <- 0.1


# 4. COMPROBAR ARCHIVOS DE ENTRADA ====

if (!file.exists(archivo_idf_006)) {
  stop(
    "No se encuentra la tabla del script 006: ", archivo_idf_006,
    "\nEjecuta antes 006_lluvia_IDF_tiempo_concentracion_52IC.R."
  )
}


# 5. LEER DATOS ====

idf_006 <- read_csv(archivo_idf_006, show_col_types = FALSE)

campos_necesarios <- c("T_anios", "Pd_mm", "KA", "I1_Id", "tc_h")
faltan <- setdiff(campos_necesarios, names(idf_006))

if (length(faltan) > 0) {
  stop(
    "Faltan campos necesarios en el archivo 006: ",
    paste(faltan, collapse = ", "),
    "\nCampos disponibles: ", paste(names(idf_006), collapse = ", ")
  )
}

idf_006 <- idf_006 %>%
  mutate(
    T_anios = as.numeric(T_anios),
    Pd_mm = as.numeric(Pd_mm),
    KA = as.numeric(KA),
    I1_Id = as.numeric(I1_Id),
    tc_h = as.numeric(tc_h)
  )

if (any(!is.finite(idf_006$T_anios))) stop("T_anios contiene valores no validos.")
if (any(!is.finite(idf_006$Pd_mm) | idf_006$Pd_mm <= 0)) stop("Pd_mm contiene valores no validos.")
if (any(!is.finite(idf_006$KA) | idf_006$KA <= 0)) stop("KA contiene valores no validos.")
if (any(!is.finite(idf_006$I1_Id) | idf_006$I1_Id <= 0)) stop("I1_Id contiene valores no validos.")
if (any(!is.finite(idf_006$tc_h) | idf_006$tc_h <= 0)) stop("tc_h contiene valores no validos.")


# 6. FILTRAR PERIODOS DE RETORNO ====

if (!is.null(T_seleccionados)) {
  idf_006 <- idf_006 %>%
    filter(T_anios %in% T_seleccionados)

  if (nrow(idf_006) == 0) {
    stop("Ninguno de los T_seleccionados existe en la tabla del script 006.")
  }
}


# 7. FUNCIONES AUXILIARES ====

calcular_duracion_total <- function(tc_h) {
  if (is.character(duracion_total_h) && duracion_total_h == "auto") {
    return(max(factor_duracion_tc * tc_h, duracion_minima_h))
  }

  dur <- suppressWarnings(as.numeric(duracion_total_h))
  if (!is.finite(dur) || dur <= 0) {
    stop("duracion_total_h debe ser 'auto' o un numero positivo en horas.")
  }
  return(dur)
}

intensidad_idf_52IC <- function(Pd_mm, KA, I1_Id, t_h) {
  # Id = Pd * KA / 24
  # I(t) = Id * (I1/Id)^(a - b * t^0.1)
  # t_h en horas; resultado en mm/h.
  Id_mm_h <- Pd_mm * KA / 24
  Fint <- I1_Id^(coef_a - coef_b * t_h^exponente_t)
  I_mm_h <- Id_mm_h * Fint
  return(I_mm_h)
}

calcular_lluvia_total <- function(Pd_mm, KA, I1_Id, dur_h) {
  if (modo_lluvia_total == "IDF_duracion_total") {
    I_dur_mm_h <- intensidad_idf_52IC(Pd_mm, KA, I1_Id, dur_h)
    return(I_dur_mm_h * dur_h)
  }

  if (modo_lluvia_total == "Pd_KA") {
    return(Pd_mm * KA)
  }

  stop("modo_lluvia_total no reconocido: ", modo_lluvia_total)
}

posiciones_bloques_alternos <- function(n, posicion_pico = 0.5) {
  centro <- round(posicion_pico * n)
  centro <- max(1, min(n, centro))

  posiciones <- centro
  paso <- 1

  while (length(posiciones) < n) {
    derecha <- centro + paso
    izquierda <- centro - paso

    if (derecha <= n) posiciones <- c(posiciones, derecha)
    if (izquierda >= 1) posiciones <- c(posiciones, izquierda)

    paso <- paso + 1
  }

  posiciones[seq_len(n)]
}

limitar_pico_pesos <- function(w, fraccion_maxima = 0.20, max_iter = 100) {
  # Ajusta iterativamente los pesos para que ningun bloque supere
  # fraccion_maxima de la lluvia total.
  if (!is.finite(fraccion_maxima) || fraccion_maxima <= 0 || fraccion_maxima >= 1) {
    return(w / sum(w))
  }

  w <- w / sum(w)

  for (i in seq_len(max_iter)) {
    idx <- which(w > fraccion_maxima)
    if (length(idx) == 0) break

    exceso <- sum(w[idx] - fraccion_maxima)
    w[idx] <- fraccion_maxima

    idx_libres <- which(w < fraccion_maxima)
    if (length(idx_libres) == 0) break

    w[idx_libres] <- w[idx_libres] + exceso * w[idx_libres] / sum(w[idx_libres])
    w <- w / sum(w)
  }

  w / sum(w)
}

pesos_hietograma <- function(n, metodo = "bloques_alternos", posicion_pico = 0.5) {
  if (n < 1) stop("El numero de bloques debe ser mayor que cero.")

  if (metodo == "uniforme") {
    return(rep(1 / n, n))
  }

  if (metodo == "triangular") {
    centro <- round(posicion_pico * n)
    centro <- max(1, min(n, centro))

    dist <- abs(seq_len(n) - centro)
    max_dist <- max(dist)

    if (max_dist == 0) {
      w <- 1
    } else {
      w <- 1 - dist / (max_dist + 1)
    }

    w <- pmax(w, 0)
    return(w / sum(w))
  }

  if (metodo == "bloques_alternos") {
    # Se genera un patrón adimensional de intensidades relativas,
    # descendente desde el bloque pico. No procede de diff(I(t)*t).
    posiciones <- posiciones_bloques_alternos(n, posicion_pico)

    rango <- seq_len(n) - 1
    lambda <- 1 / max(fraccion_decaimiento_bloques * n, 1)

    pesos_ordenados <- exp(-lambda * rango)
    pesos_ordenados <- pesos_ordenados / sum(pesos_ordenados)

    w <- rep(NA_real_, n)
    w[posiciones] <- pesos_ordenados

    w <- limitar_pico_pesos(w, fraccion_maxima_bloque)
    return(w / sum(w))
  }

  stop("Metodo de hietograma no reconocido: ", metodo)
}

construir_hietograma_T <- function(fila) {
  T_anios <- fila$T_anios
  Pd_mm <- fila$Pd_mm
  KA <- fila$KA
  I1_Id <- fila$I1_Id
  tc_h <- fila$tc_h

  dur_h <- calcular_duracion_total(tc_h)
  dt_h <- dt_min / 60

  if (dt_h <= 0) stop("dt_min debe ser positivo.")
  if (dur_h < dt_h) stop("La duracion total debe ser mayor o igual que dt_min.")

  n_bloques <- ceiling(dur_h / dt_h)
  tiempos_fin_h <- seq(dt_h, by = dt_h, length.out = n_bloques)
  dur_h_real <- max(tiempos_fin_h)

  # Lluvia total de diseño para la duración real del hietograma.
  P_total_mm <- calcular_lluvia_total(Pd_mm, KA, I1_Id, dur_h_real)

  if (!is.finite(P_total_mm) || P_total_mm <= 0) {
    stop("La lluvia total calculada no es valida para T = ", T_anios)
  }

  # Distribución temporal adimensional.
  w <- pesos_hietograma(
    n = n_bloques,
    metodo = metodo_hietograma,
    posicion_pico = posicion_pico
  )

  P_incremental_mm <- P_total_mm * w
  intensidad_media_total_mm_h <- P_total_mm / dur_h_real

  tibble(
    T_anios = T_anios,
    metodo_hietograma = metodo_hietograma,
    modo_lluvia_total = modo_lluvia_total,
    bloque = seq_len(n_bloques),
    tiempo_inicio_h = tiempos_fin_h - dt_h,
    tiempo_fin_h = tiempos_fin_h,
    tiempo_centro_h = tiempos_fin_h - dt_h / 2,
    dt_min = dt_min,
    dt_h = dt_h,
    duracion_total_h = dur_h_real,
    tc_h = tc_h,
    Pd_mm = Pd_mm,
    KA = KA,
    I1_Id = I1_Id,
    P_total_diseno_mm = P_total_mm,
    peso_bloque = w,
    P_incremental_mm = P_incremental_mm,
    intensidad_bloque_mm_h = P_incremental_mm / dt_h,
    intensidad_media_total_mm_h = intensidad_media_total_mm_h,
    P_acumulada_hietograma_mm = cumsum(P_incremental_mm)
  )
}


# 8. CONSTRUIR HIETOGRAMAS ====

lista_hietogramas <- lapply(seq_len(nrow(idf_006)), function(i) {
  construir_hietograma_T(idf_006[i, ])
})

hietogramas <- bind_rows(lista_hietogramas)


# 9. RESUMEN ====

resumen <- hietogramas %>%
  group_by(T_anios, metodo_hietograma, modo_lluvia_total) %>%
  summarise(
    n_bloques = n(),
    dt_min = first(dt_min),
    duracion_total_h = first(duracion_total_h),
    tc_h = first(tc_h),
    Pd_mm = first(Pd_mm),
    P_total_diseno_mm = first(P_total_diseno_mm),
    P_total_hietograma_mm = sum(P_incremental_mm, na.rm = TRUE),
    intensidad_media_total_mm_h = first(intensidad_media_total_mm_h),
    intensidad_maxima_bloque_mm_h = max(intensidad_bloque_mm_h, na.rm = TRUE),
    fraccion_maxima_real = max(P_incremental_mm / sum(P_incremental_mm), na.rm = TRUE),
    bloque_pico = bloque[which.max(intensidad_bloque_mm_h)][1],
    tiempo_pico_h = tiempo_centro_h[which.max(intensidad_bloque_mm_h)][1],
    .groups = "drop"
  )


# 10. EXPORTAR TABLAS ====

write_csv(hietogramas, salida_hietogramas_largo)
write_csv(resumen, salida_hietogramas_resumen)


# 11. FIGURA ====

orden_T <- sort(unique(hietogramas$T_anios))

hietogramas_plot <- hietogramas %>%
  mutate(
    T_label = factor(
      paste0("T = ", T_anios, " años"),
      levels = paste0("T = ", orden_T, " años")
    )
  )

g <- ggplot(
  hietogramas_plot,
  aes(x = tiempo_inicio_h, y = intensidad_bloque_mm_h)
) +
  geom_col(width = dt_min / 60, align = "edge", color = "grey30") +
  facet_wrap(~ T_label, ncol = 3, scales = "free_y") +
  labs(
    title = "Hietogramas de diseño",
    subtitle = paste0(
      "Método: ", metodo_hietograma,
      " | lluvia total: ", modo_lluvia_total,
      " | dt = ", dt_min, " min"
    ),
    x = "Tiempo desde el inicio de la tormenta (h)",
    y = "Intensidad del bloque (mm/h)"
  ) +
  theme_minimal()

ggsave(
  salida_figura,
  g,
  width = 10,
  height = 7,
  dpi = 300
)

print(g)


# 12. RESUMEN EN CONSOLA ====

cat("\n====================================================\n")
cat("SCRIPT 009 FINALIZADO\n")
cat("====================================================\n")

cat("\nArchivo de entrada:\n")
cat(archivo_idf_006, "\n")

if ("metodo_tc" %in% names(idf_006)) {
  cat("\nMetodo tc leído del 006:\n")
  cat(paste(unique(idf_006$metodo_tc), collapse = ", "), "\n")
}

cat("\nConfiguracion del hietograma:\n")
cat("Metodo =", metodo_hietograma, "\n")
cat("Modo lluvia total =", modo_lluvia_total, "\n")
cat("dt =", dt_min, "min\n")
cat("duracion_total_h =", as.character(duracion_total_h), "\n")
cat("factor_duracion_tc =", factor_duracion_tc, "\n")
cat("duracion_minima_h =", duracion_minima_h, "\n")
cat("posicion_pico =", posicion_pico, "\n")
cat("fraccion_decaimiento_bloques =", fraccion_decaimiento_bloques, "\n")
cat("fraccion_maxima_bloque =", fraccion_maxima_bloque, "\n")

cat("\nPeriodos de retorno procesados:\n")
cat(paste(sort(unique(hietogramas$T_anios)), collapse = ", "), "años\n")

cat("\nTablas generadas:\n")
cat(salida_hietogramas_largo, "\n")
cat(salida_hietogramas_resumen, "\n")

cat("\nFigura generada:\n")
cat(salida_figura, "\n")

cat("\nResumen:\n")
print(
  resumen %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
)

cat("====================================================\n")
