############################################################
#
# 003_lluvias_diseno_desde_datos_diarios.R
#
# Objetivo:
# - Leer precipitación diaria en formato mensual ancho
#   con columnas ID, year, MES, P1 ... P31
# - Convertir a serie diaria larga
# - Eliminar fechas imposibles
# - Calcular máximos anuales
# - Ajustar SQRT-ETmax, Gumbel y Log-Pearson III
# - Exportar lluvias de diseño Pd(T)
#
############################################################

# 0. PAQUETES ====

paquetes <- c("ggplot2", "readr", "dplyr", "tidyr", "lubridate")

instalar <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(instalar) > 0) install.packages(instalar)

library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)


# 1. PARAMETROS DE ENTRADA ====

if (file.exists("scripts/00_configuracion.R")) {
  source("scripts/00_configuracion.R")
}

archivo_csv <- "datos/brutos/valmadrid_lluvia.csv"

usar_csv2 <- FALSE

T_ret <- c(2, 5, 10, 25, 50, 100, 200, 500)


# 2. RUTAS DE SALIDA ====

dir.create("datos/procesados", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/figuras", recursive = TRUE, showWarnings = FALSE)

salida_diaria <- "datos/procesados/precipitacion_diaria_limpia.csv"
salida_maximos <- "datos/procesados/precipitacion_maxima_anual.csv"

salida_estadisticos <- "salidas/tablas/estadisticos_precipitacion.csv"
salida_lluvias <- "salidas/tablas/lluvias_diseno_comparacion.csv"

salida_curvas <- "salidas/figuras/curvas_retorno.png"
salida_diferencias <- "salidas/figuras/diferencias_vs_gumbel.png"
salida_curvas_ic <- "salidas/figuras/curvas_retorno_con_IC.png"


# 3. FUNCIONES AUXILIARES ====

skewness_sample <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  
  xbar <- mean(x)
  s <- sd(x)
  
  if (!is.finite(s) || s <= 0) return(0)
  
  g1 <- (n / ((n - 1) * (n - 2))) * sum(((x - xbar) / s)^3)
  return(g1)
}

weibull_return_period <- function(x) {
  x_sorted <- sort(x, decreasing = TRUE)
  n <- length(x_sorted)
  m <- seq_len(n)
  T <- (n + 1) / m
  
  data.frame(T = T, P = x_sorted)
}


# 4. METODO SQRT-ETMAX ====

sqrt_etmax_params <- function(media, sdv) {
  
  if (!is.finite(media) || media <= 0) {
    stop("La media debe ser positiva para SQRT-ETmax.")
  }
  
  if (!is.finite(sdv) || sdv <= 0) {
    stop("La desviacion estandar debe ser positiva para SQRT-ETmax.")
  }
  
  Cv <- sdv / media
  
  if (Cv < 0.19 || Cv > 0.99) {
    stop("El metodo SQRT-ETmax esta parametrizado para 0.19 <= Cv <= 0.99.")
  }
  
  if (Cv >= 0.19 && Cv < 0.30) {
    a <- c(-1765.864939, -7240.591990, -11785.550150,
           -9537.985174, -3834.341011, -612.6777022, 0)
    b <- c(-0.931508488, 2.156709166, -0.779770335,
           0.112962273, -0.009340454, 0.000411896, -7.5371e-06)
    
  } else if (Cv >= 0.30 && Cv < 0.70) {
    a <- c(1.801512523, 2.473760587, 23.556200190,
           49.95727380, 59.77563587, 35.69687628, 8.505712707)
    b <- c(2.342696882, -0.149784237, -0.099311975,
           0.003444122, 0.001013501, -0.000141410, 5.49271e-06)
    
  } else if (Cv >= 0.70 && Cv <= 0.99) {
    a <- c(1.318614521, -3.164627686, -1.595524397,
           -6.269110853, -11.31766964, -22.69755500, -22.06634469)
    b <- c(2.307318543, -0.136673609, -0.075035627,
           -0.013464311, 0.003228029, 0.000521240, -0.000140900)
  }
  
  lnCv <- log(Cv)
  j <- 0:6
  
  ln_k <- sum(a * (lnCv^j))
  k <- exp(ln_k)
  
  ln_I1 <- sum(b * (ln_k^j))
  I1 <- exp(ln_I1)
  
  alpha <- k * I1 / ((1 - exp(-k)) * 2 * media)
  
  list(
    Cv = Cv,
    k = k,
    I1 = I1,
    alpha = alpha
  )
}

sqrt_etmax_F <- function(x, media, sdv) {
  pars <- sqrt_etmax_params(media, sdv)
  k <- pars$k
  alpha <- pars$alpha
  
  F_x <- exp(-k * (1 + sqrt(alpha * x)) * exp(-sqrt(alpha * x)))
  return(F_x)
}

sqrt_etmax_quantile <- function(T, media, sdv) {
  
  if (T <= 1) stop("El periodo de retorno T debe ser mayor que 1.")
  
  p <- 1 - 1 / T
  
  f_obj <- function(x) {
    sqrt_etmax_F(x, media, sdv) - p
  }
  
  upper <- max(media + 10 * sdv, 10)
  
  while (f_obj(upper) < 0) {
    upper <- upper * 2
    if (upper > 1e7) {
      stop("No se encontro un intervalo adecuado para SQRT-ETmax.")
    }
  }
  
  uniroot(f_obj, lower = 0, upper = upper)$root
}



# 5. METODO DE GUMBEL ====

qgumbel_T <- function(T, media, sdv) {
  
  if (T <= 1) stop("El periodo de retorno T debe ser mayor que 1.")
  
  p <- 1 - 1 / T
  gamma_euler <- 0.577215664901533
  
  beta <- sdv * sqrt(6) / pi
  mu <- media - gamma_euler * beta
  
  xT <- mu - beta * log(-log(p))
  return(xT)
}

# 6. METODO LOG-PEARSON III ====

qlp3_T <- function(T, x) {
  
  if (T <= 1) stop("El periodo de retorno T debe ser mayor que 1.")
  
  x <- x[is.finite(x)]
  
  if (any(x <= 0)) {
    stop("Log-Pearson III requiere precipitaciones estrictamente positivas.")
  }
  
  y <- log10(x)
  
  media_y <- mean(y)
  sd_y <- sd(y)
  g_y <- skewness_sample(y)
  
  p <- 1 - 1 / T
  
  if (is.na(g_y) || abs(g_y) < 1e-8) {
    yT <- media_y + qnorm(p) * sd_y
    xT <- 10^yT
    return(xT)
  }
  
  alpha <- 4 / (g_y^2)
  beta <- sd_y * abs(g_y) / 2
  xi <- media_y - 2 * sd_y / g_y
  
  if (g_y > 0) {
    yT <- xi + qgamma(p, shape = alpha, scale = beta)
  } else {
    yT <- xi - qgamma(1 - p, shape = alpha, scale = beta)
  }
  
  xT <- 10^yT
  return(xT)
}


# 7. LECTURA Y TRANSFORMACION DE DATOS DIARIOS====

if (!file.exists(archivo_csv)) {
  stop("No se encuentra el archivo: ", archivo_csv)
}

datos_raw <- if (usar_csv2) {
  read_csv2(archivo_csv, show_col_types = FALSE)
} else {
  read_csv(archivo_csv, show_col_types = FALSE)
}

campos_necesarios <- c("ID", "year", "MES")
faltan <- setdiff(campos_necesarios, names(datos_raw))

if (length(faltan) > 0) {
  stop("Faltan campos necesarios: ", paste(faltan, collapse = ", "))
}

cols_p <- paste0("P", 1:31)
cols_p_existentes <- intersect(cols_p, names(datos_raw))

if (length(cols_p_existentes) == 0) {
  stop("No se han encontrado columnas P1...P31.")
}

factor_escala_precipitacion <- 0.1

serie_diaria <- datos_raw %>%
  select(ID, year, MES, all_of(cols_p_existentes)) %>%
  pivot_longer(
    cols = all_of(cols_p_existentes),
    names_to = "dia_col",
    values_to = "P_mm"
  ) %>%
  mutate(
    dia = as.integer(gsub("P", "", dia_col)),
    year = as.integer(year),
    MES = as.integer(MES),
    fecha_txt = sprintf("%04d-%02d-%02d", year, MES, dia),
    fecha = suppressWarnings(ymd(fecha_txt)),
    
    P_mm = as.numeric(P_mm),
    
    # Valores negativos = precipitación inapreciable
    P_mm = ifelse(P_mm < 0, 0, P_mm),
    
    # Conversión desde décimas de mm a mm
    P_mm = P_mm * factor_escala_precipitacion
  ) %>%
  filter(
    !is.na(fecha),
    !is.na(P_mm)
  ) %>%
  select(ID, fecha, year, MES, dia, P_mm) %>%
  arrange(fecha)


write_csv(serie_diaria, salida_diaria)



# 8. MAXIMOS ANUALES ====

# 8. MAXIMOS ANUALES ====

dias_minimos_anio <- 330

maximos_anuales_todos <- serie_diaria %>%
  group_by(year) %>%
  summarise(
    Pmax = max(P_mm, na.rm = TRUE),
    fecha_Pmax = fecha[which.max(P_mm)],
    n_dias_validos = sum(!is.na(P_mm)),
    .groups = "drop"
  ) %>%
  filter(is.finite(Pmax))

maximos_anuales <- maximos_anuales_todos %>%
  filter(
    n_dias_validos >= dias_minimos_anio,
    Pmax > 0
  )

write_csv(maximos_anuales_todos,
          "datos/procesados/precipitacion_maxima_anual_todos_los_anios.csv")

write_csv(maximos_anuales, salida_maximos)

prec <- maximos_anuales$Pmax
prec <- prec[is.finite(prec)]

cat("\nAños excluidos por incompletos o Pmax = 0:\n")
print(
  maximos_anuales_todos %>%
    filter(n_dias_validos < dias_minimos_anio | Pmax <= 0)
)




# 9. ESTADISTICOS DESCRIPTIVOS ====

n <- length(prec)
media <- mean(prec)
sdv <- sd(prec)
skw <- skewness_sample(prec)

prec_pos <- prec[prec > 0]
log_prec <- log10(prec_pos)

media_log <- mean(log_prec)
sd_log <- sd(log_prec)
skw_log <- skewness_sample(log_prec)

pars_sqrt <- sqrt_etmax_params(media, sdv)

estadisticos <- data.frame(
  Estadistico = c(
    "n",
    "Media",
    "Desviacion_estandar",
    "Skewness",
    "Media_log10",
    "SD_log10",
    "Skewness_log10",
    "Cv",
    "k_SQRT_ETmax",
    "I1_SQRT_ETmax",
    "alpha_SQRT_ETmax"
  ),
  Valor = c(
    n,
    media,
    sdv,
    skw,
    media_log,
    sd_log,
    skw_log,
    pars_sqrt$Cv,
    pars_sqrt$k,
    pars_sqrt$I1,
    pars_sqrt$alpha
  )
)



# 10. LLUVIAS DE DISEÑO ====

resultado <- data.frame(
  T = T_ret,
  Prob_excedencia = 1 / T_ret,
  Prob_no_excedencia = 1 - 1 / T_ret
)

resultado$SQRT_ETmax <- sapply(T_ret, sqrt_etmax_quantile, media = media, sdv = sdv)
resultado$Gumbel <- sapply(T_ret, qgumbel_T, media = media, sdv = sdv)
prec_lp3 <- prec[prec > 0]

if (length(prec_lp3) < 10) {
  warning("Log-Pearson III se calculará con menos de 10 máximos anuales positivos.")
}

resultado$LogPearsonIII <- sapply(T_ret, qlp3_T, x = prec_lp3)

resultado$Dif_SQRT_vs_Gumbel_pct <- 100 *
  (resultado$SQRT_ETmax - resultado$Gumbel) / resultado$Gumbel

resultado$Dif_LP3_vs_Gumbel_pct <- 100 *
  (resultado$LogPearsonIII - resultado$Gumbel) / resultado$Gumbel




# 11. GRAFICOS ====

obs_plot <- weibull_return_period(prec)
obs_plot$Metodo <- "Observado"

curvas_plot <- rbind(
  data.frame(T = resultado$T, P = resultado$SQRT_ETmax, Metodo = "SQRT-ETmax"),
  data.frame(T = resultado$T, P = resultado$Gumbel, Metodo = "Gumbel"),
  data.frame(T = resultado$T, P = resultado$LogPearsonIII, Metodo = "Log-Pearson III")
)

dif_plot <- rbind(
  data.frame(
    T = resultado$T,
    Diferencia_pct = resultado$Dif_SQRT_vs_Gumbel_pct,
    Metodo = "SQRT-ETmax vs Gumbel"
  ),
  data.frame(
    T = resultado$T,
    Diferencia_pct = resultado$Dif_LP3_vs_Gumbel_pct,
    Metodo = "Log-Pearson III vs Gumbel"
  )
)

g1 <- ggplot() +
  geom_point(
    data = obs_plot,
    aes(x = T, y = P),
    size = 2,
    color = "black",
    alpha = 0.5
  ) +
  geom_line(
    data = curvas_plot,
    aes(x = T, y = P, linetype = Metodo, color = Metodo),
    linewidth = 1
  ) +
  scale_x_log10(
    breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)
  ) +
  labs(
    title = "Curvas de retorno de precipitación máxima anual",
    x = "Periodo de retorno, T (años)",
    y = "Precipitación diaria de diseño, Pd (mm)",
    linetype = "Método",
    color = "Método"
  ) +
  theme_gray()

g2 <- ggplot(dif_plot, aes(x = T, y = Diferencia_pct, linetype = Metodo)) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_log10(breaks = c(2, 5, 10, 20, 50, 100, 200, 500)) +
  labs(
    title = "Diferencias porcentuales respecto a Gumbel",
    x = "Periodo de retorno, T (años)",
    y = "Diferencia (%)",
    linetype = "Comparación"
  ) +
  theme_gray()

print(g1)
print(g2)



# 12. INTERVALOS DE CONFIANZA POR BOOTSTRAP ====

bootstrap_ic <- function(x, T_ret, nboot = 1000) {
  
  n <- length(x)
  
  res_gumbel <- matrix(NA, nrow = nboot, ncol = length(T_ret))
  res_lp3 <- matrix(NA, nrow = nboot, ncol = length(T_ret))
  res_sqrt <- matrix(NA, nrow = nboot, ncol = length(T_ret))
  
  for (i in 1:nboot) {
    
    xb <- sample(x, size = n, replace = TRUE)
    
    media_b <- mean(xb)
    sd_b <- sd(xb)
    
    if (sd_b <= 0 || media_b <= 0) next
    
    try({
      res_sqrt[i, ] <- sapply(
        T_ret,
        sqrt_etmax_quantile,
        media = media_b,
        sdv = sd_b
      )
    }, silent = TRUE)
    
    res_gumbel[i, ] <- sapply(
      T_ret,
      qgumbel_T,
      media = media_b,
      sdv = sd_b
    )
    
    try({
      xb_lp3 <- xb[xb > 0]
      if (length(xb_lp3) >= 3) {
        res_lp3[i, ] <- sapply(T_ret, qlp3_T, x = xb_lp3)
      }
    }, silent = TRUE)
  }
  
  get_ci <- function(mat) {
    apply(mat, 2, function(col) {
      quantile(col, probs = c(0.025, 0.975), na.rm = TRUE)
    })
  }
  
  list(
    gumbel = get_ci(res_gumbel),
    lp3 = get_ci(res_lp3),
    sqrt = get_ci(res_sqrt)
  )
}

set.seed(1234)

cat("\nCalculando intervalos de confianza por bootstrap...\n")

ci <- bootstrap_ic(prec, T_ret, nboot = 1000)

resultado$Gumbel_low <- ci$gumbel[1, ]
resultado$Gumbel_high <- ci$gumbel[2, ]

resultado$LP3_low <- ci$lp3[1, ]
resultado$LP3_high <- ci$lp3[2, ]

resultado$SQRT_low <- ci$sqrt[1, ]
resultado$SQRT_high <- ci$sqrt[2, ]

g3 <- ggplot() +
  geom_ribbon(
    data = resultado,
    aes(x = T, ymin = Gumbel_low, ymax = Gumbel_high),
    fill = "blue",
    alpha = 0.2
  ) +
  geom_ribbon(
    data = resultado,
    aes(x = T, ymin = LP3_low, ymax = LP3_high),
    fill = "green",
    alpha = 0.2
  ) +
  geom_ribbon(
    data = resultado,
    aes(x = T, ymin = SQRT_low, ymax = SQRT_high),
    fill = "red",
    alpha = 0.2
  ) +
  geom_line(
    data = curvas_plot,
    aes(x = T, y = P, color = Metodo),
    linewidth = 1
  ) +
  geom_point(
    data = obs_plot,
    aes(x = T, y = P),
    color = "black",
    size = 2
  ) +
  scale_x_log10(
    breaks = c(2, 5, 10, 20, 50, 100, 200, 500)
  ) +
  labs(
    title = "Curvas de retorno con intervalos de confianza",
    x = "Periodo de retorno, T (años)",
    y = "Precipitación diaria de diseño, Pd (mm)"
  ) +
  theme_gray()

print(g3)




# 13. EXPORTACION ====

write_csv(estadisticos, salida_estadisticos)
write_csv(resultado, salida_lluvias)

ggsave(salida_curvas, plot = g1, width = 8, height = 5, dpi = 300)
ggsave(salida_diferencias, plot = g2, width = 8, height = 5, dpi = 300)
ggsave(salida_curvas_ic, plot = g3, width = 8, height = 5, dpi = 300)



# 14. RESUMEN EN CONSOLA ====

cat("\n============================================\n")
cat("SCRIPT 006a FINALIZADO\n")
cat("============================================\n")

cat("\nSerie diaria limpia:\n")
cat(salida_diaria, "\n")

cat("\nMáximos anuales:\n")
cat(salida_maximos, "\n")

cat("\nEstadísticos:\n")
cat(salida_estadisticos, "\n")

cat("\nLluvias de diseño Pd(T):\n")
cat(salida_lluvias, "\n")

cat("\nFiguras:\n")
cat(salida_curvas, "\n")
cat(salida_diferencias, "\n")
cat(salida_curvas_ic, "\n")

cat("\nResumen de lluvias de diseño:\n")
print(round(resultado, 3))

cat("============================================\n")
