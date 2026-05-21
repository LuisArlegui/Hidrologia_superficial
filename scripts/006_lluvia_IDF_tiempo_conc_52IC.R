############################################################
#
# 006_lluvia_IDF_tiempo_concentracion_52IC.R
#
# Objetivo:
# - Leer las lluvias de diseño Pd(T) obtenidas en el script 003
# - Leer parámetros hidrológicos del script 005
# - Leer la morfometría del cauce principal obtenida en el script 004
# - Calcular el tiempo de concentración según la formulación
#   empleada en la Norma 5.2-IC
# - Calcular KA, Id, Fa, Fint, I(T,tc) y Kt
# - Exportar tabla lista para el método racional y para los
#   módulos posteriores de hietograma e hidrograma unitario
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

if (file.exists("scripts/000_rutas_y_capas.R")) {
  source("scripts/000_rutas_y_capas.R")
}

dir.create("salidas/tablas", recursive = TRUE, showWarnings = FALSE)
dir.create("salidas/figuras", recursive = TRUE, showWarnings = FALSE)


# 2. RUTAS ====

archivo_param_005 <- "salidas/tablas/005_parametros_hidrologicos_resumen.csv"
archivo_lluvias_diseno <- "salidas/tablas/003_lluvias_diseno_comparacion.csv"
archivo_morfometria <- "salidas/tablas/004_morfometria_cauce_principal.csv"

salida_idf <- "salidas/tablas/006_lluvia_IDF_tiempo_concentracion.csv"
salida_figura <- "salidas/figuras/006_intensidad_vs_T_52IC.png"


# 3. PARAMETROS EDITABLES ====

# Método de precipitación diaria de diseño procedente del script 003.
# Opciones habituales:
# "SQRT_ETmax", "Gumbel", "LogPearsonIII"
metodo_Pd <- "SQRT_ETmax"

# Relación I1/Id.
# EDITAR según la figura/mapa correspondiente de la Norma 5.2-IC.
I1_Id <- 10

# Método de tiempo de concentración.
# Esta versión implementa la formulación principal de la Norma 5.2-IC:
# tc = 0.3 * Lc^0.76 * Jc^-0.19
# con Lc en km, Jc adimensional y tc en horas.
metodo_tc <- "principal_52IC"


# 4. COMPROBAR ARCHIVOS DE ENTRADA ====

if (!file.exists(archivo_param_005)) {
  stop("No se encuentra el resumen del script 005: ", archivo_param_005)
}

if (!file.exists(archivo_lluvias_diseno)) {
  stop("No se encuentra la tabla de lluvias de diseño del script 003: ", archivo_lluvias_diseno)
}

if (!file.exists(archivo_morfometria)) {
  stop(
    paste0(
      "No se encuentra la tabla de morfometría del cauce principal:\n",
      archivo_morfometria,
      "\n\nEjecuta primero el script 004_morfometria_cauce_principal.R."
    )
  )
}


# 5. LEER DATOS ====

param_005 <- read_csv(archivo_param_005, show_col_types = FALSE)
lluvias_diseno <- read_csv(archivo_lluvias_diseno, show_col_types = FALSE)
morf <- read_csv(archivo_morfometria, show_col_types = FALSE)


# 6. COMPROBACIONES ====

if (!("area_cuenca_km2" %in% names(param_005))) {
  stop("El archivo del script 005 debe contener el campo area_cuenca_km2.")
}

if (!("T" %in% names(lluvias_diseno))) {
  stop("La tabla de lluvias de diseño debe contener el campo T.")
}

if (!(metodo_Pd %in% names(lluvias_diseno))) {
  stop(
    "El método seleccionado en metodo_Pd no existe en la tabla de lluvias.\n",
    "Método seleccionado: ", metodo_Pd, "\n",
    "Campos disponibles: ", paste(names(lluvias_diseno), collapse = ", ")
  )
}

campos_morf <- c("Lc_km", "Jc")
faltan_morf <- setdiff(campos_morf, names(morf))

if (length(faltan_morf) > 0) {
  stop(
    "Faltan campos en 004_morfometria_cauce_principal.csv: ",
    paste(faltan_morf, collapse = ", ")
  )
}


# 7. EXTRAER PARAMETROS ====

A_km2 <- as.numeric(param_005$area_cuenca_km2[1])

Lc_km <- as.numeric(morf$Lc_km[1])
Jc <- as.numeric(morf$Jc[1])

if (!is.finite(A_km2) || A_km2 <= 0) stop("Area de cuenca no valida.")
if (!is.finite(Lc_km) || Lc_km <= 0) stop("Lc_km no valido.")
if (!is.finite(Jc) || Jc <= 0) stop("Jc no valido.")
if (!is.finite(I1_Id) || I1_Id <= 0) stop("I1_Id no valido.")


# 8. TIEMPO DE CONCENTRACION ====


  tc_h <- 0.3 * Lc_km^0.76 * Jc^(-0.19)

tc_min <- tc_h * 60


# 9. FACTOR REDUCTOR POR AREA KA ====

# Formula habitual:
# KA = 1 para A < 1 km2
# KA = 1 - log10(A) / 15 para A >= 1 km2

KA <- ifelse(
  A_km2 < 1,
  1,
  1 - log10(A_km2) / 15
)


# 10. FACTOR DE INTENSIDAD Fint ====

# Id = Pd * KA / 24
#
# I(T,tc) = Id * Fint
#
# Fa = (I1/Id)^(3.5287 - 2.5287 * tc^0.1)
#
# con tc en horas.

Fa <- I1_Id^(3.5287 - 2.5287 * tc_h^0.1)

Fb <- NA_real_

Fint <- Fa


# 11. COEFICIENTE DE UNIFORMIDAD TEMPORAL Kt ====

Kt <- 1 + tc_h^1.25 / (tc_h^1.25 + 14)


# 12. CONSTRUIR TABLA DE LLUVIA E INTENSIDAD ====

resultado <- lluvias_diseno %>%
  transmute(
    T_anios = T,
    metodo_Pd = metodo_Pd,
    Pd_mm = .data[[metodo_Pd]]
  ) %>%
  mutate(
    A_km2 = A_km2,
    Lc_km = Lc_km,
    Jc = Jc,
    metodo_tc = metodo_tc,
    tc_h = tc_h,
    tc_min = tc_min,
    KA = KA,
    I1_Id = I1_Id,
    Fa = Fa,
    Fb = Fb,
    Fint = Fint,
    Kt = Kt,
    Id_mm_h = Pd_mm * KA / 24,
    I_mm_h = Id_mm_h * Fint
  )


# 13. EXPORTAR RESULTADOS ====

write_csv(resultado, salida_idf)


# 14. FIGURA ====

g <- ggplot(resultado, aes(x = T_anios, y = I_mm_h)) +
  geom_line() +
  geom_point() +
  scale_x_log10(breaks = resultado$T_anios) +
  labs(
    title = "Intensidad de precipitación para duración t = tc",
    subtitle = paste0(
      "Norma 5.2-IC | Método Pd: ", metodo_Pd,
      " | tc = ", round(tc_min, 1), " min"
    ),
    x = "Periodo de retorno, T (años)",
    y = "I(T,tc) (mm/h)"
  ) +
  theme_gray()

ggsave(
  salida_figura,
  g,
  width = 8,
  height = 5,
  dpi = 300
)

print(g)


# 15. RESUMEN EN CONSOLA ====

cat("\n====================================================\n")
cat("SCRIPT 006 FINALIZADO - VERSION NORMA 5.2-IC\n")
cat("====================================================\n")

cat("\nArchivos de entrada:\n")
cat("Parametros 005:", archivo_param_005, "\n")
cat("Lluvias 003:", archivo_lluvias_diseno, "\n")
cat("Morfometria 004:", archivo_morfometria, "\n")

cat("\nMetodo Pd utilizado:\n")
cat(metodo_Pd, "\n")

cat("\nParametros morfometricos:\n")
cat("A =", round(A_km2, 4), "km2\n")
cat("Lc =", round(Lc_km, 4), "km\n")
cat("Jc =", round(Jc, 5), "\n")

cat("\nTiempo de concentracion:\n")
cat("Metodo =", metodo_tc, "\n")
cat("tc =", round(tc_h, 4), "h\n")
cat("tc =", round(tc_min, 2), "min\n")

cat("\nFactores:\n")
cat("KA =", round(KA, 4), "\n")
cat("I1/Id =", round(I1_Id, 3), "\n")
cat("Fa =", round(Fa, 4), "\n")
cat("Fint =", round(Fint, 4), "\n")
cat("Kt =", round(Kt, 4), "\n")

cat("\nTabla generada:\n")
cat(salida_idf, "\n")

cat("\nFigura generada:\n")
cat(salida_figura, "\n")

cat("\nPrimeras filas de resultado:\n")
print(
  resultado %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
)

cat("====================================================\n")
