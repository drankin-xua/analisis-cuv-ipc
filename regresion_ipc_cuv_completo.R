
# REGRESION IPC ELECTRICIDAD ~ COSTO VARIABLE UNITARIO (CUV)
# Análisis por promedios nacionales y por estratos




# 1. INSTALACION Y CARGA DE LIBRERIAS

install.packages("readxl")
install.packages("dplyr")
install.packages("lubridate")
install.packages("tseries")
install.packages("urca")
install.packages("lmtest")
install.packages("sandwich")
install.packages("car")
install.packages("strucchange")
install.packages("ggplot2")
install.packages("scales")
install.packages("patchwork")
install.packages("stringr")

library(readxl)
library(dplyr)
library(lubridate)
library(tseries)
library(urca)
library(lmtest)
library(sandwich)
library(car)
library(strucchange)
library(ggplot2)
library(scales)
library(patchwork)
library(stringr)

# 2. RUTAS DE ARCHIVOS

ruta1 <- "C:/Users/drankin/Documents/Base_limpia_auto_energía/BASE_TARIFAS_ENERGIA_CON_ANTIGUOS.xlsx"
ruta2 <- "C:/Users/drankin/Documents/Base_limpia_auto_energía/BASE_TARIFAS_ESTRATOS.xlsx"



# 3. CARGA Y LIMPIEZA DE DATOS


parsear_fecha_es <- function(x) {
  meses_es <- c(
    "ene"=1, "feb"=2, "mar"=3, "abr"=4, "may"=5, "jun"=6,
    "jul"=7, "ago"=8, "sep"=9, "oct"=10, "nov"=11, "dic"=12
  )
  
  x_clean <- tolower(str_trim(as.character(x)))
  x_clean <- chartr("áéíóú", "aeiou", x_clean)
  
  # Extrae parte texto (mes) y parte numérica (año de 2 o 4 dígitos)
  mes_str <- str_extract(x_clean, "^[a-z]+")
  anio_raw <- str_extract(x_clean, "\\d{2,4}$")
  
  # Convierte año de 2 dígitos a 4 dígitos (19 -> 2019, 25 -> 2025)
  anio <- ifelse(
    nchar(anio_raw) == 2,
    paste0("20", anio_raw),
    anio_raw
  )
  
  mes_num <- meses_es[mes_str]
  
  ifelse(
    !is.na(mes_num) & !is.na(anio),
    paste0(anio, "-", sprintf("%02d", mes_num)),
    NA_character_
  )
}

# IPC Electricidad
ipc <- read_excel(ruta1, sheet = "Base IPC-IPP")

ipc_clean <- ipc %>%
  select(Fecha, IPC = `IPC Electricidad`) %>%
  mutate(
    IPC         = as.numeric(IPC),
    fecha_merge = format(as.Date(Fecha), "%Y-%m")   # Fecha ya es tipo Date en Excel
  ) %>%
  filter(!is.na(IPC))

cat("Filas en ipc_clean:", nrow(ipc_clean), "\n")

# CUV Promedio Nacional
prom <- read_excel(ruta1, sheet = "PROMEDIOS_MENSUALES", skip = 1)

prom_clean <- prom %>%
  select(FECHA, CUV = CUV_Op_2) %>%
  mutate(
    fecha_merge = parsear_fecha_es(FECHA)
  ) %>%
  filter(!is.na(CUV), !is.na(fecha_merge))

cat("Filas en prom_clean:", nrow(prom_clean), "\n")

# CUV por Estratos
estratos <- read_excel(ruta2, sheet = "PROMEDIOS_MENSUALES", skip = 1)

estratos_clean <- estratos %>%
  select(FECHA, ESTRATO_1, ESTRATO_2, ESTRATO_3, ESTRATO_4, ESTRATO_5y6) %>%
  mutate(
    fecha_merge = parsear_fecha_es(FECHA)
  ) %>%
  filter(!is.na(ESTRATO_1), !is.na(fecha_merge))

cat("Filas en estratos_clean:", nrow(estratos_clean), "\n")



# 4. MERGE DE BASES

datos <- inner_join(ipc_clean, prom_clean, by = "fecha_merge")
cat("Filas merge promedios:", nrow(datos), "\n")

datos_est <- inner_join(ipc_clean, estratos_clean, by = "fecha_merge")
cat("Filas merge estratos:", nrow(datos_est), "\n")

# --- Diagnostico post-merge ---
cat("\n========================================\n")
cat("DIAGNOSTICO POST-MERGE\n")
cat("========================================\n")
cat("NAs en fecha_merge de prom_clean    :", sum(is.na(prom_clean$fecha_merge)), "\n")
cat("NAs en fecha_merge de estratos_clean:", sum(is.na(estratos_clean$fecha_merge)), "\n")
cat("Rango de fechas en datos            :", min(datos$fecha_merge), "a", max(datos$fecha_merge), "\n")
cat("Meses en IPC pero NO en promedios   :\n")
print(setdiff(ipc_clean$fecha_merge, prom_clean$fecha_merge))
cat("Meses en IPC pero NO en estratos    :\n")
print(setdiff(ipc_clean$fecha_merge, estratos_clean$fecha_merge))

# --- Cargar el nuevo promedio de estratos calculado en Python ---
cuv_nacional_est <- read_excel(ruta2, sheet = "PROMEDIOS_MENSUALES", skip = 1)

cuv_nacional_clean <- cuv_nacional_est %>%
  select(FECHA, CUV_EST_PROM = PROMEDIO_GENERAL) %>%
  mutate(
    fecha_merge = parsear_fecha_es(FECHA)
  ) %>%
  filter(!is.na(CUV_EST_PROM), !is.na(fecha_merge))

# UNIR a la base de datos principal
datos <- datos %>%
  left_join(cuv_nacional_clean %>% select(fecha_merge, CUV_EST_PROM), by = "fecha_merge")

cat("Filas en datos tras incluir CUV_EST_PROM:", nrow(datos), "\n")




# 5. VARIABLES TRANSFORMADAS


# Promedios nacionales
datos <- datos %>%
  arrange(fecha_merge) %>%
  mutate(
    ln_IPC      = log(IPC),
    ln_CUV      = log(CUV),
    ln_CUV_lag1 = lag(log(CUV), 1),
    CUV_lag1    = lag(CUV, 1),
    d_ln_IPC    = c(NA, diff(log(IPC))),
    d_ln_CUV    = c(NA, diff(log(CUV)))
  )

# Estratos
datos_est <- datos_est %>%
  arrange(fecha_merge) %>%
  mutate(
    ln_IPC    = log(IPC),
    E1_lag1   = lag(ESTRATO_1,   1),
    E2_lag1   = lag(ESTRATO_2,   1),
    E3_lag1   = lag(ESTRATO_3,   1),
    E4_lag1   = lag(ESTRATO_4,   1),
    E5y6_lag1 = lag(ESTRATO_5y6, 1)
  )

datos <- datos %>%
  arrange(fecha_merge) %>%
  mutate(
    ln_CUV_EST_PROM      = log(CUV_EST_PROM),
    ln_CUV_EST_PROM_lag1 = lag(log(CUV_EST_PROM), 1),
    CUV_EST_PROM_lag1    = lag(CUV_EST_PROM, 1),
    # Diferencias para análisis de corto plazo si fuera necesario
    d_ln_CUV_EST_PROM    = c(NA, diff(ln_CUV_EST_PROM))
  )



# PARTE I: ANALISIS CON PROMEDIOS NACIONALES



# 6. TEST ADF - ESTACIONARIEDAD

cat("\n========================================\n")
cat("TEST ADF - ESTACIONARIEDAD\n")
cat("========================================\n")

ipc_ts <- ts(datos$ln_IPC, frequency = 12)
cuv_ts <- ts(datos$ln_CUV, frequency = 12)

cat("\n--- ADF: ln(IPC) ---\n")
print(adf.test(ipc_ts))

cat("\n--- ADF: ln(CUV) ---\n")
print(adf.test(cuv_ts))



# 7. TEST DE JOHANSEN - COINTEGRACION

cat("\n========================================\n")
cat("TEST DE JOHANSEN - COINTEGRACION\n")
cat("========================================\n")

datos_coint   <- cbind(ipc_ts, cuv_ts)

johansen_test <- ca.jo(datos_coint, type = "trace", ecdet = "const", K = 2)
summary(johansen_test)



# 8. MODELOS PROMEDIOS NACIONALES

cat("\n========================================\n")
cat("MODELOS - PROMEDIOS NACIONALES\n")
cat("========================================\n")

modelo_loglin     <- lm(ln_IPC ~ CUV,         data = datos)
modelo_loglin_lag <- lm(ln_IPC ~ CUV_lag1,    data = datos)
modelo_loglog     <- lm(ln_IPC ~ ln_CUV,      data = datos)
modelo_loglog_lag <- lm(ln_IPC ~ ln_CUV_lag1, data = datos)

cat("\n--- MODELO 1: Log-Lin | ln(IPC) ~ CUV_t ---\n")
print(coeftest(modelo_loglin, vcov = NeweyWest(modelo_loglin, lag = 4, prewhite = FALSE)))

cat("\n--- MODELO 2: Log-Lin Lag1 | ln(IPC) ~ CUV_t-1 ---\n")
print(coeftest(modelo_loglin_lag, vcov = NeweyWest(modelo_loglin_lag, lag = 4, prewhite = FALSE)))

cat("\n--- MODELO 3: Log-Log | ln(IPC) ~ ln(CUV_t) ---\n")
print(coeftest(modelo_loglog, vcov = NeweyWest(modelo_loglog, lag = 4, prewhite = FALSE)))

cat("\n--- MODELO 4: Log-Log Lag1 | ln(IPC) ~ ln(CUV_t-1) ---\n")
print(coeftest(modelo_loglog_lag, vcov = NeweyWest(modelo_loglog_lag, lag = 4, prewhite = FALSE)))

cat("\n--- COMPARACION R2 ---\n")
cat("Modelo 1 - Log-Lin:          R2 =", round(summary(modelo_loglin)$r.squared,     4), "\n")
cat("Modelo 2 - Log-Lin Lag1:     R2 =", round(summary(modelo_loglin_lag)$r.squared, 4), "\n")
cat("Modelo 3 - Log-Log:          R2 =", round(summary(modelo_loglog)$r.squared,     4), "\n")
cat("Modelo 4 - Log-Log Lag1:     R2 =", round(summary(modelo_loglog_lag)$r.squared, 4), "\n")



# 9. DIAGNOSTICO COMPLETO - MODELOS NACIONALES

cat("\n========================================\n")
cat("DIAGNOSTICO COMPLETO - MODELOS NACIONALES\n")
cat("========================================\n")

modelos_nac <- list(
  list(m = modelo_loglin,     nombre = "Modelo 1 Log-Lin",
       formula_str = "ln_IPC ~ CUV",         datos = datos),
  list(m = modelo_loglin_lag, nombre = "Modelo 2 Log-Lin Lag1",
       formula_str = "ln_IPC ~ CUV_lag1",    datos = datos),
  list(m = modelo_loglog,     nombre = "Modelo 3 Log-Log",
       formula_str = "ln_IPC ~ ln_CUV",      datos = datos),
  list(m = modelo_loglog_lag, nombre = "Modelo 4 Log-Log Lag1",
       formula_str = "ln_IPC ~ ln_CUV_lag1", datos = datos)
)

for (item in modelos_nac) {
  mod <- item$m
  nom <- item$nombre
  
  cat("\n==========================================\n")
  cat(nom, "\n")
  cat("==========================================\n")
  
  cat("BG Autocorrelacion:\n")
  print(bgtest(mod, order = 3))
  
  cat("BP Heterocedasticidad:\n")
  print(bptest(mod))
  
  cat("Shapiro-Wilk (Normalidad):\n")
  print(shapiro.test(residuals(mod)))
  
  cat("Jarque-Bera (Normalidad):\n")
  print(jarque.bera.test(residuals(mod)))
  
  cat("RESET de Ramsey (Especificacion):\n")
  print(resettest(mod, power = 2:3, type = "fitted"))
  
  cat("Cook's Distance - Obs influyentes (D > 4/n):\n")
  cook <- cooks.distance(mod)
  print(which(cook > 4/length(cook)))
  
  cat("CUSUM (Estabilidad estructural):\n")
  cusum_test <- efp(as.formula(item$formula_str), data = item$datos, type = "OLS-CUSUM")
  print(sctest(cusum_test))
}

par(mfrow = c(2, 2))
plot(modelo_loglog, main = "Diagnostico Modelo Log-Log")



# 10. ECM - ERROR CORRECTION MODEL

cat("\n========================================\n")
cat("ECM - ERROR CORRECTION MODEL\n")
cat("========================================\n")

datos_ecm <- datos %>%
  mutate(
    ec_term = lag(residuals(modelo_loglog), 1)
  ) %>%
  filter(!is.na(d_ln_IPC) & !is.na(ec_term) & !is.na(d_ln_CUV))

cat("Observaciones usadas en ECM:", nrow(datos_ecm), "\n")

ecm <- lm(d_ln_IPC ~ ec_term + d_ln_CUV, data = datos_ecm)

cat("\n--- ECM: Resultados con errores HAC (lag=4) ---\n")
print(coeftest(ecm, vcov = NeweyWest(ecm, lag = 4, prewhite = FALSE)))
cat("R2 =", round(summary(ecm)$r.squared, 4), "\n")

cat("\n--- Interpretacion ECM ---\n")
cat("ec_term  : velocidad de ajuste al equilibrio de largo plazo\n")
cat("           ec_term negativo indica correccion hacia el equilibrio\n")
cat("d_ln_CUV : efecto de corto plazo del CUV sobre el IPC\n")

cat("\n--- Diagnostico ECM ---\n")
cat("BG Autocorrelacion:\n")
print(bgtest(ecm, order = 3))
cat("BP Heterocedasticidad:\n")
print(bptest(ecm))
cat("Shapiro-Wilk (Normalidad):\n")
print(shapiro.test(residuals(ecm)))



# PARTE II: ANALISIS POR ESTRATOS (AGRUPADOS)

cat("\n========================================\n")
cat("MODELOS - POR ESTRATOS (todas las variables)\n")
cat("ADVERTENCIA: Alta multicolinealidad esperada\n")
cat("========================================\n")

modelo_A <- lm(ln_IPC ~ ESTRATO_1 + ESTRATO_2 + ESTRATO_3 + ESTRATO_4 + ESTRATO_5y6,
               data = datos_est)
cat("\n--- MODELO A: Log-Lin Contemporaneo por Estratos ---\n")
print(coeftest(modelo_A, vcov = vcovHAC(modelo_A)))
cat("R2 =", round(summary(modelo_A)$r.squared, 4), "\n")

modelo_B <- lm(ln_IPC ~ E1_lag1 + E2_lag1 + E3_lag1 + E4_lag1 + E5y6_lag1,
               data = datos_est)
cat("\n--- MODELO B: Log-Lin Rezagado 1 mes por Estratos ---\n")
print(coeftest(modelo_B, vcov = vcovHAC(modelo_B)))
cat("R2 =", round(summary(modelo_B)$r.squared, 4), "\n")



# DIAGNOSTICO COMPLETO - MODELOS A Y B

cat("\n========================================\n")
cat("DIAGNOSTICO COMPLETO - MODELOS A Y B\n")
cat("========================================\n")

modelos_est <- list(
  list(m = modelo_A, nombre = "Modelo A (Contemporaneo)",
       formula_str = "ln_IPC ~ ESTRATO_1 + ESTRATO_2 + ESTRATO_3 + ESTRATO_4 + ESTRATO_5y6"),
  list(m = modelo_B, nombre = "Modelo B (Rezagado)",
       formula_str = "ln_IPC ~ E1_lag1 + E2_lag1 + E3_lag1 + E4_lag1 + E5y6_lag1")
)

for (item in modelos_est) {
  mod <- item$m
  nom <- item$nombre
  
  cat("\n==========================================\n")
  cat(nom, "\n")
  cat("==========================================\n")
  
  cat("BG Autocorrelacion:\n")
  print(bgtest(mod, order = 3))
  
  cat("BP Heterocedasticidad:\n")
  print(bptest(mod))
  
  cat("VIF (Multicolinealidad):\n")
  print(vif(mod))
  
  cat("Shapiro-Wilk (Normalidad):\n")
  print(shapiro.test(residuals(mod)))
  
  cat("Jarque-Bera (Normalidad):\n")
  print(jarque.bera.test(residuals(mod)))
  
  cat("RESET de Ramsey (Especificacion):\n")
  print(resettest(mod, power = 2:3, type = "fitted"))
  
  cat("Cook's Distance - Obs influyentes (D > 4/n):\n")
  cook <- cooks.distance(mod)
  print(which(cook > 4/length(cook)))
  
  cat("CUSUM (Estabilidad estructural):\n")
  cusum_test <- efp(as.formula(item$formula_str), data = datos_est, type = "OLS-CUSUM")
  print(sctest(cusum_test))
}

par(mfrow = c(2, 2))
plot(modelo_A, main = "Diagnostico Modelo A - Contemporaneo")



# PARTE III: REGRESIONES INDIVIDUALES POR ESTRATO

cat("\n========================================\n")
cat("REGRESIONES INDIVIDUALES POR ESTRATO\n")
cat("========================================\n")

estratos_vars <- list(
  E1   = list(cont = "ESTRATO_1",   lag = "E1_lag1"),
  E2   = list(cont = "ESTRATO_2",   lag = "E2_lag1"),
  E3   = list(cont = "ESTRATO_3",   lag = "E3_lag1"),
  E4   = list(cont = "ESTRATO_4",   lag = "E4_lag1"),
  E5y6 = list(cont = "ESTRATO_5y6", lag = "E5y6_lag1")
)

for (nombre in names(estratos_vars)) {
  
  var_cont <- estratos_vars[[nombre]]$cont
  var_lag  <- estratos_vars[[nombre]]$lag
  
  mod_cont <- lm(as.formula(paste("ln_IPC ~", var_cont)), data = datos_est)
  mod_lag  <- lm(as.formula(paste("ln_IPC ~", var_lag)),  data = datos_est)
  
  cat("\n==========================================\n")
  cat("ESTRATO", nombre, "\n")
  cat("==========================================\n")
  
  for (mod_info in list(list(m = mod_cont, nombre = "Contemporaneo",  var = var_cont),
                        list(m = mod_lag,  nombre = "Rezagado 1 mes", var = var_lag))) {
    
    mod <- mod_info$m
    nom <- mod_info$nombre
    var <- mod_info$var
    
    cat("\n---", nom, "| Coeficientes (HAC) ---\n")
    print(coeftest(mod, vcov = NeweyWest(mod, lag = 4, prewhite = FALSE)))
    cat("R2 =", round(summary(mod)$r.squared, 4), "\n")
    
    cat("BG Autocorrelacion:\n")
    print(bgtest(mod, order = 3))
    
    cat("BP Heterocedasticidad:\n")
    print(bptest(mod))
    
    cat("Shapiro-Wilk (Normalidad):\n")
    print(shapiro.test(residuals(mod)))
    
    cat("Jarque-Bera (Normalidad):\n")
    print(jarque.bera.test(residuals(mod)))
    
    cat("RESET de Ramsey (Especificacion):\n")
    print(resettest(mod, power = 2:3, type = "fitted"))
    
    cat("Cook's Distance - Obs influyentes (D > 4/n):\n")
    cook <- cooks.distance(mod)
    print(which(cook > 4/length(cook)))
    
    cat("CUSUM (Estabilidad estructural):\n")
    formula_cusum <- as.formula(paste("ln_IPC ~", var))
    cusum_test <- efp(formula_cusum, data = datos_est, type = "OLS-CUSUM")
    print(sctest(cusum_test))
  }
}



# PARTE IV: REGRESIONES INDIVIDUALES POR ESTRATO — LOG-LOG

cat("\n========================================\n")
cat("REGRESIONES INDIVIDUALES POR ESTRATO — LOG-LOG\n")
cat("Interpretación: coeficiente = elasticidad (% cambio IPC / % cambio CUV)\n")
cat("========================================\n")

# Variables log de cada estrato
datos_est <- datos_est %>%
  mutate(
    ln_E1     = log(ESTRATO_1),
    ln_E2     = log(ESTRATO_2),
    ln_E3     = log(ESTRATO_3),
    ln_E4     = log(ESTRATO_4),
    ln_E5y6   = log(ESTRATO_5y6),
    ln_E1_lag1   = lag(log(ESTRATO_1),   1),
    ln_E2_lag1   = lag(log(ESTRATO_2),   1),
    ln_E3_lag1   = lag(log(ESTRATO_3),   1),
    ln_E4_lag1   = lag(log(ESTRATO_4),   1),
    ln_E5y6_lag1 = lag(log(ESTRATO_5y6), 1)
  )

estratos_vars_ll <- list(
  E1   = list(cont = "ln_E1",   lag = "ln_E1_lag1"),
  E2   = list(cont = "ln_E2",   lag = "ln_E2_lag1"),
  E3   = list(cont = "ln_E3",   lag = "ln_E3_lag1"),
  E4   = list(cont = "ln_E4",   lag = "ln_E4_lag1"),
  E5y6 = list(cont = "ln_E5y6", lag = "ln_E5y6_lag1")
)

for (nombre in names(estratos_vars_ll)) {
  
  var_cont <- estratos_vars_ll[[nombre]]$cont
  var_lag  <- estratos_vars_ll[[nombre]]$lag
  
  mod_cont <- lm(as.formula(paste("ln_IPC ~", var_cont)), data = datos_est)
  mod_lag  <- lm(as.formula(paste("ln_IPC ~", var_lag)),  data = datos_est)
  
  cat("\n==========================================\n")
  cat("ESTRATO", nombre, "— LOG-LOG\n")
  cat("==========================================\n")
  
  for (mod_info in list(list(m = mod_cont, nombre = "Contemporaneo",  var = var_cont),
                        list(m = mod_lag,  nombre = "Rezagado 1 mes", var = var_lag))) {
    
    mod <- mod_info$m
    nom <- mod_info$nombre
    var <- mod_info$var
    
    cat("\n---", nom, "| Coeficientes (HAC) ---\n")
    print(coeftest(mod, vcov = NeweyWest(mod, lag = 4, prewhite = FALSE)))
    cat("R2 =", round(summary(mod)$r.squared, 4), "\n")
    
    cat("BG Autocorrelacion:\n")
    print(bgtest(mod, order = 3))
    
    cat("BP Heterocedasticidad:\n")
    print(bptest(mod))
    
    cat("Shapiro-Wilk (Normalidad):\n")
    print(shapiro.test(residuals(mod)))
    
    cat("Jarque-Bera (Normalidad):\n")
    print(jarque.bera.test(residuals(mod)))
    
    cat("RESET de Ramsey (Especificacion):\n")
    print(resettest(mod, power = 2:3, type = "fitted"))
    
    cat("Cook's Distance - Obs influyentes (D > 4/n):\n")
    cook <- cooks.distance(mod)
    print(which(cook > 4/length(cook)))
    
    cat("CUSUM (Estabilidad estructural):\n")
    formula_cusum <- as.formula(paste("ln_IPC ~", var))
    cusum_test <- efp(formula_cusum, data = datos_est, type = "OLS-CUSUM")
    print(sctest(cusum_test))
  }
}


# PARTE I.B: MODELOS CUV PROMEDIO ESTRATOS (LOG-LOG Y LOG-LIN)


# 1. ESTIMACIÓN DE LOS MODELOS (Primero creamos los objetos)
# ------------------------------------------------------------

# Modelos Log-Log (Elasticidades)
mod_est_cont <- lm(ln_IPC ~ ln_CUV_EST_PROM,      data = datos)
mod_est_lag  <- lm(ln_IPC ~ ln_CUV_EST_PROM_lag1, data = datos)

# Modelos Log-Lin (Semi-elasticidades)
mod_est_ll_cont <- lm(ln_IPC ~ CUV_EST_PROM,      data = datos)
mod_est_ll_lag  <- lm(ln_IPC ~ CUV_EST_PROM_lag1, data = datos)


# 2. PROCESAMIENTO Y DIAGNÓSTICO EN BLOQUE
# ------------------------------------------------------------
cat("\n================================================\n")
cat("MODELOS NACIONALES: CONTEMPORÁNEOS VS REZAGADOS\n")
cat("================================================\n")

# Ahora sí los metemos en la lista
modelos_est_nacional <- list(
  list(m = mod_est_cont,    nom = "LOG-LOG CONTEMPORÁNEO", form = "ln_IPC ~ ln_CUV_EST_PROM"),
  list(m = mod_est_lag,     nom = "LOG-LOG REZAGADO (1M)", form = "ln_IPC ~ ln_CUV_EST_PROM_lag1"),
  list(m = mod_est_ll_cont, nom = "LOG-LIN CONTEMPORÁNEO", form = "ln_IPC ~ CUV_EST_PROM"),
  list(m = mod_est_ll_lag,  nom = "LOG-LIN REZAGADO (1M)", form = "ln_IPC ~ CUV_EST_PROM_lag1")
)

for (item in modelos_est_nacional) {
  m <- item$m
  
  cat("\n******************************************\n")
  cat(item$nom, "\n")
  cat("******************************************\n")
  
  # Coeficientes con Errores HAC (Newey-West)
  cat("--- Coeficientes (HAC) ---\n")
  print(coeftest(m, vcov = NeweyWest(m, lag = 4)))
  
  # Bondad de ajuste
  cat("R2:", round(summary(m)$r.squared, 4), "\n")
  
  # Bloque resumido de Diagnóstico
  cat("\n--- Diagnóstico Rápido (p-values) ---\n")
  
  # Autocorrelación
  bg_p <- bgtest(m, order = 3)$p.value
  cat("BG Autocorrelación: ", round(bg_p, 5), ifelse(bg_p < 0.05, " (Problema)", " (OK)"), "\n")
  
  # Heterocedasticidad
  bp_p <- bptest(m)$p.value
  cat("BP Heterocedasticidad:", round(bp_p, 5), ifelse(bp_p < 0.05, " (Problema)", " (OK)"), "\n")
  
  # Estabilidad (CUSUM)
  cusum_p <- sctest(efp(as.formula(item$form), data = datos, type = "OLS-CUSUM"))$p.value
  cat("CUSUM Estabilidad:   ", round(cusum_p, 5), "\n")
  
  # Cointegración (ADF Residuos)
  adf_p <- adf.test(residuals(m))$p.value
  cat("ADF Cointegración:   ", round(adf_p, 5), ifelse(adf_p < 0.05, " (Cointegrados)", " (No cointegrados)"), "\n")
}



# GRAFICAS


# Preparar datos con fecha real
datos_graf <- datos %>%
  mutate(
    fecha   = as.Date(paste0(fecha_merge, "-01")),
    var_ipc = (IPC / lag(IPC) - 1) * 100,
    var_cuv = (CUV / lag(CUV) - 1) * 100
  )

# Paleta de colores
color_ipc <- "#1a6faf"
color_cuv <- "#c0392b"


# GRAFICA 1: Serie IPC de Electricidad (niveles)

g1 <- ggplot(datos_graf, aes(x = fecha, y = IPC)) +
  geom_line(color = color_ipc, linewidth = 1) +
  geom_point(color = color_ipc, size = 1.5) +
  labs(
    title    = "IPC de Electricidad",
    subtitle = "Base diciembre 2018 = 100",
    x        = NULL,
    y        = "Índice",
    caption  = "Fuente: DANE"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "gray40"),
    panel.grid.minor = element_blank()
  )

print(g1)


# GRAFICA 2: Serie CUV Promedio Nacional (niveles)

g2 <- ggplot(datos_graf, aes(x = fecha, y = CUV)) +
  geom_line(color = color_cuv, linewidth = 1) +
  geom_point(color = color_cuv, size = 1.5) +
  labs(
    title    = "Costo Variable Unitario (CUV) Promedio Nacional",
    subtitle = "En $/kWh",
    x        = NULL,
    y        = "$/kWh",
    caption  = "Fuente: CREG / operadores del mercado"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "gray40"),
    panel.grid.minor = element_blank()
  )

print(g2)


# GRAFICA 3: Variacion porcentual mensual IPC

g3 <- ggplot(datos_graf %>% filter(!is.na(var_ipc)),
             aes(x = fecha, y = var_ipc)) +
  geom_col(aes(fill = var_ipc > 0), show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = color_ipc, "FALSE" = "#e74c3c")) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  labs(
    title   = "Variación Porcentual Mensual del IPC de Electricidad",
    x       = NULL,
    y       = "Variación (%)",
    caption = "Fuente: DANE"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(g3)


# GRAFICA 4: Variacion porcentual mensual CUV

g4 <- ggplot(datos_graf %>% filter(!is.na(var_cuv)),
             aes(x = fecha, y = var_cuv)) +
  geom_col(aes(fill = var_cuv > 0), show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = color_cuv, "FALSE" = "#1a4f8a")) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  labs(
    title   = "Variación Porcentual Mensual del CUV Promedio Nacional",
    x       = NULL,
    y       = "Variación (%)",
    caption = "Fuente: CREG / operadores del mercado"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(g4)


# GRAFICA 5: IPC y CUV en niveles (doble eje)

escala_factor <- max(datos_graf$IPC, na.rm = TRUE) / max(datos_graf$CUV, na.rm = TRUE)

tema_academico <- theme_classic(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13, hjust = 0),
    plot.caption       = element_text(color = "gray40", size = 9, hjust = 0),
    axis.title         = element_text(size = 11),
    axis.line          = element_line(color = "black", linewidth = 0.4),
    axis.ticks         = element_line(color = "black", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom",
    legend.title       = element_blank(),
    legend.text        = element_text(size = 10),
    axis.title.y.left  = element_text(color = "#1a4f8a"),
    axis.title.y.right = element_text(color = "#8b0000")
  )

g5 <- ggplot(datos_graf, aes(x = fecha)) +
  geom_line(aes(y = IPC, color = "IPC de Electricidad"),
            linewidth = 0.9) +
  geom_line(aes(y = CUV * escala_factor, color = "CUV Promedio Nacional"),
            linewidth = 0.9, linetype = "dashed") +
  scale_y_continuous(
    name     = "IPC de Electricidad (dic-2018 = 100)",
    sec.axis = sec_axis(~ . / escala_factor, name = "CUV ($/kWh)")
  ) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "6 months") +
  scale_color_manual(
    values = c("IPC de Electricidad"   = "#1a4f8a",
               "CUV Promedio Nacional" = "#8b0000")
  ) +
  labs(
    title   = "IPC de Electricidad y Costo Variable Unitario Promedio Nacional",
    x       = NULL,
    caption = "Fuente: DANE y CREG"
  ) +
  tema_academico

print(g5)


# GRAFICA 6: Variacion porcentual (panel doble apilado)

g6a <- ggplot(datos_graf %>% filter(!is.na(var_ipc)),
              aes(x = fecha, y = var_ipc)) +
  geom_col(aes(fill = var_ipc >= 0), width = 25, show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  scale_fill_manual(values = c("TRUE" = "#1a4f8a", "FALSE" = "#8b0000")) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "6 months") +
  labs(
    title = "Variación Porcentual Mensual",
    x     = NULL,
    y     = "IPC (%)"
  ) +
  tema_academico +
  theme(plot.title = element_text(face = "bold", size = 13))

g6b <- ggplot(datos_graf %>% filter(!is.na(var_cuv)),
              aes(x = fecha, y = var_cuv)) +
  geom_col(aes(fill = var_cuv >= 0), width = 25, show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  scale_fill_manual(values = c("TRUE" = "#8b0000", "FALSE" = "#1a4f8a")) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "6 months") +
  labs(
    x       = NULL,
    y       = "CUV (%)",
    caption = "Fuente: DANE y CREG"
  ) +
  tema_academico

g6 <- g6a / g6b
print(g6)






