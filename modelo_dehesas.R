library(sp)
library(raster)
library(sf)
library(dplyr)
library(jpeg)
# Se carga el espacio de trabajo
setwd ("C:/Users/Laura/Documents/MASTER TIG/TFM/script/datos")

# Cargar los raster 
Alcornocal <- raster("Alcornocal_50.tif")
Almorox <- raster("Almorox_50.tif")
Barrero_Cerrodavila_Cornocosillo <- raster("Barrero_Cerrodavila_Cornocosillo_50.tif")
Cuelgamuros_ElEscorial <- raster("Cuelgamuros–ElEscorial_50.tif")
DehesonPueblos <- raster("DehesonPueblos_50.tif")
PelayosDeLaPresa <- raster("PelayosDeLaPresa_50.tif")
PNSierraGuadarrama_Rascafria <- raster("PNSierraGuadarrama-Rascafria_50.tif")

################################################################################ ------------ MODELO DEHESAS CON MEDIDAS EN SUELO (PIXELES = 0) ---------- ###################################################################

##################################################################### -------------- AGB MONTERO ----------- ##############################################################################

#Leer shapefile 
parcelas <-st_read("dehesa.shp") ### CON OUTLIERS (TODAS LAS PARCELAS)

# Ajustar el modelo de regresión lineal
modelo_dehesa_Montero <- lm(log(parcelas$AGB_Monter) ~ log(parcelas$CHM_mean), data = parcelas)

# Obtener los parámetros a y b
a <- exp(coef(modelo_dehesa_Montero)[1]) #coef log(a)
b <- coef(modelo_dehesa_Montero)[2]

summary_modelo_dehesa_Montero <- summary(modelo_dehesa_Montero)
summary_modelo_dehesa_Montero

# Calculo AGB
AGB_modelo_dehesa_Montero <- a * (parcelas$CHM_mean)^b

# Imprimir el SEE
SEE <- sigma(modelo_dehesa_Montero)
print(SEE)

# Calculo CF

CF_modelo_dehesa_Montero <- ((SEE)^2)/2

# Calculo AGB con CF
AGB_modelo_dehesa_Montero_CF <- CF_modelo_dehesa_Montero * a * (parcelas$CHM_mean)^b


# Imprimir RMSE
RMSE_modelo_dehesa_Montero <- sqrt(sum((AGB_modelo_dehesa_Montero - parcelas$AGB_Monter)^2/length(parcelas$IDPLOTS)))
print(RMSE_modelo_dehesa_Montero)

RMSE_modelo_dehesa_Montero_CF <- sqrt(sum((AGB_modelo_dehesa_Montero_CF - parcelas$AGB_Monter)^2/length(parcelas$IDPLOTS)))
print(RMSE_modelo_dehesa_Montero_CF)

# Imprimir BIAS
BIAS_modelo_dehesa_Montero <- sum((AGB_modelo_dehesa_Montero - parcelas$AGB_Monter)/length(parcelas$IDPLOTS))
print(BIAS_modelo_dehesa_Montero)


# Gráfico de dispersión
plot(parcelas$AGB_Monter, AGB_modelo_dehesa_Montero, pch = 16, col = "blue", xlab = "Observado", ylab = "Predicho", main = "Gráfico de Validación")
abline(modelo_dehesa_Montero, col = "red")

# Imprimir AGB transectos
#AGB_Alcornocal <- a * (Alcornocal)^b
#plot(AGB_Alcornocal)

### ---------------------------------------------------------------------------- ELIMINANDO OUTLIERS --------------------------------------------------------------- ###

#Leer shapefile 
parcelas <-st_read("dehesa.shp") 
parcelas_sinout <- subset(parcelas, AGB_Monter <= 150) ### OUTLIERS PARCELAS >= 150 tn/ha ; IDPLOTS -> 45, 46, 49

# Ajustar el modelo de regresión lineal
modelo_dehesa_Montero_sinout <- lm(log(parcelas_sinout$AGB_Monter) ~ log(parcelas_sinout$CHM_mean), data = parcelas_sinout)

# Obtener los parámetros a y b
c <- exp(coef(modelo_dehesa_Montero_sinout)[1]) #coef log(a)
d <- coef(modelo_dehesa_Montero_sinout)[2]

summary_modelo_dehesa_Montero_sinout <- summary(modelo_dehesa_Montero_sinout)
summary_modelo_dehesa_Montero_sinout

# Calculo AGB
AGB_modelo_dehesa_Montero_sinout <- c * (parcelas_sinout$CHM_mean)^d

# Imprimir el SEE
SEE_sinout <- sigma(modelo_dehesa_Montero_sinout)
print(SEE_sinout)

# Calculo CF

CF_modelo_dehesa_Montero_sinout <- ((SEE_sinout)^2)/2

# Calculo AGB con CF
AGB_modelo_dehesa_Montero_CF_sinout <- CF_modelo_dehesa_Montero_sinout * c * (parcelas$CHM_mean)^d


#AGB <- a * (Alcornocal)^b
#plot(AGB)

# Imprimir RMSE
RMSE_modelo_dehesa_Montero_sinout <- sqrt(sum((AGB_modelo_dehesa_Montero_sinout - parcelas_sinout$AGB_Monter)^2/length(parcelas_sinout$IDPLOTS)))
print(RMSE_modelo_dehesa_Montero_sinout)

RMSE_modelo_dehesa_Montero_CF_sinout <- sqrt(sum((AGB_modelo_dehesa_Montero_CF_sinout - parcelas_sinout$AGB_Monter)^2/length(parcelas_sinout$IDPLOTS)))
print(RMSE_modelo_dehesa_Montero_CF)

# Imprimir BIAS
BIAS_modelo_dehesa_Montero_sinout <- sum((AGB_modelo_dehesa_Montero_sinout - parcelas_sinout$AGB_Monter)/length(parcelas_sinout$IDPLOTS))
print(BIAS_modelo_dehesa_Montero_sinout)


# Gráfico de dispersión
plot(parcelas_sinout$AGB_Monter, AGB_modelo_dehesa_Montero_sinout, pch = 16, col = "blue", xlab = "Observado", ylab = "Predicho", main = "Gráfico de Validación")
abline(modelo_dehesa_Montero_sinout, col = "red")




##################################################################### -------------- AGB RUIZ-PEINADO ----------- ##############################################################################

#Leer shapefile 
parcelas <-st_read("dehesa.shp") ### CON OUTLIERS (TODAS LAS PARCELAS)

# Ajustar el modelo de regresión lineal
modelo_dehesa_Ruiz_P <- lm(log(parcelas$AGB_Ruiz_P) ~ log(parcelas$CHM_mean), data = parcelas)

# Obtener los parámetros a y b
a <- exp(coef(modelo_dehesa_Ruiz_P)[1]) #coef log(a)
b <- coef(modelo_dehesa_Ruiz_P)[2]

summary_modelo_dehesa_Ruiz_P <- summary(modelo_dehesa_Ruiz_P)
summary_modelo_dehesa_Ruiz_P

# Calculo AGB
AGB_modelo_dehesa_Ruiz_P <- a * (parcelas$CHM_mean)^b

# Imprimir el SEE
SEE <- sigma(modelo_dehesa_Ruiz_P)
print(SEE)

# Calculo CF

CF_modelo_dehesa_Ruiz_P <- ((SEE)^2)/2

# Calculo AGB con CF
AGB_modelo_dehesa_Ruiz_P_CF <- CF_modelo_dehesa_Ruiz_P * a * (parcelas$CHM_mean)^b


#AGB <- a * (Alcornocal)^b
#plot(AGB)

# Imprimir RMSE
RMSE_modelo_dehesa_Ruiz_P <- sqrt(sum((AGB_modelo_dehesa_Ruiz_P - parcelas$AGB_Ruiz_P)^2/length(parcelas$IDPLOTS)))
print(RMSE_modelo_dehesa_Ruiz_P)

RMSE_modelo_dehesa_Ruiz_P_CF <- sqrt(sum((AGB_modelo_dehesa_Ruiz_P_CF - parcelas$AGB_Ruiz_P)^2/length(parcelas$IDPLOTS)))
print(RMSE_modelo_dehesa_Ruiz_P_CF)

# Imprimir BIAS
BIAS_modelo_dehesa_Ruiz_P <- sum((AGB_modelo_dehesa_Ruiz_P - parcelas$AGB_Ruiz_P)/length(parcelas$IDPLOTS))
print(BIAS_modelo_dehesa_Ruiz_P)


# Gráfico de dispersión
plot(parcelas$AGB_Ruiz_P, AGB_modelo_dehesa_Ruiz_P, pch = 16, col = "blue", xlab = "Observado", ylab = "Predicho", main = "Gráfico de Validación")
abline(modelo_dehesa_Ruiz_P, col = "red")

# Imprimir AGB transectos
#AGB_Alcornocal <- a * (Alcornocal)^b
#plot(AGB_Alcornocal)


### ---------------------------------------------------------------------------- ELIMINANDO OUTLIERS --------------------------------------------------------------- ###

#Leer shapefile 
parcelas <-st_read("dehesa.shp") 
parcelas_sinout <- subset(parcelas, AGB_Ruiz_P <= 150) ### OUTLIERS PARCELAS >= 150 tn/ha ; IDPLOTS -> 45, 46, 49

# Ajustar el modelo de regresión lineal
modelo_dehesa_Ruiz_P_sinout <- lm(log(parcelas_sinout$AGB_Ruiz_P) ~ log(parcelas_sinout$CHM_mean), data = parcelas_sinout)

# Obtener los parámetros c y d
c <- exp(coef(modelo_dehesa_Ruiz_P_sinout)[1]) #coef log(c)
d <- coef(modelo_dehesa_Ruiz_P_sinout)[2]

summary_modelo_dehesa_Ruiz_P_sinout <- summary(modelo_dehesa_Ruiz_P_sinout)
summary_modelo_dehesa_Ruiz_P_sinout

# Calculo AGB
AGB_modelo_dehesa_Ruiz_P_sinout <- c * (parcelas_sinout$CHM_mean)^d

# Imprimir el SEE
SEE_sinout <- sigma(modelo_dehesa_Ruiz_P_sinout)
print(SEE_sinout)

# Calculo CF

CF_modelo_dehesa_Ruiz_P_sinout <- ((SEE_sinout)^2)/2

# Calculo AGB con CF
AGB_modelo_dehesa_Ruiz_P_CF_sinout <- CF_modelo_dehesa_Ruiz_P_sinout * c * (parcelas_sinout$CHM_mean)^d


#AGB <- a * (Alcornocal)^b
#plot(AGB)

# Imprimir RMSE
RMSE_modelo_dehesa_Ruiz_P_sinout <- sqrt(sum((AGB_modelo_dehesa_Ruiz_P_sinout - parcelas_sinout$AGB_Ruiz_P)^2/length(parcelas_sinout$IDPLOTS)))
print(RMSE_modelo_dehesa_Ruiz_P_sinout)

RMSE_modelo_dehesa_Ruiz_P_CF_sinout <- sqrt(sum((AGB_modelo_dehesa_Ruiz_P_CF_sinout - parcelas_sinout$AGB_Ruiz_P)^2/length(parcelas_sinout$IDPLOTS)))
print(RMSE_modelo_dehesa_Ruiz_P_CF_sinout)

# Imprimir BIAS
BIAS_modelo_dehesa_Ruiz_P_sinout <- sum((AGB_modelo_dehesa_Ruiz_P_sinout - parcelas_sinout$AGB_Ruiz_P)/length(parcelas_sinout$IDPLOTS))
print(BIAS_modelo_dehesa_Ruiz_P_sinout)


# Gráfico de dispersión
plot(parcelas_sinout$AGB_Ruiz_P, AGB_modelo_dehesa_Ruiz_P_sinout, pch = 16, col = "blue", xlab = "Observado", ylab = "Predicho", main = "Gráfico de Validación")
abline(modelo_dehesa_Ruiz_P_sinout, col = "red")




################################################################################ ------------ MODELO GENERAL ELIMINANDO MEDIDAS EN SUELO (PIXELES = 0) ---------- ###################################################################

##################################################################### -------------- AGB MONTERO ----------- ##############################################################################

#Leer shapefile 
parcelas <-st_read("est_zona_parcelas_general_ND.shp") ### CON OUTLIERS (TODAS LAS PARCELAS)

# Ajustar el modelo de regresión lineal
modelo_gral_Montero <- lm(log(parcelas$AGB_Monter) ~ log(parcelas$CHM_mean), data = parcelas)

# Obtener los parámetros a y b
a <- exp(coef(modelo_gral_Montero)[1]) #coef log(a)
b <- coef(modelo_gral_Montero)[2]

summary_modelo_gral_Montero <- summary(modelo_gral_Montero)
summary_modelo_gral_Montero

# Calculo AGB
AGB_modelo_gral_Montero <- a * (parcelas$CHM_mean)^b

# Imprimir el SEE
SEE <- sigma(modelo_gral_Montero)
print(SEE)

# Calculo CF

CF_modelo_gral_Montero <- ((SEE)^2)/2

# Calculo AGB con CF
AGB_modelo_gral_Montero_CF <- CF_modelo_gral_Montero * a * (parcelas$CHM_mean)^b



# Imprimir RMSE
RMSE_modelo_gral_Montero <- sqrt(sum((AGB_modelo_gral_Montero - parcelas$AGB_Monter)^2/length(parcelas$IDPLOTS)))
print(RMSE_modelo_gral_Montero)

RMSE_modelo_gral_Montero_CF <- sqrt(sum((AGB_modelo_gral_Montero_CF - parcelas$AGB_Monter)^2/length(parcelas$IDPLOTS)))
print(RMSE_modelo_gral_Montero_CF)

# Imprimir BIAS
BIAS_modelo_gral_Montero <- sum((AGB_modelo_gral_Montero - parcelas$AGB_Monter)/length(parcelas$IDPLOTS))
print(BIAS_modelo_gral_Montero)


# Gráfico de dispersión
plot(parcelas$AGB_Monter, AGB_modelo_gral_Montero, pch = 16, col = "blue", xlab = "Observado", ylab = "Predicho", main = "Gráfico de Validación")
abline(modelo_gral_Montero, col = "red")

# Imprimir AGB transectos
#AGB_Alcornocal <- a * (Alcornocal)^b
#plot(AGB_Alcornocal)

### ---------------------------------------------------------------------------- ELIMINANDO OUTLIERS --------------------------------------------------------------- ###

#Leer shapefile 
parcelas <-st_read("est_zona_parcelas_general_ND.shp")
parcelas_sinout <- subset(parcelas, AGB_Monter <= 300) ### OUTLIERS PARCELAS >= 300 tn/ha ; IDPLOTS -> 37, 46, 49

# Ajustar el modelo de regresión lineal
modelo_gral_Montero_sinout <- lm(log(parcelas_sinout$AGB_Monter) ~ log(parcelas_sinout$CHM_mean), data = parcelas_sinout)

# Obtener los parámetros c y d
c <- exp(coef(modelo_gral_Montero_sinout)[1]) #coef log(c)
d <- coef(modelo_gral_Montero_sinout)[2]

summary_modelo_gral_Montero_sinout <- summary(modelo_gral_Montero_sinout)
summary_modelo_gral_Montero_sinout

# Calculo AGB
AGB_modelo_gral_Montero_sinout <- c * (parcelas_sinout$CHM_mean)^d

# Imprimir el SEE
SEE_sinout <- sigma(modelo_gral_Montero_sinout)
print(SEE_sinout)

# Calculo CF

CF_modelo_gral_Montero_sinout <- (SEE)^2/2

# Calculo AGB con CF
AGB_modelo_gral_Montero_CF_sinout <- CF_modelo_gral_Montero_sinout * a * (parcelas$CHM_mean)^b


#AGB <- a * (Alcornocal)^b
#plot(AGB)

# Imprimir RMSE
RMSE_modelo_gral_Montero_sinout <- sqrt(sum((AGB_modelo_gral_Montero_sinout - parcelas_sinout$AGB_Monter)^2/length(parcelas_sinout$IDPLOTS)))
print(RMSE_modelo_gral_Montero_sinout)

RMSE_modelo_gral_Montero_CF_sinout <- sqrt(sum((AGB_modelo_gral_Montero_CF_sinout - parcelas_sinout$AGB_Monter)^2/length(parcelas_sinout$IDPLOTS)))
print(RMSE_modelo_gral_Montero_CF_sinout)

# Imprimir BIAS
BIAS_modelo_gral_Montero_sinout <- sum((AGB_modelo_gral_Montero_sinout - parcelas_sinout$AGB_Monter)/length(parcelas_sinout$IDPLOTS))
print(BIAS_modelo_gral_Montero_sinout)


# Gráfico de dispersión
plot(parcelas_sinout$AGB_Monter, AGB_modelo_gral_Montero_sinout, pch = 16, col = "blue", xlab = "Observado", ylab = "Predicho", main = "Gráfico de Validación")
abline(modelo_gral_Montero_sinout, col = "red")




##################################################################### -------------- AGB RUIZ-PEINADO ----------- ##############################################################################

#Leer shapefile 
parcelas <-st_read("est_zona_parcelas_general_ND.shp") ### CON OUTLIERS (TODAS LAS PARCELAS)

# Ajustar el modelo de regresión lineal
modelo_gral_Ruiz_P <- lm(log(parcelas$AGB_Ruiz_P) ~ log(parcelas$CHM_mean), data = parcelas)

# Obtener los parámetros a y b
a <- exp(coef(modelo_gral_Ruiz_P)[1]) #coef log(a)
b <- coef(modelo_gral_Ruiz_P)[2]

summary_modelo_gral_Ruiz_P <- summary(modelo_gral_Ruiz_P)
summary_modelo_gral_Ruiz_P

# Calculo AGB
AGB_modelo_gral_Ruiz_P <- a * (parcelas$CHM_mean)^b

# Imprimir el SEE
SEE <- sigma(modelo_gral_Ruiz_P)
print(SEE)

# Calculo CF

CF_modelo_gral_Ruiz_P <- ((SEE)^2)/2

# Calculo AGB con CF
AGB_modelo_gral_Ruiz_P_CF <- CF_modelo_gral_Ruiz_P * a * (parcelas$CHM_mean)^b


#AGB <- a * (Alcornocal)^b
#plot(AGB)

# Imprimir RMSE
RMSE_modelo_gral_Ruiz_P <- sqrt(sum((AGB_modelo_gral_Ruiz_P - parcelas$AGB_Ruiz_P)^2/length(parcelas$IDPLOTS)))
print(RMSE_modelo_gral_Ruiz_P)

RMSE_modelo_gral_Ruiz_P_CF <- sqrt(sum((AGB_modelo_gral_Ruiz_P_CF - parcelas$AGB_Ruiz_P)^2/length(parcelas$IDPLOTS)))
print(RMSE_modelo_gral_Ruiz_P_CF)

# Imprimir BIAS
BIAS_modelo_gral_Ruiz_P <- sum((AGB_modelo_gral_Ruiz_P - parcelas$AGB_Ruiz_P)/length(parcelas$IDPLOTS))
print(BIAS_modelo_gral_Ruiz_P)


# Gráfico de dispersión
plot(parcelas$AGB_Ruiz_P, AGB_modelo_gral_Ruiz_P, pch = 16, col = "blue", xlab = "Observado", ylab = "Predicho", main = "Gráfico de Validación")
abline(modelo_gral_Ruiz_P, col = "red")

# Imprimir AGB transectos
#AGB_Alcornocal <- a * (Alcornocal)^b
#plot(AGB_Alcornocal)


### ---------------------------------------------------------------------------- ELIMINANDO OUTLIERS --------------------------------------------------------------- ###

#Leer shapefile 
parcelas <-st_read("est_zona_parcelas_general_ND.shp")
parcelas_sinout <- subset(parcelas, AGB_Ruiz_P <= 300) ### OUTLIERS PARCELAS >= 300 tn/ha ; IDPLOTS -> 37, 46, 49

# Ajustar el modelo de regresión lineal
modelo_gral_Ruiz_P_sinout <- lm(log(parcelas_sinout$AGB_Ruiz_P) ~ log(parcelas_sinout$CHM_mean), data = parcelas_sinout)

# Obtener los parámetros c y d
c <- exp(coef(modelo_gral_Ruiz_P_sinout)[1]) #coef log(a)
d <- coef(modelo_gral_Ruiz_P_sinout)[2]

summary_modelo_gral_Ruiz_P_sinout <- summary(modelo_gral_Ruiz_P_sinout)
summary_modelo_gral_Ruiz_P_sinout

# Calculo AGB
AGB_modelo_gral_Ruiz_P_sinout <- c * (parcelas_sinout$CHM_mean)^d

# Imprimir el SEE
SEE_sinout <- sigma(modelo_gral_Ruiz_P_sinout)
print(SEE_sinout)

# Calculo CF

CF_modelo_gral_Ruiz_P_sinout <- ((SEE)^2)/2

# Calculo AGB con CF
AGB_modelo_gral_Ruiz_P_CF_sinout <- CF_modelo_gral_Ruiz_P_sinout * a * (parcelas$CHM_mean)^b


#AGB <- a * (Alcornocal)^b
#plot(AGB)

# Imprimir RMSE
RMSE_modelo_gral_Ruiz_P_sinout <- sqrt(sum((AGB_modelo_gral_Ruiz_P_sinout - parcelas_sinout$AGB_Ruiz_P)^2/length(parcelas_sinout$IDPLOTS)))
print(RMSE_modelo_gral_Ruiz_P_sinout)

RMSE_modelo_gral_Ruiz_P_CF_sinout <- sqrt(sum((AGB_modelo_gral_Ruiz_P_CF_sinout - parcelas_sinout$AGB_Ruiz_P)^2/length(parcelas_sinout$IDPLOTS)))
print(RMSE_modelo_gral_Ruiz_P_CF)

# Imprimir BIAS
BIAS_modelo_gral_Ruiz_P_sinout <- sum((AGB_modelo_gral_Ruiz_P_sinout - parcelas_sinout$AGB_Ruiz_P)/length(parcelas_sinout$IDPLOTS))
print(BIAS_modelo_gral_Ruiz_P_sinout)


# Gráfico de dispersión
plot(parcelas_sinout$AGB_Ruiz_P, AGB_modelo_gral_Ruiz_P_sinout, pch = 16, col = "blue", xlab = "Observado", ylab = "Predicho", main = "Gráfico de Validación")
abline(modelo_gral_Ruiz_P_sinout, col = "red")