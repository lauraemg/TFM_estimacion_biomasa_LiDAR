# Autor: LAura Méndez González
# Trabao Final de Máster
# Máster en Tecnologías de la Información Geográfica

# Descripción del script: 
# Carga de librerías, selección del espacio de trabajo, lectura de datos de biomasa
# Selección aleatoria de parcelas para el entrenamiento y la validación (líneas 33- 124)
# Cálculo de los coeficientes del modelo, resultados del entrenamiento y la validación para modelo incluyendo píxeles = 0 (líneas 128 - 219) 
# Cálculo de los coeficientes del modelo, resultados del entrenamiento y la validación para modelo excluyendo píxeles = 0 (líneas 221 - 324)
# Generación de cartografía (líneas 325 - 350)

# Se cargan las librerías
library(sp)
library(raster)
library(sf)
library(dplyr)
library(jpeg)
library(purrr)

# Se carga el espacio de trabajo
setwd ("C:/Users/Laura/Documents/MASTER TIG/TFM/script/datos")

# Datos de biomasa
parcelas <-st_read("mixto.shp") ### CON OUTLIERS (TODAS LAS PARCELAS)
parcelas_sinout <- subset(parcelas, AGB_Ruiz_P <= 300) ### OUTLIERS PARCELAS >= 300 Mg/ha

# Cargar los raster 
Cuelgamuros_ElEscorial <- raster("Cuelgamuros–ElEscorial_50.tif")
PelayosDeLaPresa <- raster("PelayosDeLaPresa_50.tif")
PNSierraGuadarrama_Rascafria <- raster("PNSierraGuadarrama-Rascafria_50.tif")
Almorox <- raster("Almorox_50.tif")

################################################################ HISTOGRAMA DE FRECUENCIAS Y SELECCION DE PARCELAS ALEATORIAS PARA ENTRENAMIENTO Y VALIDACION #####################################################

biomasa <- subset(parcelas_sinout, select = c(IDPLOTS, AGB_Ruiz_P))

# Obtener los IDPLOTS para cada grupo en grupo_1
IDPLOTS_0_50 <- subset(biomasa, AGB_Ruiz_P < 50, select = IDPLOTS)
IDPLOTS_50_100 <- subset(biomasa, AGB_Ruiz_P >= 50 & AGB_Ruiz_P < 100, select = IDPLOTS)
IDPLOTS_100_150 <- subset(biomasa, AGB_Ruiz_P >= 100 & AGB_Ruiz_P < 150, select = IDPLOTS)
IDPLOTS_150_200 <- subset(biomasa, AGB_Ruiz_P >= 150 & AGB_Ruiz_P < 200, select = IDPLOTS)
IDPLOTS_mas_200 <- subset(biomasa, AGB_Ruiz_P >= 200, select = IDPLOTS)


# Calcular las frecuencias de cada grupo
frecuencias <- sapply(list(IDPLOTS_0_50, IDPLOTS_50_100, IDPLOTS_100_150, IDPLOTS_150_200, IDPLOTS_mas_200), function(x) length(x$IDPLOTS))

# Mostrar las frecuencias de cada grupo
print(frecuencias)

# Etiquetas para cada intervalo
etiquetas_intervalos <- c("0-50", "50-100", "100-150", "150-200", ">= 200")

# Crear el histograma de barras
barplot(frecuencias, names.arg = etiquetas_intervalos, 
        main = "Histograma de Frecuencias por Rangos de Biomasa", 
        xlab = "Rangos de Biomasa (tn/ha)", ylab = "Frecuencia", col = "blue")

# parcelas por categoría entrenamiento
numeros_por_categoria <- c(3,9,10,4,5)

# Función para seleccionar valores aleatorios en cada categoría
valores_aleatorios_por_categoria <- function(data, n, etiqueta_intervalo) {
  if (n > nrow(data)) {
    return(data)
  } else {
    aleatorios <- data[sample(seq_len(nrow(data)), size = n), ]
    aleatorios$Etiqueta <- etiqueta_intervalo
    return(aleatorios)
  }
}

# Crear una lista para almacenar los valores aleatorios de cada categoría junto con la etiqueta
valores_aleatorios_etiquetas <- list()

# Definir las etiquetas de intervalos
etiquetas_intervalos <- c("Entre 0 y 50", "Entre 50 y 100", "Entre 100 y 150", "Entre 150 y 200", "Más de 200")

# Iterar a través de cada rango de biomasa y obtener valores aleatorios con etiqueta
for (i in 1:length(numeros_por_categoria)) {
  subconjunto <- switch(
    i,
    IDPLOTS_0_50,
    IDPLOTS_50_100,
    IDPLOTS_100_150,
    IDPLOTS_150_200,
    IDPLOTS_mas_200
  )
  valores_aleatorios_etiquetas[[i]] <- valores_aleatorios_por_categoria(subconjunto, numeros_por_categoria[i], etiquetas_intervalos[i])
  
  # Mostrar los valores aleatorios para cada categoría con su etiqueta de intervalo
  cat(paste("Valores aleatorios para la categoría '", etiquetas_intervalos[i], "':\n"))
  print(valores_aleatorios_etiquetas[[i]]$IDPLOTS)
  cat("\n")
}

# Extraer los valores aleatorios de 'parcelas_sinout' y mantenerlos separados en una tabla
valores_aleatorios_final <- purrr::map_dfr(valores_aleatorios_etiquetas, identity, .id = "Categoria")

# Mostrar los valores aleatorios con sus etiquetas de intervalos encontrados en 'parcelas_sinout'
print(valores_aleatorios_final)

# Filtrar las parcelas de parcelas_sinout cuyos IDPLOTS están en valores_aleatorios_final
parcelas_entrenamiento <- parcelas_sinout %>%
  filter(IDPLOTS %in% valores_aleatorios_final$IDPLOTS)

parcelas_validacion <- setdiff(parcelas_sinout, parcelas_entrenamiento)

# EXPORTAR PARCELAS DE ENTRENAMIENTO Y VALIDACION INCLUYENDO PIXELES = 0 (NO ES NECESARIO EJECUTARLO, SOLO SI SE DESEAN EXPORTAR)

# Ruta y nombre del archivo shapefile que  se desea guardar (parcelas entrenamiento)
#ruta_archivo_p_entrenamiento_mixto <- "C:/Users/Laura/Documents/MASTER TIG/TFM/script/datos/export_shapes/parcelas_entrenamiento_mixto.shp"
# Guardar la variable entrenamiento  en el archivo shapefile
#st_write(parcelas_entrenamiento, ruta_archivo_p_entrenamiento_mixto)

# Ruta y nombre del archivo shapefile que  se desea guardar(parcelas validacion)
#ruta_archivo_p_valdacion_mixto <- "C:/Users/Laura/Documents/MASTER TIG/TFM/script/datos/export_shapes/parcelas_validacion_mixto.shp"
# Guardar la variable validacion en el archivo shapefile
#st_write(parcelas_validacion, ruta_archivo_p_valdacion_mixto)

### si queremos recuperar las parcelas seleccinadas ###
#parcelas_entrenamiento <-st_read("C:/Users/Laura/Documents/MASTER TIG/TFM/script/datos/export_shapes/parcelas_entrenamiento_mixto.shp")
#parcelas_validacion <-st_read("C:/Users/Laura/Documents/MASTER TIG/TFM/script/datos/export_shapes/parcelas_validacion_mixto.shp")


##################################################################### -------------- AGB RUIZ-PEINADO ----------- ##############################################################################

################################################################################ ------------ MODELO MIXTO CON MEDIDAS EN SUELO (PIXELES = 0) ---------- ###################################################################


# Datos de biomasa
parcelas <-st_read("mixto.shp") ### CON OUTLIERS (TODAS LAS PARCELAS)
# eliminar parcelas con outliers
parcelas_sinout <- subset(parcelas, AGB_Ruiz_P <= 300) ### OUTLIERS PARCELAS >= 300 tn/ha

### para extraer parcelas determinadas
# Filtrar las parcelas con los IDPLOTS deseados en parcelas_sinout (ejecutar según el modelo el IDPLOTS deseado)
# PARCELAS SELECCIONADAS PARA LA VALIDACIÓN DE FORMA ALEATORIA PARA LOS MODELOS 6 Y 7 (EJECUTAR UNO U OTRO EN FUNCIÓN DE LOS RESULTADOS QUE SE DESEEN VISUALIZAR)
IDPLOTS_deseados <- c(3,5,10,15,17,19,22,23,27,33,38,43,81) # mod. 6
IDPLOTS_deseados <- c(3,7,8,12,15,16,21,27,28,29,34,40,42) # mod. 7

parcelas_deseadas <- subset(parcelas_sinout, IDPLOTS %in% IDPLOTS_deseados)
# Imprimir el resultado
print(parcelas_deseadas)

parcelas_validacion <- parcelas_deseadas
parcelas_entrenamiento <- setdiff(parcelas_sinout, parcelas_validacion)

# Ajustar el modelo
modelo_mixto_Ruiz_P_sinout <- lm(log(parcelas_entrenamiento$AGB_Ruiz_P) ~ log(parcelas_entrenamiento$CHM_mean), data = parcelas_entrenamiento)

# Obtener los parámetros c y d
c <- exp(coef(modelo_mixto_Ruiz_P_sinout)[1]) #coef log(c)
d <- coef(modelo_mixto_Ruiz_P_sinout)[2]

# Imprimir el SEE
SEE_sinout <- sigma(modelo_mixto_Ruiz_P_sinout)
print(SEE_sinout)

# Calculo CF
CF_modelo_mixto_Ruiz_P_sinout <- exp((SEE_sinout)^2/2)

## ---- ENTRENAMIENTO ---- ##

# Calculo AGB con CF entreamiento
AGB_modelo_mixto_Ruiz_P_CF_sinout_entrenamiento <- CF_modelo_mixto_Ruiz_P_sinout * c * (parcelas_entrenamiento$CHM_mean)^d

# Calculo del valor de determinacion
R2_entrenamiento <- cor(AGB_modelo_mixto_Ruiz_P_CF_sinout_entrenamiento,parcelas_entrenamiento$AGB_Ruiz_P )^2
print(R2_entrenamiento)

# Imprimir RMSE
RMSE_modelo_mixto_Ruiz_P_CF_sinout_entrenamiento <- sqrt(sum((AGB_modelo_mixto_Ruiz_P_CF_sinout_entrenamiento - parcelas_entrenamiento$AGB_Ruiz_P)^2/length(parcelas_entrenamiento$IDPLOTS)))
print(RMSE_modelo_mixto_Ruiz_P_CF_sinout_entrenamiento)

# Imprimir BIAS
BIAS_modelo_mixto_Ruiz_P_CF_sinout_entrenamiento <- sum((AGB_modelo_mixto_Ruiz_P_CF_sinout_entrenamiento - parcelas_entrenamiento$AGB_Ruiz_P)/length(parcelas_entrenamiento$IDPLOTS))
print(BIAS_modelo_mixto_Ruiz_P_CF_sinout_entrenamiento)

## ---- VALIDACION ---- ##

# Calculo AGB con CF validacion
AGB_modelo_mixto_Ruiz_P_CF_sinout_validacion <- CF_modelo_mixto_Ruiz_P_sinout * c * (parcelas_validacion$CHM_mean)^d

# Calculo del valor de determinacion

R2_validacion <- cor(AGB_modelo_mixto_Ruiz_P_CF_sinout_validacion,parcelas_validacion$AGB_Ruiz_P )^2
print(R2_validacion)

# Imprimir RMSE
RMSE_modelo_mixto_Ruiz_P_CF_sinout_validacion <- sqrt(sum((AGB_modelo_mixto_Ruiz_P_CF_sinout_validacion - parcelas_validacion$AGB_Ruiz_P)^2/length(parcelas_validacion$IDPLOTS)))
print(RMSE_modelo_mixto_Ruiz_P_CF_sinout_validacion)

# Imprimir BIAS
BIAS_modelo_mixto_Ruiz_P_CF_sinout_validacion <- sum((AGB_modelo_mixto_Ruiz_P_CF_sinout_validacion - parcelas_validacion$AGB_Ruiz_P)/length(parcelas_validacion$IDPLOTS))
print(BIAS_modelo_mixto_Ruiz_P_CF_sinout_validacion)

# GRAFICO DE DISPERSION #

# Obtener los valores máximos y mínimos de ambas variables
max_value <- max(c(parcelas_validacion$AGB_Ruiz_P, AGB_modelo_mixto_Ruiz_P_CF_sinout_validacion))
min_value <- min(c(parcelas_validacion$AGB_Ruiz_P, AGB_modelo_mixto_Ruiz_P_CF_sinout_validacion))

# Gráfico de dispersión
plot(parcelas_validacion$AGB_Ruiz_P, AGB_modelo_mixto_Ruiz_P_CF_sinout_validacion, pch = 16, col = "darkgreen",
     xlab = "Observado", ylab = "Predicho", main = "Validación Modelo Mixto - mod. 7",
     xlim = c(min_value, max_value), ylim = c(min_value, max_value))

# Ajustar un modelo lineal
modelo_lineal <- lm(parcelas_validacion$AGB_Ruiz_P ~ AGB_modelo_mixto_Ruiz_P_CF_sinout_validacion)

# Añadir la línea de tendencia
abline(modelo_lineal, col = "red", lty = 2)
# Añadir el subtítulo justo debajo del título principal
title(main = "Con valores de altura = 0", line = 0.5)

# Añadir la línea 1:1
abline(a = 0, b = 1, col = "black")


################################################################################ ------------ MODELO MIXTO ELIMINANDO MEDIDAS EN SUELO (PIXELES = 0) ---------- ###################################################################

### Preparación de los datos

#Leer shapefile 
parcelas_ND <-st_read("est_zona_mixto_ND.shp") 
# eliminar parcelas con outliers
parcelas_sinout_ND <- subset(parcelas_ND, AGB_Ruiz_P <= 300) ### OUTLIERS PARCELAS >= 300 tn/ha ; IDPLOTS -> 37, 46, 49

# Filtrar las parcelas de parcelas_sinout cuyos IDPLOTS están en valores_aleatorios_final
parcelas_validacion_ND <- parcelas_sinout_ND %>%
  filter(IDPLOTS %in% parcelas_validacion$IDPLOTS)

parcelas_entrenamiento_ND <- setdiff(parcelas_sinout_ND, parcelas_validacion_ND)

# EXPORTAR PARCELAS DE ENTRENAMIENTO Y VALIDACION ELIMINANDO PIXELES = 0 (NO ES NECESARIO EJECUTARLO, SOLO SI SE DESEAN EXPORTAR)

# Ruta y nombre del archivo shapefile que  se desea guardar (parcelas de entrenamiento)
#ruta_archivo_p_entrenamiento_mixto_ND <- "C:/Users/Laura/Documents/MASTER TIG/TFM/script/datos/export_shapes/parcelas_entrenamiento_mixto_ND.shp"

# Guardar la variable entrenamiento en el archivo shapefile
#st_write(parcelas_entrenamiento_ND, ruta_archivo_p_entrenamiento_mixto_ND)

# Ruta y nombre del archivo shapefile que  se desea guardar (parcelas de validacion)
#ruta_archivo_p_valdacion_mixto_ND <- "C:/Users/Laura/Documents/MASTER TIG/TFM/script/datos/export_shapes/parcelas_validacion_mixto_ND.shp"

# Guardar la variable validacion en el archivo shapefile
#st_write(parcelas_validacion_ND, ruta_archivo_p_valdacion_mixto_ND)

### si queremos recuperar las parcelas seleccinadas ###
#parcelas_entrenamiento_ND <-st_read("C:/Users/Laura/Documents/MASTER TIG/TFM/script/datos/export_shapes/parcelas_entrenamiento_mixto_ND.shp")
#parcelas_validacion_ND <-st_read("C:/Users/Laura/Documents/MASTER TIG/TFM/script/datos/export_shapes/parcelas_validacion_mixto_ND.shp")


# Ajustar el modelo de regresión lineal
modelo_mixto_Ruiz_P_sinout_ND <- lm(log(parcelas_entrenamiento_ND$AGB_Ruiz_P) ~ log(parcelas_entrenamiento_ND$CHM_mean), data = parcelas_entrenamiento_ND)

# Obtener los parámetros a y b
a <- exp(coef(modelo_mixto_Ruiz_P_sinout_ND)[1]) #coef log(a)
b <- coef(modelo_mixto_Ruiz_P_sinout_ND)[2]

# Imprimir el SEE
SEE_sinout_ND <- sigma(modelo_mixto_Ruiz_P_sinout_ND)
print(SEE_sinout_ND)

# Calculo CF
CF_modelo_mixto_Ruiz_P_sinout_ND <- exp((SEE_sinout_ND)^2/2)

## ---- ENTRENAMIENTO ---- ##

# Calculo AGB con CF entreamiento
AGB_modelo_mixto_Ruiz_P_CF_sinout_ND_entrenamiento <- CF_modelo_mixto_Ruiz_P_sinout_ND * c * (parcelas_entrenamiento_ND$CHM_mean)^d

# Calculo del valor de determinacion
R2_ND_entrenamiento <- cor(AGB_modelo_mixto_Ruiz_P_CF_sinout_ND_entrenamiento,parcelas_entrenamiento_ND$AGB_Ruiz_P )^2
print(R2_ND_entrenamiento)

# Imprimir RMSE
RMSE_modelo_mixto_Ruiz_P_CF_sinout_ND_entrenamiento <- sqrt(sum((AGB_modelo_mixto_Ruiz_P_CF_sinout_ND_entrenamiento - parcelas_entrenamiento_ND$AGB_Ruiz_P)^2/length(parcelas_entrenamiento_ND$IDPLOTS)))
print(RMSE_modelo_mixto_Ruiz_P_CF_sinout_ND_entrenamiento)

# Imprimir BIAS
BIAS_modelo_mixto_Ruiz_P_CF_sinout_ND_entrenamiento <- sum((AGB_modelo_mixto_Ruiz_P_CF_sinout_ND_entrenamiento - parcelas_entrenamiento_ND$AGB_Ruiz_P)/length(parcelas_entrenamiento_ND$IDPLOTS))
print(BIAS_modelo_mixto_Ruiz_P_CF_sinout_ND_entrenamiento)

## ---- VALIDACION ---- ##

# Calculo AGB con CF
AGB_modelo_mixto_Ruiz_P_CF_sinout_ND_validacion <- CF_modelo_mixto_Ruiz_P_sinout_ND * a * (parcelas_validacion_ND$CHM_mean)^b

# Calculo del valor de determinacion
R2_ND_validacion <-cor(AGB_modelo_mixto_Ruiz_P_CF_sinout_ND_validacion,parcelas_validacion_ND$AGB_Ruiz_P )^2
print(R2_ND_validacion)

# Imprimir RMSE
RMSE_modelo_mixto_Ruiz_P_CF_sinout_ND_validacion <- sqrt(sum((AGB_modelo_mixto_Ruiz_P_CF_sinout_ND_validacion - parcelas_validacion_ND$AGB_Ruiz_P)^2/length(parcelas_validacion_ND$IDPLOTS)))
print(RMSE_modelo_mixto_Ruiz_P_CF_sinout_ND_validacion)

# Imprimir BIAS
BIAS_modelo_mixto_Ruiz_P_CF_sinout_ND_validacion <- sum((AGB_modelo_mixto_Ruiz_P_CF_sinout_ND_validacion - parcelas_validacion_ND$AGB_Ruiz_P)/length(parcelas_validacion_ND$IDPLOTS))
print(BIAS_modelo_mixto_Ruiz_P_CF_sinout_ND_validacion)

# GRAFICO DE DISPERSION #

# Obtener los valores máximos y mínimos de ambas variables
max_value <- max(c(parcelas_validacion_ND$AGB_Ruiz_P, AGB_modelo_mixto_Ruiz_P_CF_sinout_ND_validacion))
min_value <- min(c(parcelas_validacion_ND$AGB_Ruiz_P, AGB_modelo_mixto_Ruiz_P_CF_sinout_ND_validacion))

# Gráfico de dispersión
plot(parcelas_validacion_ND$AGB_Ruiz_P, AGB_modelo_mixto_Ruiz_P_CF_sinout_ND_validacion, pch = 16, col = "darkgreen",
     xlab = "Observado", ylab = "Predicho", main = "Validación Modelo Mixto - mod. 7",
     xlim = c(min_value, max_value), ylim = c(min_value, max_value))

# Ajustar un modelo lineal
validacion_ND <- lm(parcelas_validacion_ND$AGB_Ruiz_P ~ AGB_modelo_mixto_Ruiz_P_CF_sinout_ND_validacion)

# Añadir la línea de tendencia
abline(validacion_ND, col = "red", lty = 2)
# Añadir el subtítulo justo debajo del título principal
title(main = "Eliminando valores de altura = 0", line = 0.5)

# Añadir la línea 1:1
abline(a = 0, b = 1, col = "black")

############################################################################## GENERACIÓN DE CARTOGRAFÍA #################################################################################################

# cartografía de biomasa transecto Cuelgamuros - El Escorial
mapa_AGB_Cuelgamuros_ElEscorial <- c * (Cuelgamuros_ElEscorial)^d
plot(mapa_AGB_Cuelgamuros_ElEscorial)
ruta_archivo_Cuelgamuros_ElEscorial <- "C:/Users/Laura/Documents/MASTER TIG/TFM/script/datos/Cuelgamuros_ElEscorial_mapa.tiff"
writeRaster(mapa_AGB_Cuelgamuros_ElEscorial, filename=ruta_archivo_Cuelgamuros_ElEscorial, format="GTiff")

# cartografía de biomasa transecto Almorox
mapa_AGB_Almorox <- c * (Almorox)^d
plot(mapa_AGB_Almorox)
ruta_archivo_Almorox <- "C:/Users/Laura/Documents/MASTER TIG/TFM/script/datos/Almorox_mapa.tiff"
writeRaster(mapa_AGB_Almorox, filename=ruta_archivo_Almorox, format="GTiff")

# cartografía de biomasa transecto PN Guadarrama - Rascafria
mapa_AGB_PNSierraGuadarama_Rascafria <- c * (PNSierraGuadarrama_Rascafria)^d
plot(mapa_AGB_PNSierraGuadarama_Rascafria)
ruta_archivo_PNSierraGuadarama_Rascafria <- "C:/Users/Laura/Documents/MASTER TIG/TFM/script/datos/PNSierraGuadarama_Rascafria_mapa.tiff"
writeRaster(mapa_AGB_PNSierraGuadarama_Rascafria, filename=ruta_archivo_PNSierraGuadarama_Rascafria, format="GTiff")

# cartografía de biomasa transecto Pelayos de la Presa
mapa_AGB_PelayosdelaPresa <- c * (PelayosDeLaPresa)^d
plot(mapa_AGB_PelayosdelaPresa)
ruta_archivo_PelayosDeLaPresa <- "C:/Users/Laura/Documents/MASTER TIG/TFM/script/datos/PelayosdelaPresa_mapa.tiff"
writeRaster(mapa_AGB_PelayosdelaPresa, filename=ruta_archivo_PelayosDeLaPresa, format="GTiff")

