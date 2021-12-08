# ******************************************** PREGUNTA 1 ******************************************** 
# Un laboratorio que fabrica aspirina en Chile llena los frascos por peso en lugar de por conteo. 
# Cada frasco debiera contener 30 tabletas si es que se cumple el supuesto de que el peso promedio 
# de las tableta es de 5 gramos. Se obtuvo la siguiente muestra de 100 tabletas:
# ****************************************************************************************************

library(TeachingDemos)
library(ggpubr)
library(BSDA)

#texto contienen los datos de la muestra
texto <- "4.62 4.43 5.18 4.89 4.89 5.41 4.87 5.07 5.30 4.98 
          4.54 5.21 4.60 4.71 4.58 4.99 5.05 4.70 4.63 4.95 
          4.85 4.19 5.25 4.69 5.03 4.74 4.67 4.85 4.45 4.93 
          4.42 4.40 5.59 4.69 5.42 5.19 4.99 4.88 4.03 5.51 
          4.90 4.43 4.93 4.84 4.73 4.89 4.53 4.97 5.10 5.95 
          4.95 4.18 4.91 4.87 5.38 5.49 4.96 4.76 4.76 4.63 
          5.10 4.84 4.87 4.39 4.99 5.03 4.31 5.05 4.71 4.78 
          4.90 5.02 4.84 5.18 4.79 4.99 4.55 4.70 4.74 4.60 
          4.94 5.25 5.01 4.95 4.19 5.27 5.00 5.15 5.12 4.34 
          4.27 4.92 4.98 4.91 5.05 5.28 4.29 5.58 5.55 4.60"

#¿Proporciona esta información evidencia para concluir que la compañía no está llenando sus frascos como
#lo anuncia?
  
################################
######    Desarrollo    ########
################################

file <- textConnection(texto)
datos <- scan(file)

# Hipótesis en lenguaje coloquial: 
# H0: Las tabletas pesan en promedio 5 gramos cada una
# H1: Las tabletas pesan en promedio 5 gramos cada una

# Hipótesis en lenguaje formal:
# suponiendo x como el promedio de gramos que pesa una tableta de aspirina
# H0: µ  = 5
# H1: µ != 5

# Se obtienen valores importantes para la formula: desviación estándar y el valor nulo que corresponde a la hipótesis además del largo de la muestra.
desv_est <- sd(datos)
valor_nulo <- 5
n <- length(datos)

# Se estudia la normalidad usando la prueba de Shapiro.
normalidad <- shapiro.test(datos)
print(normalidad)
# Con p-value = 0.4564 siendo mayor que 0 se puede decir que la muestra está distribuida de forma normal.

# Se define un alfa de 10% o sea una confiabilidad del 90%
alfa <- 0.1

# Se calcula la media
media <- mean(datos)

#se calcula el z para tener un valor como referencia
Z <- (media - valor_nulo) / desv_est
cat("Z =", Z, "\n")
#Z = -0.3592016

#se hace la prueba z con z muy similar al teórico
prueba <- z.test(x=datos, mu = valor_nulo, sigma.x=desv_est, conf.level = 1-alfa)
print(prueba)
#p-value = 0.0003281 y z = -3.592

# Ya que p es menor a alfa, se rechaza la hipótesis nula en favor de la hipótesis alternativa
# y ya que z es negativo y un numero medianamente alto, se puede decir que el promedio del peso de las tabletas es inferior a 5 gramos.

# ******************************************** PREGUNTA 2 ******************************************** 
# Se sabe que la lactancia estimula una p?rdida de masa ?sea para proporcionar cantidades de calcio
# adecuadas para la producci?n de leche. Un estudio intent? determinar si madres adolescentes pod?an
# recuperar niveles m?s normales a pesar de no consumir suplementos (Amer. J. Clinical Nutr., 2004; 1322-
# 1326). El estudio obtuvo las siguientes medidas del contenido total de minerales en los huesos del cuerpo
# (en gramos) para una muestra de madres adolescentes tanto durante la lactancia (6-24 semanas
# postparto) y posterior a ella (12-30 semana postparto).
# ?Sugieren los datos que el contenido total de minerales en los huesos del cuerpo durante el posdestete
# excede el de la etapa de lactancia por m?s de 40 g?

# ****************************************************************************************************
library(dplyr)

#Hipotesis:
# h0: la diferencia de promedio de minerales en los huesos en la etapa de posdestete es igual a 40g respecto a la etapa de lactancia
# ha: la diferencia de promedio de minerales en los huesos en la etapa de posdestete excede por más de 40g a la etapa de lactancia
# h0: promedio min_pos - promedio min_lac = 40
# ha: promedio min_pos - promedio min_lac > 40

# Vector con los datos entregados
minerales_lactancia <- c(2825, 1843, 1928, 2549, 1924, 2621, 2114, 2175, 2541, 1628)
minerales_posdestete <- c(2895, 2006, 2126, 2885, 1942, 2626, 2164, 2184, 2627, 1750)

# Diferencia de medias entre las muestras:
diferencia <- minerales_posdestete - minerales_lactancia

# Nivel de significacion
alfa <- 0.05

#Verificar si la distribución se acerca a la normal:
normalidad <- shapiro.test(diferencia)
print(normalidad)
# Se obtiene que p-value = 0.1389 > alfa

valor_nulo <- 40


# Como se cumplieron las condiciones para la prueba t de studen (los valores tienen una distribución cercana a la normal, y las instancias
# fueron elegidas al aza), por esto podemos utilizar la prueba t para dos muestras pareadas.

prueba <- t.test(x = minerales_posdestete,
                 y = minerales_lactancia,
                 paired = TRUE,
                 alternative = "greater",
                 mu = valor_nulo,
                 conf.level = 1 - alfa)

print(prueba)
cat("Diferencia entre las medias =", diferencia,"\n")
# Intervalo de confianza [45.50299; Inf]
# diferencia = 105.7

# La diferencia entre las medias se encuentra dentro del intervalo de confianza.
# como p = 0.03823 < 0.05, se rechaza la hipótesis nula en favor de la hipótesis alternativa.

# En consecuencia, se puede afirmar con un 95% de confianza que los minerales en la etapa de posdestete
# son en promedio 40g mayor que la etapa de lactancia. 

# ******************************************** PREGUNTA 3 ******************************************** 
# La avicultura de carne es un negocio muy lucrativo, y cualquier método que ayude al rápido crecimiento de
# los pollitos es beneficioso, tanto para las avícolas como para los consumidores. En el paquete datasets
# de R están los datos (chickwts) de un experimento hecho para medir la efectividad de varios
# suplementos alimenticios en la tasa de crecimiento de las aves. Pollitos recién nacidos se separaron
# aleatoriamente en 6 grupos, y a cada grupo se le dio un suplemento distinto. Para productores de la 6ta
# región, es especialmente importante saber si existe diferencia en la efectividad entre el suplemento
# basado en harina animal (meatmeal) y el basado en soya (soybean).

# ****************************************************************************************************

library(dplyr)
library(datasets)
library(ggpubr)

# Cargar los datos.
datos <- chickwts

# Pesos de meatmeal.
meatmeal <- datos %>%filter(feed == "meatmeal")
meatmeal <- meatmeal[["weight"]]

# Pesos de soybean
soybean <- datos %>%filter(feed == "soybean")
soybean <- soybean[["weight"]]

# Formulación de la hipótesis:

# H0: no hay diferencia entre la efectividad de ambos suplementos
# Ha: existe diferencia entre la efectividad de ambos suplementos

# Se representará a meatmeal con "a" y soybean con "b".

# H0: µa = µb
# Ha: µa != µb

# Fijar nivel de significacion:
alfa <- 0.01 

# Las muestras son diferentes entre si, pues son diferentes suplementos 
#   administrados aleatoriamente a diferentes grupos.

# Verificar si las muestras se distribuyen de manera cercana a la normalidad.
normalidad_meatmeal <- shapiro.test(meatmeal)
print(normalidad_meatmeal)
# Se obtiene que p-value = 0.9612
normalidad_soybean  <- shapiro.test(soybean)
print(normalidad_soybean)
# Se obtiene que p-value = 0.5064

# ambos valores p son bastante más altos que el nivel de significación, por lo que ambas
#   muestras se distribuyen de forma aproximadamente normal.

# Como se cumplen las condiciones, podemos utilizar prueba t para dos muestras independientes.

prueba <- t.test(x = meatmeal,
                 y = soybean,
                 paired = FALSE,
                 alternative = "two.sided",
                 mu = 0,
                 conf.level = 1 - alfa
                 )
print(prueba)
# Intervalo de confianza [-38.96506; 99.92610]
# valor-p = 0.2252

# Calcular la diferencia entre las medias.
media_meatmeal <- mean(meatmeal)
media_soybean  <- mean(soybean)
diferencia <- media_meatmeal - media_soybean
cat("Diferencia entre las medias =", diferencia,"\n")
# diferencia = 30.48052

# La diferencia entre las medias se encuentra dentro del intervalo de confianza.
# como p = 0.2252 > 0.01, por lo que se falla al rechazar la hipótesis nula.

# En consecuencia, se puede afirmar con un 99% de confianza que pareciera no haber diferencia
# entre la efectividad de los suplementos alimenticios.


