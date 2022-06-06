# Se lee un dataframe desde archivo csv .
dir  <- "C:/Users/Dell PC/Desktop/IME-2022/Material-IME"
base <- "PEP 1 Datos.csv"
arch <- file.path(dir, base)

datos <- read.csv2(arch, fileEncoding = "UTF-8")


# ******************************************** ENUNCIADO ********************************#
# Usted ha sido designado asesor de Calcetín Con Rombos Man, quien le ha solicitado
# responder las siguientes preguntas usando para ello una muestra de 300 trabajadores
# obtenida con la semilla 679.


# Definir semilla 
set.seed(678)

# ****************************************** PREGUNTA 1.a ********************************#
# ¿Es igual la proporción de niños explotados en las plantas etiquetadoras de Salsacia
# y Conservia?

library(dplyr)

# Se formula la hipótesis.
# Sea "p1" y "p2" la proporción de niños explotados en las plantas etiquetadoras de
# Salsacia y Conservia, respectivamente, se tiene que:

# H0: p1 - p2 = 0 ;  No hay diferencias en la proporción de niños explotados en las plantas etiquetadoras
#                    de Salsacia Y Conervia. 
# HA: p1 - p2 != 0 ;  Las proporciones de niños explotados en las plantas etiquetadoras
#                     son diferentes plantas de Salsacia y Conservia.

# OJO: No se pudo obtener la muestra de tamaño 300, por lo que se procede a utilizar
# la poblacion.

# Se supondra que se obtuvieron 140 en Salsacia y 160 en Conservia

muestra <- sample_n(datos, 300)
# Niños explotados en Salsacia
salsacia <- datos %>% filter(Pais == "Salsacia")
n_salsacia <- 944
exitos_salsacia <- 140

# Niños explotados en Conservia
conservia <- datos %>% filter(Pais == "Conservia")
n_conservia <- 556
exitos_conservia <- 160


# Fijar valores conocidos (salsacia, conservia)
n <- c(c(n_salsacia, n_conservia ))
exitos <- c(c(exitos_salsacia, exitos_conservia))
alfa <- 0.05

# Como la diferencia en la hipótesis nula es cero, se propone utilizar el método de Wilson para la, 
# diferencia entre dos proporciones, por lo que no es necesario verificar condiciones.

prueba <- prop.test(exitos, n = n, alternative = "two.sided", conf.level = 1-alfa)

print(prueba)
# p = 1.081e-10

# Como p < alfa, se rechaza la hipótesis nula en favor de la hipótesis alternativa,
# por lo que se concluye con un 95% de confianza, que si hay diferencia en la proporción de niños
# explotados en las plantas etiquetadoras de Salsacia y Conservia.

# ****************************************** PREGUNTA 1.b ********************************#
# ¿Que poder estadistico tiene la prueba realizada?

# Objetivo: 
# Se nos pide encontrar el poder estadístico con un 95% de confianza, un tamaño de muestra
# para 300 niños de Salsacia y Conservia, siendo la diferencia en las proporciones de 0.

# Como se tienen dos proporciones en donde ambas muestras son de igual tamaño y diferencia 0,
# se procede a utilizar pwr.2p.test:

library(pwr)
library(Hmisc)

poder <- pwr.2p.test(h = ES.h(exitos_salsacia/n_salsacia,exitos_conservia/n_conservia), 300, alfa, NULL, "two.sided")
print(poder)
# poder = 0.9869957

# La prueba realizada tiene un poder estadistico de 0.9869957

# ****************************************** PREGUNTA 1.c ********************************#

# Objetivo: 
# Se nos pide encontrar el tamaño de la muestra con un 95% de confianza y 75% poder.

tamaño <- pwr.2p.test(h = ES.h(exitos_salsacia/n_salsacia,exitos_conservia/n_conservia), NULL, alfa, 0.75, "two.sided")
print(tamaño)
# 119
