library(dplyr)

# Se crea una tabla con los datos para entender mejor el problema.
especialidad <- c("Pediatría", "Obstetricia", "Dermatología", "Psiquiatría", "Medicina Interna", 
                  "Oncología", "Neurología", "Anestesiología", "Radiología")
mujeres <- c(54, 71, 35, 30, 45, 44, 56, 21, 17)

hombres <- c(52, 66, 41, 42, 65, 62, 88, 40, 35)

tabla <- data.frame(especialidad, mujeres, hombres)
print(tabla)

# ******************************************** PREGUNTA 1 ********************************#
# Estudios previos habían determinado que la proporción de autoras en la especialidad     #
# de obstetricia era de 59%. ¿Respaldan estos datos tal estimación?                       #
# ****************************************************************************************#

# Se formula la hipótesis.
# Sea "p" la proporción de autoras en la especialidad de obstetricia, considerando
# el valor hipotético p0 = 0.59, se tiene que:

# H0: p = p0  ;   La proporción de autoras en la especialidad de obstetricia es del 59%
# HA: p != p0 ;   La proporción de autoras en la especialidad de obstetricia es distinta del 59%

# El total de muestras sería el número de autoras más autores en la especialidad de obstetricia.
obstetricia = tabla %>% filter(especialidad == "Obstetricia")
n_obstetricia = obstetricia[["mujeres"]] + obstetricia[["hombres"]]
# La cantidad de éxito en la muestra sería el número de autoras en obstetricia.
exitos_obstetricia = obstetricia[["mujeres"]]
alfa = 0.05
valor_nulo = 0.59

# Se propone utilizar el método de Wilson para una proporción, 
# por lo que no es necesario verificar condiciones.

prueba <- prop.test(exitos_obstetricia, n = n_obstetricia, p = valor_nulo,
                    alternative = "two.sided", conf.level = 1-alfa)

print(prueba)
# p = 0.5182

# Como p > alfa, se falla en rechazar la hipótesis nula en favor de la hipótesis alternativa,
# por lo que se concluye con un 95% de confianza, que la proporción de mujeres en la especialidad de 
# obstetricia es del 59%.

# ******************************************** PREGUNTA 2 ********************************#
# Según estos datos, ¿es igual la proporción de autoras en las áreas de neurología y      #
# obstetricia?                                                                            #
# ****************************************************************************************#

# Se formula la hipótesis.
# Sea "p1" y "p2" la proporción de autoras en la especialidad de neurología y obstetricia,
# respectivamente, se tiene que:

# H0: p1 - p2 = 0 ;  No hay diferencias en la proporción de autoras en la especialidad de neurología y obstetricia
# HA: p1 - p2 != 0 ;  Las proporciones de autoras son diferentes para la especialidad de neurología y obstetricia


neurologia = tabla %>% filter(especialidad == "Neurología") 
n_neurologia = neurologia[["mujeres"]] + neurologia[["hombres"]]
exitos_neurologia <- neurologia[["mujeres"]]

# Fijar valores conocidos (neurología, obstetricia)
n <- c(c(n_neurologia, n_obstetricia ))
exitos <- c(c(exitos_neurologia, exitos_obstetricia))
alfa <- 0.05

# Como la diferencia en la hipótesis nula es cero, se propone utilizar el método de Wilson para la, 
# diferencia entre dos proporciones, por lo que no es necesario verificar condiciones.

prueba2 <- prop.test(exitos, n = n, alternative = "two.sided", conf.level = 1-alfa)

print(prueba2)
# p = 0.03959

# Como p < alfa, se rechaza la hipótesis nula en favor de la hipótesis alternativa,
# por lo que se concluye con un 95% de confianza, que si hay diferencia en la proporción de autoras
# en la especialidad de neurología y obstetricia.

# ******************************************** PREGUNTA 3 ********************************#
# Suponiendo que la diferencia en la proporción de autoras en la especialidad de          #
# psiquiatría y la de neurología es de 0,25. ¿A cuántos autores deberíamos monitorear     #
# para obtener un intervalo de confianza del 95% y poder estadístico de 75%, si se        #
# intenta mantener aproximadamente la misma proporción de gente estudiada en cada caso?   #
# ****************************************************************************************#
library(pwr)
library(Hmisc)

# Objetivo: 
# Se nos pide encontrar el tamaño de la muestra para autoras en la especialidad de 
# psiquiatría y de neurología, teniendo una diferencia en las proporciones de 0.25,
# un alfa de 0.05 y un poder estadístico de 0.75.

# Valores conocidos:
alfa <- 0.05
poder <- 0.75
diferencia_proporciones <- 0.25

# Proporcion autoras en psiquiatría
psiquiatria = tabla %>% filter(especialidad == "Psiquiatría") 
n_psiquiatria = psiquiatria[["mujeres"]] + psiquiatria[["hombres"]]
# n_psiquiatria = 72
prop1 <- psiquiatria[["mujeres"]] / n_psiquiatria

# Proporcion autoras en neurología
neurologia = tabla %>% filter(especialidad == "Neurología") 
n_neurologia = neurologia[["mujeres"]] + neurologia[["hombres"]]
# n_neurologia = 144
prop2 <- neurologia[["mujeres"]] / n_neurologia

diferencia <- abs(prop1 - prop2)
print(diferencia)
# La diferencia de proporciones es de aproximadamente 0.028,
# pero se nos hace suponer que la diferencia es de 0.25, por lo que
# se aproximan las proporciones:

prop1_nulo <- (prop1 + prop2)/2 - 0.125
prop2_nulo <- (prop1 + prop2)/2 + 0.125
diferencia <- abs(prop1_nulo - prop2_nulo)
# La diferencia de las proporciones es de 0.25.

# Como necesitamos obtener el tamaño para dos muestras,
# se usará la función bsamsize() para calcular el tamaño de la muestra
# para autoras en neurología y psiquiatría


prueba3 <- bsamsize(prop1_nulo, prop2_nulo, fraction=(n_psiquiatria/(n_psiquiatria+n_neurologia)), 
                    alpha = alfa, power = poder)
print(prueba3)
# n1 = 39.81364
# n2 = 79.62729

# Para mantener un alfa de 0.05 y un poder de 0.75, con una diferencia entre las proporciones de 
# psiquiatría y neurología de 0.25, se necesita un tamaño de muestra de 40 y 80 autoras mujeres 
# de la especialidad neurología y psiquiatría respectivamente.