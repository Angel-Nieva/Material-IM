library(ggpubr)
library(dplyr)

# Lectura de archivo 
dir <- "C:/Users/Dell PC/Desktop/IME-2022/Actividades/act4"

basename <- "EP04-datos.csv"
file <- file.path(dir, basename)
población <- read.csv2(file = file, fileEncoding = "UTF-8")


# ******************************************** PREGUNTA 1 ********************************#
# El Comité Olímpico cree que el mejor tiempo medio de los atletas orientales             #
# antes de ingresar al programa de entrenamiento es inferior a 20,32 segundos.            #
# ¿Soportan los datos esta afirmación?                                                    #
# ****************************************************************************************#

# Se establecen las hipótesis.
# H0: media = 20,32; (el mejor tiempo medio de los atletas orientales antes de ingresar 
#                      al programa de entrenamiento es igual a 20,32 segundos)
# HA:  media < 20,32; (el mejor tiempo medio de los atletas orientales antes de ingresar 
#                      al programa de entrenamiento es inferior a 20,32 segundos)

# Para poder realizar una PRUEBA Z, se debe cumplir que:
# 1) Cantidad de muestras >= 30
# 2) Observaciones independientes
# 3) Población sigue una distribución Normal

atletas_orientales <- población %>% filter(Raza == "Oriental")

# 1) Cantidad de muestras <= 30
cantidad_muestras <- count(atletas_orientales)
print(cantidad_muestras)

# Al contar con una muestra pequeña ( n = 14), se propone utilizar 
# la prueba T de Student para una muestra.

# Para poder realizar una PRUEBA T de Student, se debe cumplir que:
# 1) Las observaciones son independientes entre sí.
# 2) Las observaciones provienen de una distribución cercana a la normal.

# Se comprueban las condiciones:

# 1) Observaciones independientes:
# El Comité Olímpico ha recopilado datos de diversos atletas, por lo que se puede suponer que 
# las observaciones son independientes.

# 2) Se estudia la normalidad usando la prueba de Shapiro:

# Se define el nivel de significancia, para un 95% de confianza (1-alfa = 0.95)
alfa <- 0.05
confianza <- 1 - alfa

pre_programa <- atletas_orientales[["Previo"]]
normalidad <- shapiro.test(pre_programa)
print(normalidad)
# p = 0.4826

# Como p > alfa, se puede concluir que las observaciones provienen de una distribución 
# cercana a la normal.

# Se declara el valor nulo
nulo <- 20.32

# Se realiza la prueba utilizando t.test
prueba <- t.test(pre_programa , alternative = "less", mu = nulo, conf.level = confianza)
print(prueba)
# p = 0.002579

# Dado que el valor de p < alfa , se rechaza la hipótesis nula en favor de la hipótesis alternativa. 
# Finalmente, como creía el Comité Olímpico, se puede asegurar con un 95% de confianza que el mejor 
# tiempo medio de los atletas orientales antes de ingresar 
# al programa de entrenamiento es inferior a 20,32 segundos.

# ******************************************** PREGUNTA 2 ********************************#
# ¿Sugieren los datos que la mejor marca de los atletas negros se reduce en más de 2,27   #
#  segundos tras el entrenamiento?                                                        #
# ****************************************************************************************#

# Se establecen las hipótesis.
# H0: mediaDiferencias = 2,27; (la media de las diferencias entre la marca de los atletas negros antes y después del entrenamiento
#                               es de 2,27[s])

# HA: mediaDiferencias > 2,27; (la media de las diferencias entre la marca de los atletas negros antes y despues del entrenamiento
#                               es mayor a 2,27[s])

atletas_negros <- población %>% filter(Raza == "Negra")

# 1) Cantidad de muestras <= 30
cantidad_muestras2 <- count(atletas_negros)
print(cantidad_muestras2)

# Al contar con una muestra pequeña ( n = 18), se propone utilizar 
# la prueba T de Student para dos muestras pareadas.

# Se comprueban las condiciones:

# 1) Observaciones independientes:
# El Comité Olímpico ha recopilado datos de diversos atletas, por lo que se puede suponer que 
# las observaciones son independientes.

# 2) Se estudia la normalidad usando la prueba de Shapiro:

# Se define el nivel de significancia, para un 95% de confianza (1-alfa = 0.95)
alfa <- 0.05
confianza <- 1 - alfa

previo <- atletas_negros[["Previo"]]
posterior <- atletas_negros[["Posterior"]]

# Se calcula la diferencia entre la marca previo y posterior al entrenamiento
diferencia <- previo - posterior

normalidad2 <- shapiro.test(diferencia)
print(normalidad2)
# p =0.6622

# Como p > alfa, se puede concluir que las observaciones provienen de una distribución 
# cercana a la normal.

# Se declara el valor nulo
nulo2 <- 2.27

# Se realiza la prueba utilizando t.test
prueba2 <- t.test(diferencia , alternative = "greater", mu = nulo2, conf.level = confianza)
print(prueba2)
# p = 0.00283

# Dado que el valor de p < alfa , se rechaza la hipótesis nula en favor de la hipótesis alternativa. 
# Finalmente, se puede asegurar con un 95% de confianza que la mejor marca de los atletas negros se reduce en más de 2,27
# segundos tras el entrenamiento.

# ******************************************** PREGUNTA 3 ********************************#
# ¿Es posible afirmar que, en promedio, los atletas negros superan a los orientales por   #
# 3,08 segundos después del entrenamiento?                                                #
# ****************************************************************************************#

# Se establecen las hipótesis.
# H0: media_orientales - media_negros =< 3.08; (en promedio, la media de los atletas negros no supera por 
#                                              3.08[s] a los atletas orientales después del entrenamiento)

# HA: media_orientales - media_negros > 3.08; (en promedio, la media de los atletas negros supera por 
#                                              3.08[s] a los atletas orientales después del entrenamiento)

atletas_negros <- población %>% filter(Raza == "Negra")
atletas_orientales <- población %>% filter(Raza == "Oriental")


# 1) Cantidad de muestras <= 30
cantidad_muestras3 <- count(atletas_negros)
cantidad_muestras4 <- count(atletas_orientales)
print(cantidad_muestras3)
print(cantidad_muestras4)

# Al contar con una muestra pequeña ( n = 18 y n = 14), se propone utilizar 
# la prueba T de Student para dos muestras independientes.

# Se comprueban las condiciones:

# 1) Observaciones independientes:
# El Comité Olímpico ha recopilado datos de diversos atletas, por lo que se puede suponer que 
# las observaciones son independientes para atletas negros y orientales.

# 2) Se estudia la normalidad usando la prueba de Shapiro:

# Se define el nivel de significancia, para un 95% de confianza (1-alfa = 0.95)
alfa <- 0.05
confianza <- 1 - alfa

posterior_negros <- atletas_negros[["Posterior"]]
posterior_orientales <- atletas_orientales[["Posterior"]]

# Se calcula la normalidad para ambos atletas
normalidad3 <- shapiro.test(posterior_negros)
normalidad4 <- shapiro.test(posterior_orientales)
print(normalidad3)
# p = 0.1537
print(normalidad4)
# p = 0.8099

# Como p > alfa para ambos casos, se puede concluir que las observaciones provienen de una distribución 
# cercana a la normal para atletas negros y orientales posterior al entrenamiento.

# Se declara el valor nulo
nulo3 <- 3.08

# Se realiza la prueba utilizando t.test
prueba3 <- t.test(x = posterior_orientales, y = posterior_negros, paired = FALSE , alternative = "greater", mu = nulo3, conf.level = confianza)
print(prueba3)
# p = 0.1408

# Calcular la diferencia entre las medias .
media_orientales <- mean(posterior_orientales)
media_negros <- mean (posterior_negros)

diferencia <- media_orientales - media_negros
cat (" Diferencia de las medias =", diferencia , "[S]\n")

# Dado que el valor de p > alfa ,  no se rechaza la hipótesis nula en favor de la hipótesis alternativa. 
# Finalmente, se puede asegurar con un 95% de confianza que en promedio, la media de los atletas 
# negros no supera por 3.08[s] a los atletas orientales después del entrenamiento.
