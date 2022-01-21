library("dplyr")

##############################
#####     Pregunta 1     #####
##############################

# Se cree que la ardilla chilena (Octodon degus) emite un chillido cuando es perseguido por un depredador,
# posiblemente para alertar a otros degús. Se hizo un experimento en que 45 degús se liberaron a 10 o a
# 100 metros de su madriguera y luego se les perseguía hasta que se metían en ella, para simular la
# persecución de un depredador. De las 24 ardillas liberadas a 10 metros de la madriguera, 16 emitieron el
# chillido esperado, mientras que solo 3 de las 21 ardillas liberadas a 100 metros de la madriguera lo
# hicieron. ¿Influye la distancia a la madriguera en la emisión del chillido por parte de un degú?

##############################
#####     Desarrollo     #####
##############################

# Se contruye la tabla con los datos:
liberadas_10 <- c(8, 16)
liberadas_100 <- c(18, 3)
tabla <- as.table(rbind(liberadas_10,liberadas_100))

dimnames(tabla) <- list(distancia = c("10[m]", "100[m]"), 
                        chillido = c("No", "Si"))
print(tabla)

#Hipotesis:
# H0: las variables distancia y chillido son independientes (no influye la distancia con el chillido de la ardilla).
# Ha: las variables distancia y chillido son dependientes (la distancia influye en el chillido de la ardilla).

# Ya que se necesita determinar si dos variables dicotómicas (distancia y chillido) están relacionadas, se propone  
# utilizar la prueba exacta de Fisher.

# Suponiendo que la muestra fue seleccionada de manera aleatoria y es menor al 10% de la población, 
# se verifica que las observaciones son independientes entre sí. 
# En consecuencia, se verifica la condición para usar la prueba.  

# Aplicar prueba exacta de Fisher.

alfa <- 0.01
prueba <- fisher.test(tabla,1-alfa)

cat("\nResultados de la prueba:\n")
print(prueba)
# Se obtiene un valor p = 0.0006862

# Conclusión:
# Aún con un nivel de significación α = 0.01, el valor p < α, nos permite rechazar la hipótesis nula.
# En consecuencia, concluimos con un 99% de confianza que las variables distancia y chillido están relacionadas,
# (La distancia a la madriguera influye en la emisión del chillido por parte de un degú).

##############################
#####     Pregunta 3     #####
##############################

# Una investigación monitoreó a más de 50 mil mujeres adultas durante 10 años (Lucas et al., 2011. Coffee,
# Caffeine, and Risk of Depression Among Women. Archives of Internal Medicine, 171(17), 1571–1578) con
# la intención de identificar factores de riesgo de desarrollar un cuadro de depresión. Entre otras cosas, se
# registró el consumo de cafeína, cuyos datos se resumen en la siguiente tabla. ¿Existe alguna asociación
# entre la cantidad de café que se bebe y la depresión?

##############################
#####     Desarrollo     #####
##############################

# Se contruye la tabla con los datos:

depresión_sí <- c(670, 373, 905, 564, 95)
depresión_no <- c(11545, 6244, 16329, 11726, 2288)
tabla3 <- as.table(rbind(depresión_sí,depresión_no))

dimnames(tabla3) <- list(depresión = c("Sí", "No"), 
                        consumo_café = c("1 taza x semana", "2-6 tazas x semana", "1 taza al día",
                                         "2-3 tazas al día", "4 tazas al día"))
print(tabla3)

#Hipotesis:
# H0: las variables depresión y consumo de café son independientes (no existe relación entre el consumo de café y depresión).
# Ha: las variables depresión y consumo de café son dependientes (existe relación entre el consumo de café y depresión).

# Ya que se necesita determinar si dos variables categóricas, siendo una de ellas dicotómica (depresión y consumo de café), 
# están relacionadas, se propone utilizar la prueba chi-cuadrado de independencia.

# Aplicar prueba chi-cuadrado de independencia.

prueba3 <- chisq.test(tabla3)
cat("\nSe calculan los valores esperados de cada grupo:\n")
esperados3 <- round(prueba3[["expected"]], 3)
print(esperados3)

# Suponiendo que la muestra fue seleccionada de manera aleatoria y es menor al 10% de la población, 
# se verifica que las observaciones son independientes entre sí. Además, como las observaciones esperadas
# en cada grupo son > 5. En consecuencia, se verifica la condición para usar la prueba.

cat("\nResultados de la prueba:\n")
print(prueba3)
# Se obtiene un valor p = 0.0003267

# Conclusión:
# Aún con un nivel de significación α = 0.01, el valor p < α, nos permite rechazar la hipótesis nula.
# En consecuencia, concluimos con un 99% de confianza que las variables depresión y consumo de café están relacionadas,
# (Existe relación entre el consumo de café que se bebe y la depresión).