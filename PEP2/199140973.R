# ******************************************** PREGUNTA 1 ********************************#
# Una escudería de Fórmula 1 ha decidido probar un nuevo suplemento para el combustible,
# elaborado en base a óxido nitroso. A fin de determinar si incorporar el uso estándar de este suplemento en
# futuras competiciones, la escudería ha medido el tiempo(en minutos) que sus pilotos tardan en completar
# 10 vueltas a la pista de prueba, cada uno en el mismo vehículo, usando el suplemento y sin usarlo. La
# directiva de la escudería le ha solicitado a usted que, usando repeticiones con el método de Monte Carlo,
# indique si existe diferencia en el tiempo registrado por los pilotos al usar y no usar el nuevo suplemento.
# ****************************************************************************************#

# Ingresar datos originales

id <- c(1:15)
sin <- c(12.851, 10.616, 12.076, 11.438, 13.107, 17.443, 15.007, 12.620,
         17.938, 18.096, 25.000, 13.214, 17.739, 11.990, 14.500)
con <- c(14.499, 11.230, 6.470, 11.640, 14.773, 11.448, 13.464, 13.203,
         14.971, 13.279, 13.543, 12.720, 12.971, 11.446, 10.736)
datos <- data.frame(id, sin, con)

# Objetivo: Se necesita determinar si existen diferencias en el tiempo registrado por los pilotos al usar
# y no usar el nuevo suplemento. Como queremos averiguar si existen diferencias en las medias de dos
# grupos se plantea utilizar la prueba T de studen para dos muestras pareadas.

# Se define un alfa
alfa <- 0.05

# Se estudia la normalidad de los grupos mediante la prueba de shapiro.
normalidad_sin <- shapiro.test(sin)
# p-value = 0.02389 < alfa
normalidad_con <- shapiro.test(con)
# p-value = 0.02863 < alfa

# Como se puede observar en las pruebas de Shapiro, las muestras no se
# comportan de manera normal ( p < alfa), por lo que no es adecuado utilizar
# la prueba T de Student. Es por este motivo que se piensa utilizar 
# el método de repeticiones con el método de Monte Carlo.

# Se formula la Hipótesis

# H0: media_dif = 0 ; La media de las diferencias en los tiempos de que los pilotos tardan en completar 10 vueltas
#     al usar el suplemento y no usar el suplemento es 0
# HA: media_dif != 0 ; La media de las diferencias en los tiempos de que los pilotos tardan en completar 10 vueltas
#     al usar el suplemento y no usar el suplemento es distinta de 0

# Como queremos determinar si existen diferencias significativas en el tiempo entre las dos muestras pareadas,
# entonces el estadístico a utilizar para el re-muestreo es la media.

#-----------------------------------------------------------------------------#
# Se definen las funciones a utilizar para el metodo de Monte Carlo.

# Se define una semilla
set.seed(199)

# Función para calcular la diferencia entre dos estadísticos (Media para el ejemplo)
# Argumentos:
# - muestra_1, muestra_2: vectores numéricos con las muestras a comparar
# - FUN: función del estadístico E para el que se calcula la diferencia
# Valor:
# - diferencia E_1 - E_2
calcular_diferencia <- function(muestra_1, muestra_2, FUN){
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return(diferencia)
}

# Función para hacer una permutación y calcular el estadístico
# de interés
# Argumentos:
# - muestra_1, muestra_2: vectores numéricos con las muestras a comparar
# - FUN: función del estadístico E para el que se calcula la diferencia
# Valor:
# - diferencia E_1 - E_2
permutar <- function(muestra_1, muestra_2, FUN){
  n_1 <- length(muestra_1)
  n_2 <- length(muestra_2)
  
  # Hacer la permutación
  permutacion <- sample(c(muestra_1, muestra_2), size = n_1 + n_2,
                        replace = FALSE) # TRUE si es independiente / FALSE correlacionados
  
  # Asignar elementos a los dos grupos
  permutacion_1 <- permutacion[1 : n_1]
  permutacion_2 <- permutacion[n_1 + 1 : n_2]
  
  # Calcular y devolver la diferencia de medias
  return(calcular_diferencia(permutacion_1, permutacion_2, FUN))
}

# Función para calcular el valor p
# Argumentos:
# - distribución: distribución nula del estadístico de interés
# - valor observado: valor del estadístico de interés para las muestras originales
# - repeticiones: cantidad de permutaciones a realizar
# - alternative: tipo de hipótesis alternativa ("two.sided" para
#   hipótesis bilateral, "greater" o "less" para hipótesis unilaterales)
# Valor:
# - el valor p calculado
calcular_valor_p <- function(distribucion, valor_observado, repeticiones, alternative){
  if(alternative == "two.sided"){
    numerador <- sum(abs(distribucion) > abs(valor_observado)) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else if(alternative == "greater"){
    numerador <- sum(distribucion > valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else{
    numerador <- sum(distribucion < valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  return(valor_p)
}

# Función para graficar una distribución
# Argumentos:
# - distribución: distribución nula el estadístico de interés
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot
graficar_distribucion <- function(distribucion, ...){
  observaciones <- data.frame(distribucion)
  
  histograma <- gghistogram(observaciones, x = "distribucion",
                            xlab = "Media para la diferencia de las muestras",
                            ylab = "Frecuencia", ...)
  qq <- ggqqplot(observaciones, x = "distribucion", ...)
  
  # Crear una unica figura con todos losgraficos de dispersion
  figura <- ggarrange(histograma, qq, ncol = 2, nrow = 1)
  print(figura)
}

# Función para hacer la prueba de permutaciones
# Argumentos :
# - muestra_1, muestra_2: vectores numéricos con las muestras a comparar
# - repeticiones: cantidad de permutaciones a realizar
# - FUN: función del estadístico E para el que se calcula la diferencia
# - alternative: tipo de hipótesis alternativa ("two.sided" para
#   hipótesis bilateral, "greater" o "less" para hipótesis unilaterales)
# - plot: si es TRUE, construye el grafico de la distribución generada
# - ...: otros argumentos a ser entregados a graficar_distribucion
contrastar_hipotesis_permutaciones <- function(muestra_1, muestra_2,
                                               repeticiones, FUN,
                                               alternative, plot, ...){
  cat("Prueba de permutaciones\n\n")
  cat("Hipotesis alternativa:", alternative, "\n")
  observado <- calcular_diferencia(muestra_1, muestra_2, FUN)
  cat("Valor observado:", observado, "\n")
  
  distribucion <- rep(NA, repeticiones)
  
  for(i in 1:repeticiones){
    distribucion[i] <- permutar(muestra_1, muestra_2, FUN)
  }
  
  if(plot){
    graficar_distribucion(distribucion, ...)
  }
  
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones,
                              "two.sided")
  
  cat("Valor p:", valor_p, "\n\n")
}
#-----------------------------------------------------------------------------#

# Para realizar la prueba, se realizará una simulación de Monte Carlo con un nivel de
# significación de 0.05, utilizando 3999 permutaciones.

# Hacer pruebas de permutaciones para la media
R = 3999

contrastar_hipotesis_permutaciones(sin, con, repeticiones = R, FUN = mean,
                                   alternative = "two.sided", plot = TRUE, color = "blue", fill = "blue", bins = 15)
# Valor p: 0.029

# En conclusión, podemos decir que Considerando un nivel de significación alfa = 0,05 > 0.029, 
# se rechaza la hipótesis nula en favor de la hipótesis alternativa. 
# En consecuencia, concluimos con 95 % de confianza que si existe una diferencia en los tiempos que los 
# pilotos tardan en completar las 10 vueltas al usar y no usar el suplemento para combustibles. 

