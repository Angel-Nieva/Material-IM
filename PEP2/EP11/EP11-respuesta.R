#
library(readxl)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(simpleboot)
set.seed (432)

##############################
#####     Pregunta 1     #####
##############################

#Propongan una pregunta de investigaci?n original, que involucre la comparaci?n de las medias de dos
#grupos independientes (m?s abajo se dan unos ejemplos). Fijando una semilla propia, seleccionen una
#muestra aleatoria de hogares (250 < n < 500) y respondan la pregunta propuesta utilizando una simulaci?n
#Monte Carlo.

##############################
#####     Desarrollo     #####
##############################

# Funcionpara calcular la diferencia de medias
# Argumentos:
# - muestra_1, muestra_2: vectores numericos con las muestras a comparar
# - FUN: funcion del estadistico E para el que se calcula la diferencia
# Valor:
# - diferencia E_1 - E_2
calcular_diferencia <- function(muestra_1, muestra_2, FUN){
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return(diferencia)
}

# Funcion para hacer una permutacion y calcular el estadistico
# de interes
# Argumentos:
# - muestra_1, muestra_2: vectores numericos con las muestras a comparar
# - FUN: funcion del estadistico E para el que se calcula la diferencia
# Valor:
# - diferencia E_1 - E_2
permutar <- function(muestra_1, muestra_2, FUN){
  n_1 <- length(muestra_1)
  n_2 <- length(muestra_2)
  
  # Hacer la permutacion
  permutacion <- sample(c(muestra_1, muestra_2), size = n_1 + n_2,
                        replace = FALSE)
  
  # Asignar elemento s a los dos grupos
  permutacion_1 <- permutacion[1 : n_1]
  permutacion_2 <- permutacion[n_1 + 1 : n_2]
  
  # Calcular y devolver la diferencia de medias
  return(calcular_diferencia(permutacion_1, permutacion_2, FUN))
}

# Funcion para calcular el valor p
# Argumentos:
# - distribucion: distribucion nula del estadistico de interes
# - valor observado: valor del estadistico de interes para las muestras originales
# - repeticiones: cantidad de permutaciones a realizar
# - alternative: tipo de hipotesis alternativa ("two.sided" para
#   hipotesis bilateral, "greater" o "less" para hipotesis unilaterales)
# Valor:
# - el valorp calculado
calcular_valor_p <- function(distribucion, valor_observado,
                             repeticiones, alternative){
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

# Funcion para graficar una distribucion
# Argumentos:
# - distribucion: distribucion nula el estadistico de interes
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot
graficar_distribucion <- function(distribucion){
  observaciones <- data.frame(distribucion)
  
  histograma <- gghistogram(observaciones, x = "distribucion",
                            xlab = "Estadistico de interes",
                            ylab = "Frecuencia")
  qq <- ggqqplot(observaciones, x = "distribucion")
  
  # Crear una unica figura con todos losgraficos de dispersion
  figura <- ggarrange(histograma, qq, ncol = 2, nrow = 1)
  print(figura)
}

# Funcion para hacer la prueba de permutaciones
# Argumentos :
# - muestra_1, muestra_2: vectores numericos con las muestras a comparar
# - repeticiones: cantidad de permutaciones a realizar
# - FUN: funcion del estadistico E para el que se calcula la diferencia
# - alternative: tipo de hipotesis alternativa ("two.sided" para
#   hipotesis bilateral, "greater" o "less" para hipotesis unilaterales)
# - plot: si es TRUE, construye el grafico de la distribucion generada
# - ...: otros argumentos a ser entregados a graficar_distribucion
contrastar_hipotesis_permutaciones <- function(muestra_1, muestra_2,
                                               repeticiones, FUN,
                                               alternative, plot){
  cat("Prueba de permutaciones\n\n")
  cat("Hipotesis alternativa:", alternative, "\n")
  observado <- calcular_diferencia(muestra_1, muestra_2, FUN)
  cat("Valor observado:", observado, "\n")
  
  distribucion <- rep(NA, repeticiones)
  
  for(i in 1:repeticiones){
    distribucion[i] <- permutar(muestra_1, muestra_2, FUN)
  }
  
  if(plot){
    graficar_distribucion(distribucion)
  }
  
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones,
                              "two.sided")
  
  cat("Valor p:", valor_p, "\n\n")
}


#----------------------------------------------------------------------------------------------------------------

# Existe una diferencia significativa en el ingreso per capita de las personas de la region de tarapaca y 
# magallanes

#ruta_excel <- "C:\\Users\\esteb\\Desktop\\Datos-Casen-v2.xls"
ruta_excel <- "C:/Users/Dell PC/Desktop/IME/Material-IME/PEP2/EP11/Datos-Casen-v2.xls"
datos <- read_excel(ruta_excel)
datos <- datos%>% select(region,numper,ytotcorh)

#se obtiene los ingresos per capita de la Regi?n de tarapac?
datosTarapaca  <- datos %>% filter(region == "Regi?n de Tarapac?",numper != "NA")
datosTarapaca$ytotcorh <- datosTarapaca$ytotcorh / as.integer(datosTarapaca$numper)
datosTarapaca <- datosTarapaca[["ytotcorh"]]   

#se obtiene los ingresos per capita de la Regi?n de Magallanes y de la Ant?rtica Chilena
datosMagallanes  <- datos %>% filter(region == "Regi?n de Magallanes y de la Ant?rtica Chilena",numper != "NA")
datosMagallanes$ytotcorh <- datosMagallanes$ytotcorh / as.integer(datosMagallanes$numper)
datosMagallanes <- datosMagallanes[["ytotcorh"]] 



#La hipotesis a formular son:
# H0: No existe una diferencia significativa en el ingreso per capita de los habitantes de la regi?n de Tarapac? y Magallanes
# HA: S? existe una diferencia significativa en el ingreso per capita de los habitantes de la regi?n de Tarapac? y Magallanes

# Para esta prueba se utiliza la simulaci?n de  Monte Carlo, con un nivel de significaci?n de 0.01, es decir, 
# con un 99% de confianza, con el fin de ser cautelosos al realizar la prueba, y con ello, la respuesta entregada.

# Hacer pruebas de permutciones para la media y la varianza
R = 4000

contrastar_hipotesis_permutaciones(datosTarapaca, datosMagallanes, repeticiones = R, FUN = mean,
                                   alternative = "two.sided", plot = TRUE)

#Conclusi?n:
#Dado que la prueba realizada arroja un valor p = 0.0002499375 < alfa = 0.01, se rechaza la hipotesis nula en favor
# de la hipotesis alternativa. Entonces, se asegura con un 99% de confianza que s? existe una diferencia 
# significativa en el ingreso per capita de los habitantes de la regi?n de Tarapac? y Magallanes


##############################
#####     Pregunta 2     #####
##############################

#Propongan una pregunta de investigaci?n original, que involucre la comparaci?n de las medias de m?s de
#dos grupos independientes (m?s abajo se dan unos ejemplos). Fijando una semilla distinta a la anterior,
#seleccionen una muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta
#utilizando bootstrapping. Solo por ejercicio acad?mico, aplique un an?lisis post-hoc con bootstrapping
#aunque este no sea necesario.

##############################
#####     Desarrollo     #####
##############################

# Pregunta propuesta:-
# Existe una diferencia significativa entre el n?mero de personas en el hogar con respecto a 
# 3 regiones (tarapaca, metropolitana, magallanes)


# La hip?tesis a formular son:
# H0: No existe una diferencia significativa entre el n?mero de personas en el hogar con respecto a 
#     tres regiones (tarapaca, metropolitana, magallanes)
# HA: S? existe una diferencia significativa entre el n?mero de personas en el hogar con respecto a 
#     tres regiones (tarapaca, metropolitana, magallanes)

# Para esta prueba, se utiizar? el m?todo de bootstrapping para evaluar tres muestras independientes. As?,
# es necesario mencionar que al graficar las distribuciones de estas tres muestras, se presentan varios valores
# at?picos, por ello, se utilizar? un nivel de significaci?n igual a 0.01 con el fin de ser muy cautelosos al 
# realizar la prueba. Por tanto, se proceder? a realizar la preuba con el m?todo de bootstrapping utilizando
# un 99% de confianza.


datos2 <- datos %>% filter(region == "Regi?n de Tarapac?" | 
                             region == "Regi?n Metropolitana de Santiago" |
                             region == "Regi?n de Magallanes y de la Ant?rtica Chilena" ,numper != "NA")
#se define el valor del alpha 
alpha <- 0.01


set.seed(523)
# Se obtiene la muestra de tama?o 500
tamano <- 500
muestra <- datos2[sample(nrow(datos2), tamano),]


integrantes <- as.integer(muestra[["numper"]])
region <- factor(muestra[["region"]])
instancia <- factor(1:tamano)
datos3 <- data.frame(instancia, integrantes, region)


integrantes_tarapaca <- datos3 %>% filter(region == "Regi?n de Tarapac?")
n_integrantes_tarapaca <- nrow(integrantes_tarapaca)

integrantes_metropolitana <- datos3 %>% filter(region == "Regi?n Metropolitana de Santiago")
n_integrantes_metropolitana <- nrow(integrantes_metropolitana)

integrantes_magallanes <- datos3 %>% filter(region == "Regi?n de Magallanes y de la Ant?rtica Chilena")
n_integrantes_magallanes <- nrow(integrantes_magallanes)

my_boot <- function(x){
  #se toma una muestra con reemplazo para cada grupo
  i_integrantes_tarapaca <- sample(1:n_integrantes_tarapaca, replace = TRUE) 
  i_integrantes_metropolitana <- sample(1:n_integrantes_metropolitana, replace = TRUE)
  i_integrantes_magallanes <- sample(1:n_integrantes_magallanes, replace = TRUE)
  rbind(integrantes_tarapaca[i_integrantes_tarapaca,], integrantes_metropolitana[i_integrantes_metropolitana,], integrantes_magallanes[i_integrantes_magallanes,])
}

my_F <- function(frame){
  # Obtener valor observado, correspondiente al estad?stico F entregado
  # por ANOVA para la muestra original
  anova <- ezANOVA(frame, dv = integrantes, between = region, 
                   wid = instancia, return_aov = FALSE)
  invisible(anova$ANOVA$F)
}

calcular_valor_p <- function(distribucion, valor_observado, repeticiones, alternative){
  if (alternative == "two.sided"){
    numerador <- sum (abs(distribucion) > abs(valor_observado)) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador 
  }
  else if(alternative == "greater"){
    numerador <- sum(abs(distribucion) > valor_observado) + 1
    denominador <- repeticiones
    valor_p <- numerador / denominador
  }
  else{
    numerador <- sum(distribucion < valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  return(valor_p)
}


# Se obtiene el estad?stico F original
anova_original <- ezANOVA(datos3, dv = integrantes, between = region, 
                          wid = instancia, return_aov = FALSE)
print(anova_original)

R <- 4000 
set.seed(705) # segunda semilla

# Se llama a la funci?n my_boot
# lapply en lista
distribucion1  <- lapply(1:R, my_boot)
#print(distribucion1)

#guarda en un vector
suppressMessages(suppressWarnings(Fs <- sapply(distribucion1, my_F))) # evitar los warnings
#Fs <- sapply(distribucion1, my_F)


p <- calcular_valor_p(Fs, anova_original$ANOVA$F, R, "two.sided")
cat("el valor p es: ", p ) 

graficar_distribucion(Fs)


# CONCLUSI?N:
# Al finalizar la prueba, se obtiene un p igual a X, el cual resulta mayor al nivel de significaci?n, 
# por tanto se falla en rechazar la hip?tesis nula. Es decir, se asegura con un 99% de confianza que
# no existe una diferencia significativa entre el n?mero de personas en el hogar con respecto a tres 
# regiones (tarapaca, metropolitana, magallanes).

