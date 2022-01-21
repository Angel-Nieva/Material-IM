#----- GRUPO 7 -----
#Integrantes:
#Angel Nieva
#Esteban Cruces
#Maria Jesus Cañoles

library(ggpubr)
library(ggplot2)

# Identificacion de los tipos de variables y sus escalas:

# folio: discreta
# o: discreta 
# id.vivienda: discreta
# hogar: discreta
# region: nominal
# provincia: nominal 
# comuna: nominal 
# ing.comuna: discreta 
# zona: nominal
# sexo: nominal 
# edad: discreta 
# ecivil: nominal
# ch1: nominal 
# ytot: discreta

basename <- "Casen 2017.csv"
file <- file.path("C:/Users/Dell PC/Desktop/inferencia/clase4", basename)
poblacion <- read.csv(file = file, fileEncoding = "UTF-8")

#Dar formato categorico a las variables categoricas (nominal y ordinal)
poblacion[["region"]] <- factor(poblacion[["region"]], labels = c("RM"))
poblacion[["provincia"]] <- factor(poblacion[["provincia"]])
poblacion[["comuna"]] <- factor(poblacion[["comuna"]])
poblacion[["zona"]] <- factor(poblacion[["zona"]])
poblacion[["sexo"]] <- factor(poblacion[["sexo"]])
poblacion[["ecivil"]] <- factor(poblacion[["ecivil"]])
poblacion[["ch1"]] <- factor(poblacion[["ch1"]])

#----- PREGUNTA 1 -----
#Definan su propia semilla y obtengan 5.000 casos para una distribucion de ingresos aproximadamente normal
tamano <- nrow(poblacion)
ingreso <- as.numeric(poblacion[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamano.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamano.podado )
set.seed(7)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)


#----- PREGUNTA 2 -----
#A partir de la distribucion conseguida, y sin usar nuevamente la funcion rnorm(), generen la correspondiente distribucion Z
z <- (ingreso.normal - media.ingreso)/sd.ingreso

#----- PREGUNTA 3 -----
#Con la distribucion Z obtenida en el punto anterior, y sin utilizar funciones como rchisq(), 
#   construyan dos distribuciones ??2, cada una con m?s de 3 y menos de 15 grados de libertad

# 5 grados de libertad
chi5 <- c() #vector vacio
for(i in 1:5000){
  #elegir 5 valores aleatorios
  x <- sample(z,5)
  x <- x^2
  chi5 <- c(chi5,sum(x))
}

# 10 grados de libertad
chi10 <- c() #vector vacio
for(i in 1:5000){
  #elegir 5 valores aleatorios
  x <- sample(z,10)
  x <- x^2
  chi10 <- c(chi10,sum(x))
}

#----- PREGUNTA 4 -----
#Usando las dos distribuciones X2 generadas en el punto anterior, construyan una distribucion F

distribucion.F <- ( (chi10^2)/5 ) / ( (chi5^2)/10 )

#----- PREGUNTA 5 -----
#En cada caso, construyan un grafico para mostrar las distribuciones obtenidas a sus companeras y companeros.

# Histograma normal
h1 <- data.frame(ingreso.normal)
g <- ggplot(h1, aes(x = ingreso.normal)) + 
  geom_histogram(color = "black", fill = "white", bins=30) +
  theme_pubr() +
  ggtitle("Distribución normal")
print(g)

#Histograma distribucion Z con 5 grados de libertad
h2 <- data.frame(chi5)
g2 <- ggplot(h2, aes(x = chi5)) + 
  geom_histogram(color = "black", fill = "white", bins=30) +
  theme_pubr() +
  ggtitle("Distribución normal estandarizada con 5 grados de libertad")
print(g2)

#Histograma distribucion Z con 10 grados de libertad
h3 <- data.frame(chi10)
g3 <- ggplot(h3, aes(x = chi10)) + 
  geom_histogram(color = "black", fill = "white", bins=30) +
  theme_pubr() +
  ggtitle("Distribución normal estandarizada con 10 grados de libertad")
print(g3)

#Histograma distribucion F
h4 <- data.frame(distribucion.F)
g4 <- ggplot(h4, aes(x = distribucion.F)) + 
  geom_histogram(color = "black", fill = "white", bins=30) +
  theme_pubr() +
  ggtitle("Distribución F")
print(g4)


