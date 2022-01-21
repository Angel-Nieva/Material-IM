library(ggpubr)
library(ggplot2)

datos <- read.csv2("C:/Users/Dell PC/Desktop/IME/Material-IME/Datos PEP 1.csv")

# Dar formato categorico:

datos[["sexo"]] <- factor(datos[["sexo"]])
datos[["planeta"]] <- factor(datos[["planeta"]])

set.seed(7)
tamano <- nrow(datos)
estaturas <- as.numeric(datos[["estatura"]])

# Valores importantes
desv_est <- sd(estaturas)
valor_nulo <- 1.65
n <- length(estaturas)


# Hipótesis: 
# H0: Las estatura promedio de los reclutas es 1.65 [m] ( µ  = 1.65)
# H1: Las estatura promedio de los reclutas es superior a 1.65[m] (µ  > 1.65)

# Como se necesita ver si los promedios difieren o no se verifica con una prueba de normalidad.

alfa = 0.05
# Se estudia la normalidad usando la prueba de Shapiro.
normalidad <- shapiro.test(estaturas)
print(normalidad)
# 0.006768 siendo mayor que alfa por lo que se comporta con normalidad

# Como las muestras tienen mas de 30 observaciones, son independientes y se comporta de forma normal
# podemos proceder con el estudio.

prueba <- t.test(x = estaturas,
                 alternative = "greater",
                 mu = valor_nulo,
                 conf.level = 1 - alfa)
print(prueba)

# Como el valor p = 2.2e-16 < alfa, es prueba suficiente para descartar la hipotesis nula a favor de 
# la hipotesis alternativa.




