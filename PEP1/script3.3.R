library(discreteRV)

# Crear una variable discreta para representar el dado adulterado de la tabla y calcular su valor esperado, varianza y desviacion estandar
resultados <- 1:6
probabilidades = c(0.25, 0.125, 0.125, 0.125, 0.125, 0.25)
X <- RV(outcomes = resultados, probs = probabilidades)

esperado_x <- E(X)
varianza_x <- V(X)
desviacion_x <- SD(X)
cat("E(X)", esperado_x, "\n")
cat("V(X", varianza_x, "\n")
cat("SD(X)", desviacion_x, "\n")

# dado balanceado
Y <- X <- RV(outcomes = resultados, probs = 1/6)
esperado_y <- E(Y)
varianza_y <- V(Y)
desviacion_y <- SD(Y)
cat("E(Y)", esperado_y, "\n")
cat("V(Y)", varianza_y, "\n")
cat("SD(Y)", desviacion_y, "\n")

# Crear una combinacion lineal de variables aleatorias y calcular su E, V Y SD
Z <- 0.5 * X + 0.5 * Y 
esperado_z <- E(Z)
varianza_z <- V(Z)
desviacion_z <- SD(Z)
cat("E(Z)", esperado_z, "\n")
cat("V(Z)", varianza_z, "\n")
cat("SD(Z)", desviacion_z, "\n")

