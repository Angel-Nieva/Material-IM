library(discreteRV)

# Crear una variable discreta para representar el dado adulterado
resultados <- 1:6
probabilidades = c(0.25, 0.125, 0.125, 0.125, 0.125, 0.25)
X <- RV(outcomes = resultados, probs = probabilidades)

# Calcular el valor esperado

esperado <- E(X)
cat("valor esperado:", esperado, "\n")
# Calcular la varianza

varianza <- V(X)
cat("varianza:", varianza, "\n")

# Calcular la desviacion estandar

desviacion <- SD(X)
cat("desviación estándar:", desviacion, "\n")
