library(dplyr) # Filter
library(ggpubr)
library(leaps)
library(car) # 
library(caret) # train
basename <- "EP13 Datos.csv"
file <- file.path("C:/Users/Dell PC/Desktop/IME-2022/Actividades/act13",basename)
datos <- read.csv2(file = file)

# PRIMER PASO
# Se establece la semilla según el integrante de menor edad en el equipo
# (últimos 4 dígitos de su rut).
semilla <- 0731
set.seed(semilla)

# SEGUNDO PASO
# Seleccionar una muestra de 50 mujeres (si la semilla es numero par) 
# o 50 hombres (si la semilla es número impar).

# Hombres
datos_hombres <- filter(datos, Gender == 1)
datos_hombres <- datos_hombres[sample(nrow(datos_hombres), 50, replace = F), ]
datos_hombres["Gender"] <- NULL

# TERCER PASO
# Seleccionar de forma aleatoria ocho posibles variables predictoras.
muestra_hombres <- datos_hombres

# Separar variable de respuesta.
peso <- muestra_hombres["Weight"]
muestra_hombres["Weight"] <- NULL

# Se obtienen las 8 variables
columnas_muestra <- colnames(muestra_hombres)
predictores_aleatorios <- sample(columnas_muestra,8,replace = FALSE)
cat("Predictores seleccionados al azar:\n")
print(predictores_aleatorios)

# CUARTO PASO
# Seleccionar, de las otras variables, una que el equipo considere que podría ser útil 
# para predecir la variable Peso, justificando bien esta selección.

# Se almacenan los posibles predictores que pueden ser seleccionados
variables_restantes <- setdiff(columnas_muestra, predictores_aleatorios)

# Se reduce el data.frame con las columnas a utilizar.
muestra_hombres <- muestra_hombres[variables_restantes]

# Se agrega la columna peso a la muestra obtenida.
muestra_hombres <- cbind(muestra_hombres, peso)

# Se utiliza la función add1, la cual evalúa la incorporación de cada nuevo predictor potencial
# a un modelo base y entrega algunas métricas para el efecto que tiene en su incorporación.

# Ajustar un modelo nulo.
nulo <- lm(Weight ~ 1, data = muestra_hombres)
# Ajustar modelo completo.
completo <- lm(Weight ~ ., data = muestra_hombres)

# Evaluar la variable para incorporar
print(add1(nulo, scope = completo))

# Se obtiene que el mejor predictor para incorporar al modelo nulo es Hip.Girth, ya que según la función
# add1, es el predictor que retorna el menor AIC al ser incorporado al modelo.

# Además, con la ayuda de la función cor, podemos ver que la variable predictora con la mayor 
# correlación es Hip.Girth (Grosor a la altura de las caderas). Por lo que la variable predictora 
# Hip.Girth puede ser útil para predecir la variable peso. 	
correlaciones <- round(cor(x = muestra_hombres, method = "pearson"), 3)

# QUINTO PASO
# Construir un modelo de regresión lineal simple con el predictor seleccionado en el paso anterior.

# modelo <- lm(Weight ~ Hip.Girth, data = muestra_hombres)
# print(summary(modelo))

# Ajustar modelo de regresión lineal simple usando validación cruzada de 10
# pliegues.
modelo <- train (Weight ~ Hip.Girth, data = muestra_hombres, method = "lm",
              trControl = trainControl(method = "cv", number = 10))
cat("\nModelo de regresión lineal simple\n")
print(summary(modelo))

# Se grafica el modelo.
g <- ggscatter(muestra_hombres, x = "Hip.Girth", y = "Weight", color = "blue",
               xlab = "Grosor a la altura de las caderas", ylab = "Peso")
print(g)

# SEXTO PASO
# Usando herramientas para la exploración de modelos del entorno R, buscar entre dos y cinco predictores 
# de entre las variables seleccionadas al azar en el punto 3, para agregar al modelo de regresión lineal 
# simple obtenido en el paso 5.

# Se reduce el data.frame con las columnas a utilizar y se guarda la columna Hip.Girth .
muestra_hombres_2 <- datos_hombres
girth <- muestra_hombres_2["Hip.Girth"]
muestra_hombres_2 <- muestra_hombres_2[predictores_aleatorios]


# Se agrega la columna peso y Hip.Girt
muestra_hombres_2 <- cbind(muestra_hombres_2, peso)
muestra_hombres_2 <- cbind(muestra_hombres_2, girth)

# Se recrea el modelo del paso 5
modelo_2 <- lm(Weight ~ Hip.Girth, data = muestra_hombres_2)
print(summary(modelo_2))

#Se grafica el modelo.
g_2 <- ggscatter(muestra_hombres_2, x = "Hip.Girth", y = "Weight", color = "blue",
                 xlab = "Grosor a la altura de las caderas", ylab = "Peso")
print(g_2)


# Ajustar modelo completo
completo2 <- lm(Weight ~ ., data=muestra_hombres_2)

# Ajustar modelo con eliminación hacia atrás
backwards <- step(object = completo2, scope = list(lower = modelo_2),
                  direction = "backward",
                  trace = 0)
print(summary(backwards))
#AIC(backwards) = 260.6668

# Utilizando eliminación hacia atrás, se agregan 6 predictores al modelo:
# Ankle.Minimum.Girth, Waist.Girth, Chest.Girth, Biiliac.diameter, Age y Elbows.diameter.
# Como nos piden agregar un máximo de 5 predictores al modelo de regresión lineal simple, 
# Se estudia la correlación entre estos predictores y la variable respuesta (Weight) para ver
# si existe una relación lineal, en caso contrario, no se agrega al modelo.

correlaciones <- round(cor(x = muestra_hombres_2, method = "pearson"), 3)

# Se puede ver que el predictor Age tiene la menor correlación entre los predictores (0.116), por lo
# que no se agrega al modelo. Finalmente, los predictores para agregar al modelo del paso 5 son:
# Waist.Girth, Chest.Girth, Biiliac.diameter, Age y Elbows.diameter.

modelo_final <- update(modelo_2, . ~ . + Ankle.Minimum.Girth + Waist.Girth + Chest.Girth + 
                         Biiliac.diameter  + Elbows.diameter)

# SÉPTIMO PASO
# Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema con 
# las condiciones que deben cumplir.

# Se define un nivel de significación:
alfa <- 0.05

# Se comprueban las condiciones para el primer modelo de RLS:

# Condiciones:
# 1. Los datos deben presentar una relación lineal.
# 2. La distribución de los residuos debe ser cercana a la normal.
# 3. La variabilidad de los puntos en torno a la línea de mínimos cuadrados debe ser aproximadamente
#    constante.
# 4. Las observaciones deben ser independientes entre sí.

# Verificación de condiciones:
# 1) Se puede saber si existe una relación lineal entre la variable predictora (Hip.Girth)
#    y respuesta (Weight) mediante la correlación entre las dos variables.
correlaciones_2 <- round(cor(x = muestra_hombres, method = "pearson"), 3)
cat("\n\n")

# Se obtiene una correlación de 0.892 para Weight~Hip.Girth, lo que significa que existe
# una relación lineal directa relativamente fuerte entre la variable respuesta y predictora.

# 2) Verificar la normalidad de los residuos
print(shapiro.test(modelo$residuals))
# p-value = 0.1424

# Al aplicar la prueba de normalidad de los residuos concluimos que estos siguen una 
# distribución cercana a la normal ( p > alfa).

# 3) Graficar los residuos para estudiar la variabilidad de los puntos.
b_1 <- modelo$coefficients[2]
b_0 <- modelo$coefficients[1]
residuos <- muestra_hombres[["Weight"]] - (b_1 * muestra_hombres[["Hip.Girth"]] + b_0)
muestra_hombres <- data.frame(muestra_hombres, residuos)
g_var <- ggscatter(muestra_hombres, x = "Hip.Girth", y = "residuos", color = "blue", fill = "blue",
                   xlab = "Grosor a la altura de las caderas", ylab = "Residuos")
g_var <- g_var + geom_hline(yintercept = 0, colour = "red")
print(g_var)

# Se puede apreciar en la grafica que la variabilidad de los residuos
# es relativamente constante.

# 4) Las observaciones son independientes entre sí, pues han sido seleccionadas
# de manera aleatoria y corresponden a menos del 10% de la población.

# Por lo tanto, se cumplen todas las condiciones para el primer modelo de RLS.

# Evaluar modelo.
# Obtener residuos y estadísticas de influencia de los casos.
eval.rls <- data.frame(predicted.probabilities = fitted(modelo[["finalModel"]]))
eval.rls[["standardized.residuals"]] <- rstandard(modelo[["finalModel"]])
eval.rls[["studentized.residuals"]] <-rstudent(modelo[["finalModel"]])
eval.rls[["cooks.distance"]] <- cooks.distance(modelo[["finalModel"]])
eval.rls[["dfbeta"]] <- dfbeta(modelo[["finalModel"]])
eval.rls[["dffit"]] <- dffits(modelo[["finalModel"]])
eval.rls[["leverage"]] <- hatvalues(modelo[["finalModel"]])
eval.rls[["covariance.ratios"]] <- covratio(modelo[["finalModel"]])

cat("Influencia de los casos:\n")

# 95% de los residuos estandarizados deberían estar entre −1.96 y +1.96, y 99%
# entre -2.58 y +2.58.
sospechosos1 <- which(abs(eval.rls[["standardized.residuals"]]) > 1.96)
cat("- Residuos estandarizados fuera del 95% esperado: ")
print(sospechosos1)

# Observaciones con distancia de Cook mayor a uno.
sospechosos2 <- which(eval.rls[["cooks.distance"]] > 1)
cat("- Residuos con distancia de Cook mayor que 1: ")
print(sospechosos2)

# Observaciones con apalancamiento superior al doble del apalancamiento
# promedio: (k + 1)/n.
apalancamiento.promedio <- (ncol(muestra_hombres) / nrow(muestra_hombres))
sospechosos3 <- which(eval.rls[["leverage"]] > 2 * apalancamiento.promedio)

cat("- Residuos con apalancamiento fuera de rango (promedio = ",
    apalancamiento.promedio, "): ", sep = "")

print(sospechosos3)

# DFBeta debería ser < 1.
sospechosos4 <- which(apply(eval.rls[["dfbeta"]] >= 1, 1, any))
names(sospechosos4) <- NULL
cat("- Residuos con DFBeta mayor que 1: ")
print(sospechosos4)

# Finalmente, los casos no deberían desviarse significativamente
# de los límites recomendados para la razón de covarianza:
# CVRi > 1 + [3(k + 1)/n]
# CVRi < 1 – [3(k + 1)/n]
CVRi.lower <- 1 - 3 * apalancamiento.promedio
CVRi.upper <- 1 + 3 * apalancamiento.promedio

sospechosos5 <- which(eval.rls[["covariance.ratios"]] < CVRi.lower |
                        eval.rls[["covariance.ratios"]] > CVRi.upper)

cat("- Residuos con razón de covarianza fuera de rango ([", CVRi.lower, ", ",
    CVRi.upper, "]): ", sep = "")

print(sospechosos5)

sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4,
                 sospechosos5)

sospechosos <- sort(unique(sospechosos))
cat("\nResumen de observaciones sospechosas:\n")

print(round(eval.rls[sospechosos,
                     c("cooks.distance", "leverage", "covariance.ratios")],
            3))

# leverage: valor que va de 0 a 1, si toma el valor 1 entonces la influencia ejercida por el punto es total

# Si bien hay algunas observaciones que podrían considerarse atípicas, la
# distancia de Cook para todas ellas se aleja bastante de 1, por lo que no
# deberían ser causa de preocupación.

# El modelo explica el 79,6% de la
# variabilidad en el rendimiento, lo que sugiere que tiene un buen ajuste con los datos/ su bondad de ajuste
# es relativamente alto con respecto a las observaciones.

################################################################################
# Se comprueban las condiciones para el segundo modelo de RLM:

# Condiciones:
# 1. Las variables predictoras deben ser cuantitativas o dicotomicas.
# 2. La variable de respuesta debe ser cuantitativa y continua, sin 
#    restricciones para su variabilidad.
# 3. Los predictores deben tener algún grado de variabilidad (su varianza no 
#    debe ser igual a cero). En otras palabras, no pueden ser constantes.
# 4. Cada predictor se relaciona linealmente con la variable de respuesta.
# 5. Los valores de la variable de respuesta son independientes entre sí. 

# 6. No debe existir autocorrelación en los residuos.
# 7. Los residuos deben seguir una distribución cercana a la normal centrada en cero.
# 8. Los residuos deben ser homocedásticos (con varianzas similares) para cada 
#    nivel de los predictores.
# 9. No debe existir multicolinealidad. Esto significa que no deben existir 
#    relaciones lineales fuertes entre dos o más predictores 
#    (coeficientes de correlación altos).  

# Verificación de condiciones:
# 1) Las variables predictoras son cuantitativas, ya que cada uno de los predictores son 'medidas' numéricas (centímetros).
# 2) La variable respuesta (peso) es cuantitativa (cm) y continua.
# 3) Los predictores poseen variabilidad ya que no son constantes (son medidas que varían).
# 4) Se estudia la correlación entre la variable respuesta y predictoras:
correlaciones <- round(cor(x = muestra_hombres_2, method = "pearson"), 3)

# Se puede ver que existe una relación lineal fuerte entre la mayoría de los predictores y 
# la variable respuesta (Weight), siendo el predictor Elbows.diameter aquel con la menor 
# correlación (0.578), teniendo una relación moderada con la variable respuesta.

# 5) Las observaciones son independientes entre sí, pues han sido seleccionadas
#    de manera aleatoria y corresponden a menos del 10% de la población. 

# 6) Comprobando la independencia de los residuos:
cat("\nPrueba de Durbin-Watson para autocorrelaciones.")
cat("entre errores:\n")
print(durbinWatsonTest(modelo_final))
# p-value = 0.92, por lo que podemos concluir que los residuos son, en efecto, independientes.

# 7) Comprobando la normalidad de los residuos:
cat("\nPrueba de normalidad para los residuos:\n")
print(shapiro.test(modelo_final$residuals))
# p-value = 0.07705, por lo tanto, se cumple la normalidad de los residuos manteniendo 
# cautela por la cercanía con el nivel de significancia.

# 8) Comprobando la homocedasticidad de los residuos:
cat("\nPrueba de homocedasticidad para los residuos:\n")
print(ncvTest(modelo_final))
#  p = 0.20373, por lo que podemos concluir que el supuesto de homocedasticidad se cumple.

# 9) Comprobando la multicoleanidad:
vifs <- vif(modelo_final)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs: \n")
print(vifs)
cat("- Tolerancias:\n")
print(1 / vifs)
cat("- VIF medio:", mean(vifs), "\n")

# Si se miran los factores de inflacion de varianza, en general no parecen ser preocupantes (vifs < 4).
# Sin embargo, los estadisticos de la tolerancia son preocupantes, ya que algunos no superan el valor
# de 0.4. Adicionalmente, el VIF promedio tambien indica que el modelo podria estar sesgado, ya que
# es un valor mayor a 1 ( VIF medio = 2.461). Es necesario buscar un modelo mas confiable.

# Se elimina la variable menos significativa, aquella con la menor tolerancia, y se vuelve a comprobar
# la multicoleanidad. El proceso se repite hasta que se cumpla la condición.

modelo_final <- update(modelo_final, . ~ .  -Waist.Girth)

vifs <- vif(modelo_final)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs: \n")
print(vifs)
cat("- Tolerancias:\n")
print(1 / vifs)
cat("- VIF medio:", mean(vifs), "\n")

modelo_final <- update(modelo_final, . ~ .  -Hip.Girth)

vifs <- vif(modelo_final)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs: \n")
print(vifs)
cat("- Tolerancias:\n")
print(1 / vifs)
cat("- VIF medio:", mean(vifs), "\n")

modelo_final <- update(modelo_final, . ~ .  -Ankle.Minimum.Girth)

vifs <- vif(modelo_final)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs: \n")
print(vifs)
cat("- Tolerancias:\n")
print(1 / vifs)
cat("- VIF medio:", mean(vifs), "\n")

modelo_final <- update(modelo_final, . ~ .  -Elbows.diameter)

vifs <- vif(modelo_final)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs: \n")
print(vifs)
cat("- Tolerancias:\n")
print(1 / vifs)

# Se consigue obtener un VIF medio = 1.068387, mediante un modelo con 2 predictores:
# Chest.Girth y Biiliac.diameter, por lo que podemos concluir que la multicolinealidad
# se cumple.

# OCTAVO PASO
# Evaluar el poder predictivo del modelo en datos no utilizados para construirlo 
# (o utilizando validación cruzada).

# Utilizando validación cruzada.
# Evaluando poder predictivo para modelo RLS:

# Crear conjunto de entrenamiento y prueba.
set.seed(101)
n_RLS <- nrow(muestra_hombres)
n_entrenamientoRLS <- floor(0.7 * n_RLS)
muestra_RLS <- sample.int(n=n_RLS, size=n_entrenamientoRLS, replace = F)
entrenamiento_RLS <- muestra_hombres[muestra_RLS, ]
prueba_RLS <- muestra_hombres[-muestra_RLS, ]

# Ajustar modelo usando validación cruzada de 5 pliegues.
modelo_RLS <- train(Weight ~ Hip.Girth, data = entrenamiento_RLS, method = "lm",
                    trControl = trainControl(method = "cv", number = 5))

print(summary(modelo_RLS))

# Hacer predicciones para el conjunto de entrenamiento.
predicciones_entrenamiento_RLS <- predict(modelo_RLS, entrenamiento_RLS)

# Calcular error cuadrado promedio para el conjunto de entrenamiento
error_entrenamiento_RLS <- entrenamiento_RLS[["Weight"]] - predicciones_entrenamiento_RLS
mse_entrenamiento_RLS <- mean(error_entrenamiento_RLS **2)

# Hacer predicciones para el conjunto de prueba.
predicciones_prueba_RLS <- predict(modelo_RLS, prueba_RLS)

# Calcular error cuadrado promedio para el conjunto de prueba
error_prueba_RLS <- prueba_RLS[["Weight"]] - predicciones_prueba_RLS
mse_prueba_RLS <- mean(error_prueba_RLS **2)

cat("Error cuadratico medio para RLS:", "\n")
cat("MSE para conjunto de entrenamiento: ", mse_entrenamiento_RLS, "\n")
cat("MSE para conjunto de prueba: ", mse_prueba_RLS, "\n")
# entrenamiento:  20.96419
# prueba: 24.22482

# Evaluando poder predictivo para modelo RLM:

# Crear conjunto de entrenamiento y prueba.
set.seed(101)
n_RLM <- nrow(muestra_hombres_2)
n_entrenamientoRLM <- floor(0.7 * n_RLM)
muestra_RLM <- sample.int(n=n_RLM, size=n_entrenamientoRLM, replace = F)
entrenamiento_RLM <- muestra_hombres_2[muestra_RLM, ]
prueba_RLM <- muestra_hombres_2[-muestra_RLM, ]

# Ajustar modelo usando validación cruzada de 5 pliegues.
modelo_RLM <- train(Weight ~ Chest.Girth + Biiliac.diameter, data = entrenamiento_RLM, method = "lm",
                    trControl = trainControl(method = "cv", number = 5))

print(summary(modelo_RLM))

# Hacer predicciones para el conjunto de entrenamiento.
predicciones_entrenamiento_RLM <- predict(modelo_RLM, entrenamiento_RLM)

# Calcular error cuadrado promedio para el conjunto de entrenamiento
error_entrenamiento_RLM <- entrenamiento_RLM[["Weight"]] - predicciones_entrenamiento_RLM
mse_entrenamiento_RLM <- mean(error_entrenamiento_RLM **2)

# Hacer predicciones para el conjunto de prueba.
predicciones_prueba_RLM <- predict(modelo_RLM, prueba_RLM)

# Calcular error cuadrado promedio para el conjunto de prueba
error_prueba_RLM <- prueba_RLM[["Weight"]] - predicciones_prueba_RLM
mse_prueba_RLM <- mean(error_prueba_RLM **2)

cat("Error cuadratico medio para RLM:", "\n")
cat("MSE para conjunto de entrenamiento: ", mse_entrenamiento_RLM, "\n")
cat("MSE para conjunto de prueba: ", mse_prueba_RLM, "\n")
# entrenamiento:  28.34433
# prueba:  23.44724

# Como se puede observar, utilizando la semilla 101, tanto en el modelo RLS como RLM los 
# valores del MSE para el conjunto de entrenamiento y prueba son parecidos, 
# no llegando a una diferencia superior a 5 para ambos casos. En conclusión,
# ambos modelos son generalizables.


