library(dplyr)
library(car)
library(pROC)

# Lectura de datos
dir <- "C:/Users/dgrdo/Documents/repo_ime_local/EP14"
basename <- "EP13 Datos.csv"
file <- file.path(dir, basename)
datos <- read.csv2(file = file)

# Trabajando el IMC y creando la variable EN
IMC <- datos$Weight/((datos$Height/100)**2)
IMC <- data.frame(IMC)

datos <- cbind(datos,IMC)

# Crear la variable dicotómica EN (estado nutricional)
# 1: Sobrepeso
# 0: No sobrepeso
condicion <- ifelse(datos[["IMC"]] >= 25, 1,  0)

# Se transforma EN en variable categórica
datos[["EN"]] <- factor(condicion)

# Función para calcular la matriz de confusión
matriz_confusion <- function(modelo_rl, datos_vars){
  predicted_value <- predict(modelo_rl,datos_vars,type = "response")
  predicted_class <- ifelse(predicted_value>0.5, "1", "0")
  performance_data<-data.frame(observed=datos_vars$EN,
                               predicted= predicted_class)
  positive <- sum(performance_data$observed=="1")
  negative <- sum(performance_data$observed=="0")
  predicted_positive <- sum(performance_data$predicted=="1")
  predicted_negative <- sum(performance_data$predicted=="0")
  total <- nrow(performance_data)
  tp<-sum(performance_data$observed=="1" & performance_data$predicted=="1")
  tn<-sum(performance_data$observed=="0" & performance_data$predicted=="0")
  fp<-sum(performance_data$observed=="0" & performance_data$predicted=="1")
  fn<-sum(performance_data$observed=="1" & performance_data$predicted=="0")
  accuracy <- (tp+tn)/total
  error_rate <- (fp+fn)/total
  sensitivity <- tp/positive
  especificity <- tn/negative
  precision <- tp/predicted_positive
  npv <- tn / predicted_negative
  return (data.frame(accuracy,error_rate,sensitivity,especificity,precision,npv))
}

#________________________________________________________________________________________________________#
# PARTE 1: Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos del RUN 
# (sin considerar el dígito verificador) del integrante de mayor edad del equipo.

set.seed(8631)
#Semilla impar -> Se seleccionan 120 hombres

#________________________________________________________________________________________________________#
# PARTE 2: Seleccionar una muestra de 120 mujeres (si la semilla es un número par) o 
# 120 hombres (si la semilla es impar), asegurando que la mitad tenga estado nutricional “sobrepeso” y la otra 
# mitad “no sobrepeso”. Dividir esta muestra en dos conjuntos: los datos de 80 personas(40 con EN “sobrepeso”) 
# para utilizar en la construcción de los modelos y 40 personas (20 con EN “sobrepeso”) para poder evaluarlos.

# Se toma una muestra de 120 hombres (60 sobrepeso y 60 sin sobrepeso)
datos <- datos %>% filter(Gender == "1")
hombres_sobrepeso <- datos %>% filter(datos$EN == 1)
hombres_no_sobrepeso <- datos %>% filter(datos$EN == 0)

# Se agregan ambos tipos de personas a un solo conjunto de datos
datos_hombres <- rbind(sample_n(hombres_no_sobrepeso, 60),sample_n(hombres_sobrepeso, 60))

# Se eligen 80 datos para la construcción del modelo (40 sobrepeso y 40 sin sobrepeso)
entrenamiento <- datos_hombres[c(1:40,61:100),]
# Se reordenan los datos
rows_e <- sample(nrow(entrenamiento))
entrenamiento <- entrenamiento[rows_e, ]

# Se eligen los restantes 40 datos para la evaluación del modelo (20 sobrepeso y 20 sin sobrepeso)
prueba <- datos_hombres[c(41:60, 101:120),]
# Se reordenan los datos
rows_p <- sample(nrow(prueba))
prueba <- prueba[rows_p, ]

#________________________________________________________________________________________________________#

# PARTE 3: Recordar las ocho posibles variables predictoras seleccionadas de forma aleatoria en 
# el ejercicio anterior.

# Variables aleatorias a no considerar
# "Forearm.Girth"       "Ankle.Minimum.Girth" "Biiliac.diameter"    "Elbows.diameter"     "Height"             
# "Bicep.Girth"         "Hip.Girth"           "Wrist.Minimum.Girth"

# Se agregan al vector de las 8 variables aletorias a no considerar, las variables Weight, Gender y EN.
# Este vector se utiliza para quitar las variables que no son necesarias para la matriz de correlación
# de la parte 4.
variables_quitar <- c("Forearm.Girth","Ankle.Minimum.Girth","Biiliac.diameter","Elbows.diameter","Bicep.Girth","Hip.Girth",
                      "Wrist.Minimum.Girth","Height","Weight","Gender","EN")

# Se quitan de los datos de construcción del modelo inicial las variables que no se consideraran
datos_construccion_sin_aleatorias <- select(entrenamiento, -one_of(variables_quitar))

#________________________________________________________________________________________________________#

# PARTE 4: Seleccionar, de las otras variables, una que el equipo considere que podría ser útil 
# para predecir la clase EN, justificando bien esta selección.

# Justificación
# Dado que el IMC tiene una directa relación con el estado nutricional, se utiliza la matriz de 
# correlación para obtener la variable con mayor correlación con el IMC y esta será la escogida
# para ajustar el modelo.

matriz_correlacion <- as.data.frame(cor(datos_construccion_sin_aleatorias))

correlacion_peso <- matriz_correlacion%>%select(c(IMC))%>%filter(IMC!=1)

correlacion_peso <- correlacion_peso%>%filter(IMC==max(abs(correlacion_peso)))

cat("La varible con la mayor correlación al IMC es:", row.names(correlacion_peso))

#________________________________________________________________________________________________________#

# PARTE 5: Usando el entorno R y paquetes estándares, construir un modelo de regresión logística con el 
# predictor seleccionado en el paso anterior y utilizando de la muestra obtenida.

# Se crea el modelo de regresión logística
modelo <- glm(EN ~ Waist.Girth, family = binomial(link = "logit"), data =  entrenamiento)
print(summary(modelo))

#_________________________  CONDICIONES REGRESIÓN LOGÍSTICA (1 predictor)  _______________________________________#
cat("Independencia de los residuos\n")
cat("-----------------------------------------\n")
print(durbinWatsonTest(modelo))

# Detectar posibles valores atípicos
cat("Identificación posibles valores atípicos\n")
cat("-----------------------------------------\n")
plot(modelo)

# ----------------Análisis condiciones regresión logística (1 predictor)  ----------------
# Independencia de los residuos
# El valor p obtenido por la prueba de DurbinWatson es 0.734, el cual es bastante mayor a un alfa = 0.05,
# por lo que se cumple la independencia de los residuos

# No hay Verificación de multicolinealidad al ser un solo predictor.

# Información incompleta
# Esta condición es muy complicada de verificar, debido a que pueden existir muchas variables que pueden
# afectar la variable de respuesta, en este caso EN, además mientras más variables se consideren
# más problemas tendremos para encontrar datos que cumplan con las condiciones.
# Por esta razón, se asumirá que se cumple y así poder aplicar la regresión.

# Separación perfecta
# No se observa la existencia de separación perfecta, por lo que se cumple esta condición.

# Valores atípicos 
# Al observar los gráficos obtenidos del modelo con 1 predictor, se tiene que hay una sola observación (numero 39)
# que se aleja significativamente de las demás, esta también se aleja bastante de la recta esperada
# en el gráfico Q-Q de los residuos. A pesar de esto, al observar el gráfico de apalancamiento, se tiene
# que la observación 39 no se encuentra dentro del rango preocupante de la distancia de Cook, por lo que
# no es extremadamente preocupante, y por lo tanto se puede optar por no eliminarla.

#________________________________________________________________________________________________________#
# PARTE 6: Usando herramientas estándares para la exploración de modelos del entorno R, buscar entre dos
# y cinco predictores de entre las variables seleccionadas al azar, recordadas en el punto 3, para agregar
# al modelo obtenido en el paso 5.

# El vector de variables aleatorias tiene solo 7 de las 8 variables aleatorias de la parte 3 dado que Height no puede 
# considerarse dentro de las variables a utilizar en la parte 6. Se agregan a este vector las variables EN y Waist.Girth
# para construir el modelo completo.
variables_aleatorias <- c("Forearm.Girth","Ankle.Minimum.Girth","Biiliac.diameter","Elbows.diameter","Bicep.Girth",
                       "Hip.Girth","Wrist.Minimum.Girth", "Waist.Girth", "EN")

# Se crean los datos de entrenamiento y prueba con las variables aleatorias
entrenamiento_var_aleatorias <- select(entrenamiento, one_of(variables_aleatorias))

prueba_var_aleatorias <- select(prueba, one_of(variables_aleatorias))

# Se ajusta el modelo completo
cat("\n\n")
completo <- glm(EN ~ ., family = binomial(link = "logit"), data =  entrenamiento_var_aleatorias)

# Se utiliza regresión escalonada para obtener el mejor modelo con las variables disponibles
cat("Modelo con regresión escalonada\n")
cat("-----------------------------------------\n")
modelo_mejor <- step(modelo, scope = list(lower = modelo,upper = completo), direction = "both", trace = 0)
print(summary(modelo_mejor))

# El modelo resultante contiene las variables Waist.Girth, Forearm.Girth y Elbows.diameter

#_________________________  CONDICIONES REGRESIÓN LOGÍSTICA MÚLTIPLE (3 predictores)  _______________________________________#
cat("Independencia de los residuos\n")
cat("-----------------------------------------\n")
print(durbinWatsonTest(modelo_mejor))

cat("Verificación de multicolinealidad\n")
cat("-----------------------------------------\n")
cat("\n VIF: \n")
vifs <- vif(modelo_mejor)
print(vifs)
cat("\n Promedio VIF: ")
print(mean(vifs))

# Detectar posibles valores atípicos
cat("Identificación posibles valores atípicos\n")
cat("-----------------------------------------\n")
plot(modelo_mejor)

# ----------------Análisis condiciones regresión logística múltiple (3 predictores)  ----------------
# Independencia de los residuos
# El valor p obtenido por la prueba de DurbinWatson es 0.912, el cual es bastante mayor a un alfa = 0.05,
# por lo que se cumple la independencia de los residuos

# Multicolinealidad
# Al observar los vifs obtenidos para cada variable, en general no son preocupantes, por lo que se verifica
# la condición de multicolinealidad
# Sin embargo, dado que el VIF promedio es mayor a 1, el modelo podría estar sesgado.

# Información incompleta
# Esta condición es muy complicada de verificar, debido a que pueden existir muchas variables que pueden
# afectar la variable de respuesta, en este caso EN, además mientras más variables se consideren
# más problemas tendremos para encontrar datos que cumplan con las condiciones.
# Por esta razón, se asumirá que se cumple y así poder aplicar la regresión.

# Separación perfecta
# No se observa la existencia de separación perfecta, por lo que se cumple esta condición

# Valores atípicos 
# Al observar los gráficos obtenidos del mejor modelo, se tiene que hay una sola observación (numero 39)
# que se aleja significativamente de las demás, la cual también se aleja bastante de la recta esperada
# en el gráfico Q-Q de los residuos. A pesar de esto, al observar el gráfico de apalancamiento, se tiene
# que la observación 39 no se encuentra dentro del rango preocupante de la distancia de Cook, por lo que
# no es extremadamente preocupante, y por lo tanto se puede optar por no eliminarla.

#________________________________________________________________________________________________________#
# Parte 7: Evaluar la confiabilidad de los modelos (i.e. que tengan un buen nivel de ajuste 
# y son generalizables) y “arreglarlos” en caso de que tengan algún problema.

# EVALUACIÓN MODELO REGRESIÓN LOGÍSTICA (1 predictor)
# Obtener los residuos y las estadísticas
resultados_1 <- data.frame(predicted.probabilities = fitted(modelo))
resultados_1[["residuos_estandarizados"]] <- rstandard(modelo)
resultados_1[["residuos_estudiantizados"]] <- rstudent(modelo)
resultados_1[["distancia_Cook"]] <- cooks.distance(modelo)
resultados_1[["dfbeta"]] <- dfbeta(modelo)
resultados_1[["dffit"]] <-  dffits(modelo)
resultados_1[["apalancamiento"]] <- hatvalues(modelo)

# Evaluar los residuos estandarizados que escapen de la normalidad
# 95% de los residuos estandarizados deberían estar entre
# -1.96 y 1.96, y 99% entre -2.58 y 2.58
sospechosos1_1 <- which(abs(resultados_1[["residuos_estandarizados"]]) > 1.96)
sospechosos1_1 <- sort(sospechosos1_1)
cat("\n\n")
cat("Residuos estandarizados fuera del 95% esperado\n")
cat("-----------------------------------------------\n")
print(rownames(entrenamiento_var_aleatorias[sospechosos1_1,]))

# Observaciones con distancia de Cook mayor de uno
sospechosos2_1 <- which(resultados_1[["cooks.distance"]] > 1)
sospechosos2_1 <- sort(sospechosos2_1)
cat("\n\n")
cat("Residuales con distancia de Cook alta\n")
cat("-----------------------------------------------\n")
print(rownames(entrenamiento_var_aleatorias[sospechosos2_1,]))

# Casos cuyo apalancamiento sea más del doble o triple del apalancamiento promedio
apal_medio_1 <- (ncol(entrenamiento_var_aleatorias) + 1) / nrow(entrenamiento_var_aleatorias)
sospechosos3_1 <- which(resultados_1[["apalancamiento"]]>2 * apal_medio_1)
sospechosos3_1 <- sort(sospechosos3_1)

cat("\n\n")
cat("Residuales con leverage fuera de rango (> 2 * apalancamiento medio)\n")
cat(round(apal_medio_1, 3), ")", "\n", sep = "")
cat("-----------------------------------------------\n")
print(rownames(entrenamiento_var_aleatorias[sospechosos3_1,]))

# Revisar casos con DFBeta >=1
sospechosos4_1 <- which(apply(resultados_1[["dfbeta"]] >= 1, 1, any))
sospechosos4_1 <- sort(sospechosos4_1)
names(sospechosos4_1) <- NULL
cat("\n\n")
cat("Residuales con DFBeta sobre 1\n")
cat("-----------------------------------------------\n")
print(rownames(entrenamiento_var_aleatorias[sospechosos4_1,]))

# Detalle de observaciones posiblemente atípicas
sospechosos_1 <- c(sospechosos1_1, sospechosos2_1, sospechosos3_1, sospechosos4_1)
sospechosos_1 <- sort(unique(sospechosos_1))
cat("\n\n")
cat("Casos sospechosos\n")
cat("-----------------------------------------------\n")
cat("\n\n")
print(resultados_1[sospechosos_1, ])

# ----------------Análisis evaluación modelo de regresión logística (1 predictor) ----------------
# Al mostrar la información del modelo con un predictor por pantalla podemos observar que se tiene un 
# AIC: 78.262 y una desviación (78 grados de libertad) de 74.262.

# A partir de la evaluación, se obtienen 3 datos sospechosos, dentro de estos datos lo más preocupante
# puede ser el valor dfbeta.(Intercept) que es mayor a 1 en las 3 observaciones, sin embargo,
# la distancia de Cook estimada para todas estas observaciones potencialmente influyentes 
# están muy lejos de sobrepasar el valor recomendado, por lo que en este caso no 
# parece ser necesario quitar observaciones.

# ---------------------------------------------------------------------------------------------------
# EVALUACIÓN MODELO REGRESIÓN LOGÍSTICA MÚLTIPLE (3 predictores)

# Obtener los residuos y las estadísticas
resultados <- data.frame(predicted.probabilities = fitted(modelo_mejor))
resultados[["residuos_estandarizados"]] <- rstandard(modelo_mejor)
resultados[["residuos_estudiantizados"]] <- rstudent(modelo_mejor)
resultados[["distancia_Cook"]] <- cooks.distance(modelo_mejor)
resultados[["dfbeta"]] <- dfbeta(modelo_mejor)
resultados[["dffit"]] <-  dffits(modelo_mejor)
resultados[["apalancamiento"]] <- hatvalues(modelo_mejor)

# Evaluar los residuos estandarizados que escapen de la normalidad
# 95% de los residuos estandarizados deberían estar entre
# -1.96 y 1.96, y 99% entre -2.58 y 2.58
sospechosos1 <- which(abs(resultados[["residuos_estandarizados"]]) > 1.96)
sospechosos1 <- sort(sospechosos1)
cat("\n\n")
cat("Residuos estandarizados fuera del 95% esperado\n")
cat("-----------------------------------------------\n")
print(rownames(entrenamiento_var_aleatorias[sospechosos1,]))

# Observaciones con distancia de Cook mayor de uno
sospechosos2 <- which(resultados[["cooks.distance"]] > 1)
sospechosos2 <- sort(sospechosos2)
cat("\n\n")
cat("Residuales con distancia de Cook alta\n")
cat("-----------------------------------------------\n")
print(rownames(entrenamiento_var_aleatorias[sospechosos2,]))

# Casos cuyo apalancamiento sea más del doble o triple del apalancamiento promedio
apal_medio <- (ncol(entrenamiento_var_aleatorias) + 1) / nrow(entrenamiento_var_aleatorias)
sospechosos3 <- which(resultados[["apalancamiento"]]>2 * apal_medio)
sospechosos3 <- sort(sospechosos3)

cat("\n\n")
cat("Residuales con leverage fuera de rango (> 2 * apalancamiento medio)\n")
cat(round(apal_medio, 3), ")", "\n", sep = "")
cat("-----------------------------------------------\n")
print(rownames(entrenamiento_var_aleatorias[sospechosos3,]))

# Revisar casos con DFBeta >=1
sospechosos4 <- which(apply(resultados[["dfbeta"]] >= 1, 1, any))
sospechosos4 <- sort(sospechosos4)
names(sospechosos4) <- NULL
cat("\n\n")
cat("Residuales con DFBeta sobre 1\n")
cat("-----------------------------------------------\n")
print(rownames(entrenamiento_var_aleatorias[sospechosos4,]))

# Detalle de observaciones posiblemente atípicas
sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4)
sospechosos <- sort(unique(sospechosos))
cat("\n\n")
cat("Casos sospechosos\n")
cat("-----------------------------------------------\n")
cat("\n\n")
print(resultados[sospechosos, ])

# ----------------Análisis evaluación modelo de regresión logística múltiple (3 predictores) ----------------
# Al mostrar la información del modelo con 3 predictores por pantalla podemos observar que se tiene un 
# AIC: 64.67 y una desviación (76 grados de libertad) de 56.67.
# Al comparar los valores obtenidos con el modelo de un predictor podemos observar que tanto el AIC como
# la desviación han disminuido, por lo que es posible decir que el modelo de 3 predictores debería ser mejor
# que el de un solo predictor.

# A partir de la evaluación, se obtienen 9 datos sospechosos, dentro de estos datos lo más preocupante
# puede ser el valor dfbeta.(Intercept) que es mayor a 1 en 8 de las 9 observaciones, sin embargo,
# la distancia de Cook estimada para todas estas observaciones potencialmente influyentes 
# están muy lejos de sobrepasar el valor recomendado, por lo que en este caso no 
# parece ser necesario quitar observaciones.

#________________________________________________________________________________________________________#
# Parte 8: Usando código estándar, evaluar el poder predictivo de los modelos con los datos de las 40 
# personas que no se incluyeron en su construcción en términos de sensibilidad y especificidad.

# Modelo regresión logística simple (1 predictor)

# Evaluación del poder predictivo del modelo (1 predictor) con el conjunto de entrenamiento
cat("Evaluación del modelo con el conjunto de entrenamiento\n")
probs_e <- predict(modelo, entrenamiento_var_aleatorias, type = "response")

umbral <- 0.5
preds_e <- sapply(probs_e, function(p) ifelse(p>=umbral, "1", "0"))
preds_e <- factor(preds_e, levels = levels(datos_hombres[["EN"]]))

ROC_e <- roc(entrenamiento_var_aleatorias[["EN"]], probs_e)
plot(ROC_e)

# Evaluación del poder predictivo del modelo (1 predictor) con el conjunto de prueba
cat("Evaluación del modelo con el conjunto de prueba\n")
probs_p <- predict(modelo, prueba_var_aleatorias, type = "response")

preds_p <- sapply(probs_p, function(p) ifelse(p>=umbral, "1", "0"))
preds_p <- factor(preds_p, levels = levels(datos_hombres[["EN"]]))

ROC_p <- roc(prueba_var_aleatorias[["EN"]], probs_p)
plot(ROC_p)

# Resultados matriz de confusión con datos de entrenamiento modelo 1 predictor
resultados_modelo_simple_train <- matriz_confusion(modelo, entrenamiento_var_aleatorias)

# Resultados matriz de confusión con datos de prueba modelo 1 predictor
resultados_modelo_simple_test <- matriz_confusion(modelo, prueba_var_aleatorias)

cat("Resultados matriz de confusión modelo simple\n\n Entrenamiento: \n")
print(resultados_modelo_simple_train)
cat("\n\n Prueba: \n") 
print(resultados_modelo_simple_test)

# -------------------- Análisis del valor predictivo del modelo simple ------------------------------
# En este caso al comparar las curvas ROC producidas usando los datos de entrenamiento y de prueba
# pareciera que la curva más alejada de la diagonal es la de los datos de entrenamiento.
# Sin embargo, al comparar los resultados de la matriz de confusión, se observa que la sensitividad
# se mantiene y la especificidad aumenta levemente de 0,75 a 0,8 .
# Con esta información podemos inferir que el modelo de un solo predictor es relativamente bueno en su
# poder predictivo, ya que lo mantiene al usar los datos de prueba.
#  -------------------- -------------------- -------------------- -------------------- --------------------

# Modelo de 3 predictores
# Evaluación del poder predictivo del modelo (3 predictores) con el conjunto de entrenamiento
cat("Evaluación del modelo con el conjunto de entrenamiento\n")
probs_e <- predict(modelo_mejor, entrenamiento_var_aleatorias, type = "response")

umbral <- 0.5
preds_e <- sapply(probs_e, function(p) ifelse(p>=umbral, "1", "0"))
preds_e <- factor(preds_e, levels = levels(datos_hombres[["EN"]]))

ROC_e <- roc(entrenamiento_var_aleatorias[["EN"]], probs_e)
plot(ROC_e)

# Evaluación del poder predictivo del modelo (3 predictores) con el conjunto de prueba
cat("Evaluación del modelo con el conjunto de prueba\n")
probs_p <- predict(modelo_mejor, prueba_var_aleatorias, type = "response")

preds_p <- sapply(probs_p, function(p) ifelse(p>=umbral, "1", "0"))
preds_p <- factor(preds_p, levels = levels(datos_hombres[["EN"]]))

ROC_p <- roc(prueba_var_aleatorias[["EN"]], probs_p)
plot(ROC_p)

# Resultados matriz de confusión con datos de entrenamiento modelo 3 predictores
resultados_modelo_mult_train <- matriz_confusion(modelo_mejor, entrenamiento_var_aleatorias)

# Resultados matriz de confusión con datos de prueba modelo 3 predictores
resultados_modelo_mult_test <- matriz_confusion(modelo_mejor, prueba_var_aleatorias)

cat("Resultados matriz de confusión modelo múltiple\n\n Entrenamiento: \n")
print(resultados_modelo_mult_train)
cat("\n\n Prueba: \n") 
print(resultados_modelo_mult_test)

# -------------------- Análisis del valor predictivo del modelo múltiple ------------------------------
# En este caso al comparar las curvas ROC producidas usando los datos de entrenamiento y de prueba
# se observa que la curva más alejada de la diagonal es la de los datos de prueba.
# Esto se puede verificar mediante los resultados de la matriz de confusión, en donde es posible observar 
# que la sensitividad se mantiene (0.85) y la especificidad aumenta (de 0,825 a 0,85)

# Esto nos indica que el modelo es bastante bueno, lo cual se verifica por valores de sensitividad muy
# altos (mayores al 80%), esto conlleva a que el comportamiento no solo se mantenga al usar otra muestra,
# si no que incluso mejore. Sin embargo, para comprobar realmente esta mejora debería ser necesario probar
# con otros datos de prueba, debido a que pudo darse el caso de que justo los datos de prueba obtenidos
# se comportan bien con este modelo, pero para otros datos esto no se cumple.
#  -------------------- -------------------- -------------------- -------------------- --------------------



