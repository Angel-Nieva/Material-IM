library(dplyr)
library(caret)
library(leaps)
library(bootES)
library(randomForest)
library(pROC)
library(car)

# basename <- "EP13 Datos.csv"
# file <- file.path("C:/Users/Dell PC/Desktop/IME-2022/Actividades/act15",basename)
# datos <- read.csv2(file = file)

datos <- read.csv2("C:/Users/dpriv/OneDrive/Escritorio/IngInfo/IME/Unidad3/Ep15/EP13 Datos.csv",stringsAsFactors = FALSE)

# El equipo crea la variable IMC (?ndice de masa corporal) como el peso de una persona (en kilogramos) 
# dividida por el cuadrado de su estatura (en metros).
datos <- datos %>% mutate(IMC = Weight/((Height/100)^2))

# Si bien esta variable se usa para clasificar a las personas en varias clases de estado nutricional 
# (bajo peso, normal, sobrepeso, obesidad, obesidad mÃ³rbida), para efectos de este ejercicio, 
# usaremos dos clases: sobrepeso (IMC â‰¥ 25,0) y no sobrepeso (IMC â‰¤ 25,0).

# El equipo crea la variable dicot?mica EN (estado nutricional) de acuerdo al valor de IMC de cada persona.

# Creando la variable dicot?mica EN (estado nutricional) de acuerdo al valor de IMC de cada persona,
# siendo 1 Sobrepeso y 0 No sobrepeso.

datos <- datos %>% mutate(EN = ifelse(IMC >= 25.0, 1, 0))

#1. Definir la semilla a utilizar, que corresponde a los primeros cinco d?gitos del RUN del integrante de mayor edad del equipo.
semilla <- 4097
set.seed(semilla)

#2.Seleccionar una muestra de 100 personas, asegurando que la mitad tenga estado nutricional ?sobrepeso? y la otra mitad ?no sobrepeso?.

sobrepeso <- datos %>% filter(EN == 1) %>% sample_n(50)

no_sobrepeso <- datos %>% filter(EN == 0) %>% sample_n(50)


muestra <- rbind(sobrepeso,no_sobrepeso)

n <- nrow(muestra)
n_entrenamiento <- floor(0.7 * n)
numeros <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
muestra_entrenamiento <- muestra[numeros,]
muestra_prueba <- datos[-numeros,]

#3.Usando las herramientas del paquete leaps, realizar una b?squeda exhaustiva para seleccionar entre dos y ocho predictores que ayuden a estimar la variable Peso (Weight), obviamente sin considerar las nuevas variables IMC ni EN, y luego utilizar las funciones del paquete caret para construir un modelo de regresi?n lineal
# m?ltiple con los predictores escogidos y evaluarlo usando bootstrapping.

muestra_entrenamiento["IMC"] <- NULL
muestra_entrenamiento["Height"] <- NULL
muestra_entrenamiento["EN"] <- NULL
muestra_entrenamiento["Gender"] <- NULL



# Usando regresión 
modelos <- regsubsets(Weight ~ ., data = muestra_entrenamiento, method = "exhaustive",
                      nbest = 2, nvmax = 8)

print(plot(modelos))

# Entonces a partir de lo anterior se puede ver que los predictores
# que le dan mejor BIC al modelo son: 

# Biiliac.diameter
# Elbows.diameter
# Shoulder.Girth
# Waist.Girth
# Thigh.Girth
# Knee.Girth
# Wrist.Minimum.Girth
# Age

B <- 999
modelo <- train(Weight ~ Biiliac.diameter + Elbows.diameter + Shoulder.Girth + Waist.Girth + Thigh.Girth + 
                Knee.Girth + Wrist.Minimum.Girth + Age, data = muestra_entrenamiento, method = "lm",
                trControl = trainControl(method = "boot", number = B, 
                                         savePredictions = TRUE))

print(summary(modelo))

# Hacer predicciones para el conjunto de entrenamiento.
#predicciones_entrenamiento <- predict(modelo, muestra_entrenamiento)

# Calcular error cuadrado promedio para el conjunto de prueba.
#error_entrenamiento <- muestra_entrenamiento[["Weight"]] - predicciones_entrenamiento
#mse_entrenamiento <- mean(error_entrenamiento ** 2)
#cat("MSE para el conjunto de entrenamiento:", mse_entrenamiento, "\n")

# Hacer predicciones para el conjunto de prueba.
#predicciones_prueba <- predict(modelo, muestra_prueba)

#media_y <- mean(predicciones_prueba)

# Calcular error cuadrado promedio para el conjunto de prueba.
# error_prueba <- muestra_prueba[["Weight"]] - predicciones_prueba
# mse_prueba <- mean(error_prueba ** 2)
# cat("MSE para el conjunto de prueba:", mse_prueba)

# 4. Haciendo un poco de investigación sobre el paquete caret, en particular cómo hacer 
# Recursive Feature Elimination (RFE), construir un modelo de regresión lineal múltiple 
# para predecir la variable IMC que incluya entre 10 y 20 predictores, seleccionando el 
# conjunto de variables que maximice R2 y que use cinco repeticiones de validación 
# cruzada de cinco pliegues para evitar el sobreajuste (obviamente no se debe considerar 
# las variables Peso,  Estatura ni estado nutricional â€“Weight, Height, EN respectivamente).

# Control de RFE usando random forest con validación cruzada de 5 pliegues
# con 5 repeticiones
control <- rfeControl(functions = lmFuncs,
                      method = "repeatedcv", 
                      repeats = 5, 
                      number = 5) 

# Variable respuesta.

peso <- muestra$Weight





# Sacando otro conjunto de entrenamiento y prueba para predictores en base a los mismos
# números de posiciones de el vector calculados anteriormente.
muestra_entrenamiento_x <- muestra[numeros,]
muestra_prueba_x <- muestra[-numeros,]


# Construyendo entrenamiento con la respuesta
entrenamiento <- muestra_entrenamiento_x

# Quitando peso
muestra_entrenamiento_x  <- muestra_entrenamiento_x %>% select(-Weight)



# Sacando conjunto de entrenamiento y prueba para respuesta.
muestra_entrenamiento_y <- peso[numeros]
muestra_prueba_y <- peso[-numeros]

muestra_entrenamiento_x["IMC"] <- NULL
muestra_entrenamiento_x["Height"] <- NULL
muestra_entrenamiento_x["EN"] <- NULL
muestra_entrenamiento_x["Gender"] <- NULL

muestra_prueba_x["IMC"] <- NULL
muestra_prueba_x["Height"] <- NULL
muestra_prueba_x["EN"] <- NULL
muestra_prueba_x["Gender"] <- NULL


rfe1 <- rfe(x = muestra_entrenamiento_x, # Conjunto de entrenamiento predictores
                   y = muestra_entrenamiento_y, # Conjunto de entrenamiento respuesta
                   sizes = c(1:20),
                   rfeControl = control)

rfe1


# A partir de los restultados anteriores, el top de los mejores predictores seleccionados fueron 
# un subconjunto de 5 predictores:
# Wrist.Minimum.Girth
# Wrist.Minimum.Girth
# Elbows.diameter
# Ankles.diameter
# Knee.Girth

modelo2 <- lm(Weight ~ Wrist.Minimum.Girth + Wrists.diameter + Elbows.diameter + Ankles.diameter + Knee.Girth,
              data = entrenamiento)



# Usando RFE, construir un modelo de regresión logística múltiple para la variable EN que incluya el conjunto, de entre dos y seis, predictores que entregue la mejor curva ROC y que utilice validación cruzada dejando uno fuera para evitar el sobreajuste (obviamente no se debe considerar las variables Peso, Estatura –Weight y Height respectivamente– ni IMC).

control2 <- rfeControl(functions = rfFuncs,
                      method = "LOOCV", 
                      repeats = 5, 
                      number = 5) 

# Variable respuesta.

en <- muestra$EN

datos_predictores2 <- muestra %>% select(-Weight,-Gender,-IMC)


# Sacando otro conjunto de entrenamiento y prueba para predictores en base a los mismos
# números de posiciones de el vector calculados anteriormente.
muestra_entrenamiento2_x <- datos_predictores2[numeros,]
muestra_prueba2_x <- datos_predictores2[-numeros,]

# Sacando conjunto de entrenamiento y prueba para respuesta.
muestra_entrenamiento2_y <- en[numeros]
muestra_prueba2_y <- en[-numeros]

# Guardar EN

EN <- muestra_entrenamiento2_x$EN

muestra_entrenamiento2_x <- muestra_entrenamiento2_x %>% select(-EN)

rfe2 <- rfe(x = muestra_entrenamiento2_x, # Conjunto de entrenamiento predictores
            y = muestra_entrenamiento2_y, # Conjunto de entrenamiento respuesta
            sizes = c(1:6),
            rfeControl = control2)

predictors(rfe2)


# A partir de los restultados anteriores, los predictores seleccionados son:
# Chest.Girth 
# Hip.Girth
# Bicep.Girth
# Waist.Girth

# Recuperando columna EN guardada 
entrenamiento_logistico <- muestra_entrenamiento2_x %>% cbind(EN)

modelo_logistico <- glm(EN ~ Chest.Girth + Waist.Girth + Hip.Girth + Bicep.Girth, family = binomial(link = "logit"), data = entrenamiento_logistico)
print(summary(modelo_logistico))

# Conseguir probabilidades para cada valor de la variable predictora en
# el conjunto de entrenamiento.
probs_e <- predict(modelo_logistico, entrenamiento_logistico, type = "response")

umbral <- 0.5
# Aplicar a cada probabilidad predicha la función que determina si pasa
# el umbral o no.
preds_e <- sapply(probs_e, function(p) ifelse(p >= umbral, "1", "0"))
preds_e <- factor(preds_e, levels = levels(entrenamiento_logistico[["EN"]]))

# Evaluación del clasificador: Gráfico ROC y matriz de confusión.


# Es mejor mientras la curva se aleje más de la diagonal.

# Matriz de confusión (VP,FP,VN,FN)
# (predicciones, observados)
# matriz_e <- confusionMatrix(preds_e, entrenamiento_logistico[["EN"]])
# print(matriz_e)


# 6.Pronunciarse sobre la confiabilidad y el poder predictivo de los modelos obtenidos.

# Evaluando modelo con busqueda exhaustiva:
# 1. El modelo tiene predictores cuantitativos y dicotómicos.

# 2. La variable de respuesta (peso) es cuantitativa y continua.

# 3. Haciendo una prueba de Breusch-Pagan-Godfrey para la homocedasticidad de los residuos:


print(ncvTest(modelo$finalModel))

# p = 0.144, por lo tanto las varianzas de los resiudos son iguales considerando
# una significación de 0.05, aunque están al borde.

# 4. Verificando la normalidad de los residuos

print(shapiro.test(modelo$finalModel$residuals))

# p = 0.41 > 0.05, por lo tanto los residuos se distribuyen normal.

# 5. Verificando la condición de independencia de los residuos con Dubrin-Watson.

print(durbinWatsonTest(modelo$finalModel))

# p = 0.61 > 0.05, por lo que los residuos son independientes.

# 6. Verificando multicolinealidad de los predictores.

vifs <- vif(modelo$finalModel)

vifPromedio <- mean(vifs)

tolerancias <- (1/vifs)

# El vif máximo es de 7, se podría considerar observación preocupante los que
# tienen 6 y 7 aunque no llegan a ser 10.

# El vif promedio es 3.8 por lo que puede haber sesgo.

# Hay dos tolerancias que son menores a 0.2, Waist.Girth y Shoulder.Girth, por
# lo que son predictores preocupantes.

# Entonces en general no hay mucha colinealidad entre predictores exceptuando
# por la prueba de tolerancias.

# 7.  Los valores de la variable de respuesta son independientes entre sí y
# viendo los datos los predictores no son constantes, además cada predictor
# se relaciona linealmente con la variable de respuesta.


# Por lo tanto el primer modelo hecho, se puede decir que es generalizable,
# aunque es preocupante lo de la colinealidad entre predictores.

# Probando confiabiliad del modelo con predicciones del conjunto de entrenamiento
# y de prueba determinados anteriormente para el primer modelo:


# Hacer predicciones para el conjunto de entrenamiento.
predicciones_entrenamiento <- predict(modelo, muestra_entrenamiento)

# Calcular error cuadrado promedio para el conjunto de prueba.
error_entrenamiento <- entrenamiento[["Weight"]] - predicciones_entrenamiento
mse_entrenamiento <- mean(error_entrenamiento ** 2)
cat("MSE para el conjunto de entrenamiento:", mse_entrenamiento, "\n")

# Hacer predicciones para el conjunto de prueba.
predicciones_prueba <- predict(modelo, muestra_prueba)

# Calcular error cuadrado promedio para el conjunto de prueba.
error_prueba <- muestra_prueba[["Weight"]] - predicciones_prueba
mse_prueba <- mean(error_prueba ** 2)
cat("MSE para el conjunto de prueba:", mse_prueba)

# MSE entrenamiento = 4.9
# MSE prueba = 9.14

# MSE prueba es mayor que el de entrenamiento, pero no es demasiada la 
# diferencia, aunque está algo ajustado.

# Evaluando modelo2:



# 1. El modelo tiene predictores cuantitativos y dicotómicos.

# 2. La variable de respuesta (peso) es cuantitativa y continua.

# 3. Haciendo una prueba de Breusch-Pagan-Godfrey para la homocedasticidad de los residuos:


print(ncvTest(modelo2))

# p = 0.29, por lo tanto las varianzas de los resiudos son iguales considerando
# una significación de 0.05

# 4. Verificando la normalidad de los residuos

print(shapiro.test(modelo2$residuals))

# p = 0.32 > 0.05, por lo tanto los residuos se distribuyen normal.

# 5. Verificando la condición de independencia de los residuos con Dubrin-Watson.

print(durbinWatsonTest(modelo2))

# p = 0.808 > 0.05, por lo que los residuos son independientes.

# 6. Verificando multicolinealidad de los predictores.

vifs2 <- vif(modelo2)

vifPromedio2 <- mean(vifs2)

tolerancias2 <- (1/vifs2)

# El vif máximo es de 6, se podría considerar observación preocupante pero no
# supera 10 y ninguno
# más tiene valores preocupantes.

# El vif promedio es 3.57 > 1 por lo que puede haber sesgo.

# Hay una tolerancia menor a 0.2: Wrist.Minimum.Girth 

# Entonces en general no hay mucha colinealidad entre predictores exceptuando
# por la prueba de tolerancias y vif promedio.

# 7.  Los valores de la variable de respuesta son independientes entre sí y
# viendo los datos los predictores no son constantes, además cada predictor
# se relaciona linealmente con la variable de respuesta.


# Por lo tanto el segundo modelo hecho con RFE, se puede decir que es generalizable,
# aunque otra vez es preocupante lo de la colinealidad entre predictores.

# Probando confiabiliad del modelo con predicciones del conjunto de entrenamiento
# y de prueba determinados anteriormente para el segundo modelo con RFE:


# Hacer predicciones para el conjunto de entrenamiento.
predicciones_entrenamiento2 <- predict(modelo2, entrenamiento)

# Calcular error cuadrado promedio para el conjunto de prueba.
error_entrenamiento2 <- entrenamiento[["Weight"]] - predicciones_entrenamiento2
mse_entrenamiento2 <- mean(error_entrenamiento2 ** 2)
cat("MSE para el conjunto de entrenamiento RFE:", mse_entrenamiento2, "\n")

# Hacer predicciones para el conjunto de prueba.
predicciones_prueba2 <- predict(modelo2, muestra_prueba_x)

# Calcular error cuadrado promedio para el conjunto de prueba.
error_prueba2 <- muestra_prueba_x[["Weight"]] - predicciones_prueba2
mse_prueba2 <- mean(error_prueba2 ** 2)
cat("MSE para el conjunto de prueba RFE:", mse_prueba2)

# MSE entrenamiento = 31.36

# MSE prueba = 60.56

# Aquí el MSE de prueba es aún mayor que el de entrenamiento y, comparado con el modelo
# anterior,y ambos mucho mayores, por lo que es menos generalizable que él y está 
# sobre ajustado. 

# Evaluando el modelo de regresión logística:


# Graficar ROC de las respuestas observadas y sus probabilidades predichas.

ROC_e <- roc(entrenamiento_logistico[["EN"]], probs_e)
plot(ROC_e)


# Se puede ver que el modelo es un preciso porque se aleja de la diagonal,
# aunque está muy escalonado, por lo que le faltaría un poco más de precisión.

# Viendo bondad de ajuste con log-verosimilitud.
print(logLik(modelo_logistico))

# lnL = -18.56, al ser negativo se podría considerar bajo y por lo tanto
# hay poca diferencia entre las respuestas predichas y observadas.

