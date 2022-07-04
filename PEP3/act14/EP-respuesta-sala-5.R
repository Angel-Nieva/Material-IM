library(boot)
library(ggpubr)
library(ez)
library(tidyverse)
library(dplyr)
library(caret)
library(pROC)
library(car)

# Para esta actividad usaremos la misma muestra de medidas anat?micas seleccionada para el ejercicio
# pr?ctico anterior desde los datos recolectados por Heinz et al. (2003). La Como este ejercicio requiere de
# una variable dicot?mica, vamos a realizar lo siguiente:

alfa <- 0.01

# Lectura de los datos

basename <- "EP13 Datos.csv"
file <- file.path("C:/Users/Dell PC/Desktop/IME-2022/Actividades/act14",basename)
datos <- read.csv2(file = file)

# Seleccionar la muestra de 50 hombres del ejercicio anterior
set.seed(6177)
datos[["Gender"]] <- factor(datos[["Gender"]]) 
variables <- datos %>% filter(Gender == "1")



########################################

# 1. Crear la variable IMC (?ndice de masa corporal) como el peso de una persona (en kilogramos)
# dividida por el cuadrado de su estatura (en metros)

# La nueva variable se agrega usando mutate()
# se divide la altura por 100 pues est? en cent?metros y la necesitamos en metros
variables2 <- variables %>% mutate(IMC = Weight/((Height/100)**2))

# 2.Si bien esta variable se usa para clasificar a las personas en varias clases de estado nutricional
# (bajo peso, normal, sobrepeso, obesidad, obesidad m?rbida), para efectos de este ejercicio,
# usaremos dos clases: sobrepeso (IMC ??? 25,0) y no sobrepeso (IMC < 25,0)


# 3. Crear la variable dicot?mica EN (estado nutricional) de acuerdo al valor de IMC de cada persona

variables3 <- variables2 %>% mutate(EN = ifelse(IMC >= 25.0, "Sobrepeso", "No sobrepeso"))

# Se escogen 25 muestras con valor EN == "Sobrepeso"
variableSobrepeso <- variables3%>%filter(EN == "Sobrepeso")
variableSobrepeso2 <- variableSobrepeso[sample(nrow(variableSobrepeso), 25),]

print(variableSobrepeso2)

# Se escogen 25 muestras con valor EN == "No sobrepeso"
variableNoSobrepeso <- variables3%>%filter(EN == "No sobrepeso")
variableNoSobrepeso2 <- variableNoSobrepeso[sample(nrow(variableNoSobrepeso), 25),]

print(variableNoSobrepeso2)

# Se deja tabla final
final <- merge(x = variableSobrepeso2, y = variableNoSobrepeso2, all = TRUE)
final[["EN"]] <- factor(final[["EN"]], levels = c("Sobrepeso", "No sobrepeso"))

########################################

# 2. Si bien esta variable se usa para clasificar a las personas en varias clases de estado nutricional
# (bajo peso, normal, sobrepeso, obesidad, obesidad m?rbida), para efectos de este ejercicio,
# usaremos dos clases: sobrepeso (IMC ??? 25,0) y no sobrepeso (IMC < 25,0)
# 3. Crear la variable dicot?mica EN (estado nutricional) de acuerdo al valor de IMC de cada persona

variables3 <- variables2 %>% mutate(EN = ifelse(IMC >= 25.0, "Sobrepeso", "No sobrepeso"))
# Se pasan los valores EN a factores
variables3[["EN"]] <- factor(variables3[["EN"]], levels = c("Sobrepeso", "No sobrepeso"))

########################################

# Ahora podemos construir un modelo de regresi?n log?stica para predecir la variable EN, de acuerdo con
# las siguientes instrucciones:
# 1. Recordar las ocho posibles variables predictoras seleccionadas de forma
# aleatoria en el ejercicio anterior

# Las variables aleatorias seleccionadas fueron:
# Hip.Girth = Grosor a la altura de las caderas
# Waist.Girth = Grosor a la altura de la cintura
# Height = Altura
# Ankles.diameter = Suma de los di?metros de los tobillos
# Elbows.diameter = Suma de los di?metros de los codos
# Wrists.diameter = Suma de los di?metros de las mu?ecas
# Calf.Maximum.Girth = Grosor promedio de la parte m?s ancha de las pantorrillas
# Biacromial.diameter = Di?metro biacromial (a la altura de los hombros)

########################################

# 2. Seleccionar, de las otras variables, una que el equipo considere que podr?a ser ?til para predecir la
# clase EN, justificando bien esta selecci?n

# Se selecciona la variable Thigh.Girth (Grosos a la altura de los muslos),
# pues de acuerdo con los datos obtenidos, aquellas personas con sobrepeso
# presentan valores superiores con respecto a la variable Thigh.Girth,
# en comparaci?n con aquellas que no presentan sobrepeso

########################################

# 3. Usando el entorno R, construir un modelo de regresi?n log?stica
# con el predictor seleccionado en el paso anterior

# Se separan los conjuntos de entrenamiento y prueba
n <- nrow(final)
n_entrenamiento <- floor(0.8 * n)
muestra <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- final[muestra, ]
prueba <- final[- muestra, ]

# Ajustar modelo con funci?n train
modelo2 <- train(EN ~ Thigh.Girth,
                 family = binomial(link = "logit"),
                 data = entrenamiento,
                 method = "glm",
                 trControl = trainControl(method = "CV", number = 5, savePredictions = TRUE))



# Se calcula la matriz de confusi?n
matriz_e2 <- confusionMatrix(modelo2$pred$pred, modelo2$pred$obs)
print(matriz_e2)



# 4. Usando herramientas para la exploraci?n de modelos del entorno R, buscar entre dos y cinco
# predictores de entre las variables seleccionadas al azar, recordadas en el punto 3, para agregar al
# modelo obtenido en el paso 3

# Se ajusta el modelo nulo:

nulo <- glm(EN ~ 1,
              family = binomial(link = "logit"),
              data = entrenamiento)

# Se ajusta el modelo completo:

completo <- glm(EN ~ .,
            family = binomial(link = "logit"),
            data = entrenamiento[, c(27, 14, 12, 24, 9, 6, 7, 19, 1)])

mejor <- step(nulo, scope= list(lower = nulo, upper = completo), direction = "both", trace = 0)
print(summary(mejor))

# Los mejores predictores ser?an Hip.Girth, Height, Wrists.diameter y Calf.Maximum.Girth

# EL nuevo modelo completo ser?a:

modelo3 <- train(EN ~ Thigh.Girth + Hip.Girth + Height + Wrists.diameter+ Calf.Maximum.Girth ,
                 family = binomial(link = "logit"),
                 data = entrenamiento,
                 method = "glm",
                 trControl = trainControl(method = "CV", number = 5, savePredictions = TRUE))

# Se calcula la matriz de confusi?n
matriz_e3 <- confusionMatrix(modelo3$pred$pred, modelo3$pred$obs)
print(matriz_e3)

# 5. Evaluar los modelos y "arreglarlos" en caso de que tengan alg?n problema con las condiciones que
# deben cumplir

# Modelo actividad 3


# Condiciones:

# 1) Relaci?n lineal:

# Se pasa variable categ?rica a variable indicadora
EN_numerico <- match(entrenamiento$EN, c("Sobrepeso", "No sobrepeso")) - 1;

print(cor(EN_numerico, entrenamiento[["Thigh.Girth"]]))

# Se puede ver que con un valor de correlaci?n lineal de -0.6235 si existe una relaci?n lineal entre el predictor
# y la respuesta

# 2) Los residuos deben ser independientes entre s?:

# Prueba de independencia:

print(durbinWatsonTest(glm(EN ~ Thigh.Girth,family = binomial(link = "logit"), data = entrenamiento), max.lag = 1))

# Por lo tanto, con un valor p = 0.452 se puede decir que se cumple la condici?n de independencia de los residuos


# 3) Informaci?n incompleta:

# El modelo cuenta con la informaci?n necesaria 

# 4) Separaci?n perfecta:

pp <- ggscatter(data = entrenamiento, x= "Thigh.Girth", y = "EN", color = "EN", Legend= "none")
print(pp)


# Se puede apreciar la existencia de separaci?n Perfecta, esto debido a que se trata de una variable categ?rica. Por lo que
# el algoritmo no converge

# Luego, el modelo no puede ser mejorado al tener ?nicamente un predictor (por el mismo motivo no se revisa la colinealidad).



# Modelo actividad 4

# Se pasa variable categ?rica a variable indicadora
EN_numerico <- match(entrenamiento$EN, c("Sobrepeso", "No sobrepeso")) - 1;

matrizResultados <- entrenamiento %>% select(Thigh.Girth, Hip.Girth, Height, Wrists.diameter, Calf.Maximum.Girth)
matrizResultados <- data.frame(EN_numerico, matrizResultados)

# Condiciones:

# 1) Relaci?n lineal:


print(cor(matrizResultados))

# Se puede ver que los predictores si poseen relaciones lineales con EN

# 2) Los residuos deben ser independientes entre s?:

# Prueba de independencia:

print(durbinWatsonTest(glm(EN ~ Thigh.Girth + Hip.Girth + Height + Wrists.diameter+ Calf.Maximum.Girth,
                          family = binomial(link = "logit"), data = entrenamiento), max.lag = 1))

# Por lo tanto, con un valor p = 0 se puede decir que no se cumple la condici?n de independencia de los residuos


# 3) Informaci?n incompleta:

# El modelo cuenta con la informaci?n necesaria 

# 4) Separaci?n perfecta:

# De igual forma que en el caso anterior, al tratarse de una variable categ?rica deber?a haber separaci?n perfecta entre
# los predictores, y por lo tanto el algoritmo no converge

# 5) Multicolinealidad 

# Se revisa la presencia de multicolinealidad

modelo3g <- glm(EN ~ Thigh.Girth + Hip.Girth + Height + Wrists.diameter+ Calf.Maximum.Girth,
                family = binomial(link = "logit"), data = entrenamiento)


vifs <- vif(modelo3g)
print(vifs)
print(mean(vifs))

# Por lo tanto si existe multicolinealidad, por lo que se eliminar? el predictor Height del modelo
modelo4 <- update(modelo3g, . ~ . - Height)

print(durbinWatsonTest(glm(EN ~ Thigh.Girth + Hip.Girth + Wrists.diameter+ Calf.Maximum.Girth,
                           family = binomial(link = "logit"), data = entrenamiento), max.lag = 1))

vifs <- vif(modelo4)
print(vifs)
print(mean(vifs))


# Luego, el valor promedio de VIF es superior a 1, y el valor p de independencia de residuos ahora es mayor al valor alfa
# por lo que se debe eliminar otro predictor. En este caso Calf.Maximum.Girth 

modelo5 <- update(modelo4, . ~ . - Calf.Maximum.Girth)


vifs <- vif(modelo5)
print(vifs)
print(mean(vifs))

# Se ve que el valor promedio de VIF es superior a 1, por lo que se elimina otro predictor (Thigh.Girth con un VIF = 1.32)

modelo6 <- update(modelo5, . ~ . - Thigh.Girth)

vifs <- vif(modelo6)
print(vifs)
print(mean(vifs))

# Finalmente el modelo tiene un VIF lo suficientemente cercano a 1 (1.000), por lo que el modelo estar?a "arreglado"


# 6. Evaluar el poder predictivo de los modelos en datos no utilizados para construirlo (o utilizando
# validaci?n cruzada) y revisar las respectivas curvas ROC


# Se evaluar? el poder predictivo usando matriz y gr?fico de confunsi?n. 

#Modelo ejercicio 3

# Datos entrenamiento (MODELO SIMPLE)

# Se calcula la matriz de confusi?n
matriz_e1 <- confusionMatrix(modelo2$pred$pred, modelo2$pred$obs)
print(matriz_e1)

# Gr?fico de confusi?n

pro1 <- predict(glm(EN ~ Thigh.Girth, family = binomial(link = "logit"),
                data = entrenamiento), entrenamiento, type = "response")

ROC_e1 <- roc(entrenamiento[["EN"]], pro1)
plot(ROC_e1)


# Datos prueba (MODELO SIMPLE)

# Matriz de confunsi?n


modelo2b <- train(EN ~ Thigh.Girth,
                  family = binomial(link = "logit"),
                  data = prueba,
                  method = "glm",
                  trControl = trainControl(method = "CV", number = 5, savePredictions = TRUE))


matriz_e2 <- confusionMatrix(modelo2b$pred$pred, modelo2b$pred$obs)
print(matriz_e2)

# Gr?fico de confusi?n


pro2 <- predict(glm(EN ~ Thigh.Girth, family = binomial(link = "logit"),
                    data = prueba), prueba, type = "response")

ROC_e2 <- roc(prueba[["EN"]], pro2)
plot(ROC_e2)

# Luego, a partir del gr?fico ROC se puede ver que con los datos de prueba las rectas tienen un comportamiento alejado de la
# diagonal en comparaci?n con los de entrenamiento. Adem?s de acuerdo a las matrices, datos como Exactitud, sensivilidad y 
# especificidad son mayores para los datos de prueba en contraposici?n a los de entrenamiento, por lo tanto se puede concluir 
# que el modelo Simple (1 variable predictora) tiene mayor poder predictivo.


#Modelo ejercicio 4

# Datos entrenamiento (MODELO MULTIPLE)

# Se calcula la matriz de confusi?n
matriz_e3 <- confusionMatrix(modelo3$pred$pred, modelo3$pred$obs)
print(matriz_e3)

# Gr?fico de confusi?n

pro3 <- predict(glm(EN ~ Thigh.Girth + Hip.Girth + Height + Wrists.diameter+ Calf.Maximum.Girth,
                    family = binomial(link = "logit"),
                    data = entrenamiento), entrenamiento, type = "response")

ROC_e3 <- roc(entrenamiento[["EN"]], pro3)
plot(ROC_e3)


# Datos prueba (MODELO MULTIPLE)

# Matriz de confunsi?n


modelo3b <- train(EN ~ Thigh.Girth + Hip.Girth + Height + Wrists.diameter+ Calf.Maximum.Girth,
                  family = binomial(link = "logit"),
                  data = prueba,
                  method = "glm",
                  trControl = trainControl(method = "CV", number = 5, savePredictions = TRUE))


matriz_e4 <- confusionMatrix(modelo3b$pred$pred, modelo3b$pred$obs)
print(matriz_e4)

# Gr?fico de confusi?n


pro4 <- predict(glm(EN ~ Thigh.Girth+ Hip.Girth + Height + Wrists.diameter+ Calf.Maximum.Girth, 
                    family = binomial(link = "logit"),
                    data = prueba), prueba, type = "response")

ROC_e4 <- roc(prueba[["EN"]], pro4)
plot(ROC_e4)

# Luego, a partir del gr?fico ROC se puede ver que en ambas instancias (datos prueba y entrenamiento) las rectas est?n mucho
# m?s alejadas de la diagonal central que en el modelo simple. Por otro lado de acuerdo a las matrices nuevamente el modelo
# con datos de entrenamieto tiene valores de exactitud y sensivilidad m?s altos, por lo tanto se puede concluir que
# el modelo m?ltiple (+ de 1 variable predictora) tiene un poder predictivo menor.

# Como conclusi?n, a partir de los gr?ficos, se puede decir que el modelo m?ltiple es mucho mejor que el simple.




