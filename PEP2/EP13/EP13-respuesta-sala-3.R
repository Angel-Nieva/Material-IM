library(dplyr)
library(ggpubr)
library(tidyr)
library(car)
library(pROC)
library(caret)

dir <-"~/../Documents/uni/ime/ep13"
base <- "Body-EP12.csv"
arch <- file.path(dir,base)
datos <- read.csv(arch, fileEncoding = "UTF-8")


set.seed(3892)

tamano <- 25
datos <- datos %>% filter(Gender == 0)

peso <- datos[ ,"Weight"]
estatura <- datos[ ,"Height"] 
estatura <- estatura/100
imc <- peso/estatura**2

#Se representa el sobrepeso como 1, y no sobrepeso como 0
EN <- ifelse(imc >= 25, c("1"), c("0"))
#valores <- sample(nrow(datos), tamano)
#muestra_50 <- datos[valores,]

datos <- cbind(datos, EN)

#Se seleccionan 25 personas con sobrepeso y 25 sin sobrepeso
datos_EN0 <- datos %>% filter(EN == 0)
datos_EN1 <- datos %>% filter(EN == 1)
valores_0 <- sample(nrow(datos_EN0), tamano)
valores_1 <- sample(nrow(datos_EN1), tamano)
muestra_0 <- datos_EN0[valores_0,]
muestra_1 <- datos_EN1[valores_1,]
valores <- append(valores_0,valores_1)

muestra_50 <- rbind(muestra_0,muestra_1)

#Se recuerdan las 8 variables elegidas aleatoriamente en la actividad 12
variables <- c("Ankle.Minimum.Girth","Bicep.Girth","Forearm.Girth","Ankles.diameter","Waist.Girth","Biacromial.diameter","Wrist.Minimum.Girth","Shoulder.Girth")
variables <- append(variables, "EN")
muestra_8var <- muestra_50 %>% select(all_of(variables))
muestra_8var$EN <- factor(muestra_8var$EN)


#Según estudios, se dice que las mujeres acumulan más grasa en el abdomen, glúteos y muslos. 
#Por tanto, creemos que si utilizamos el grosor promedio de ambos muslos bajo el pliegue del 
#glúteo (variable "Thigh.Girth"), es posible que esta sea influyente en el peso de las mujeres, 
#y consigo, el imc que presentan.

#Se agrega Thigh.girth
Thigh.Girth <- muestra_50$Thigh.Girth
muestra_8var <- cbind(muestra_8var,Thigh.Girth)


# 3 -------------Construcción del modelo inicial------------------------------------------

#Separar conjuntos de entrenamiento y prueba
n <- nrow(muestra_8var)
#Se considera un conjunto de entrenamiento con 80% de las instancias
n_entrenamiento <- floor(0.7 * n)
muestra <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- muestra_8var[muestra, ]
prueba <- muestra_8var[-muestra, ]

#Ajustar modelo
modelo <- glm(EN ~ Thigh.Girth, family = binomial(link = "logit"), data = entrenamiento)
print(summary(modelo))

#Evaluar el modelo con el conjunto de entrenamiento
cat("Evaluacion del modelo a partir del conjunto de entrenamiento:\n")
probs_e <- predict(modelo, entrenamiento, type = "response")

umbral <- 0.5
preds_e <- sapply(probs_e, function(p) ifelse(p >= umbral, "1", "0"))
preds_e <- factor(preds_e, levels = levels(muestra_8var[["EN"]]))

matriz_e <- confusionMatrix(preds_e, entrenamiento [["EN"]])
print(matriz_e)

#Se obtiene gráfico con valores de entrenamiento
ROC_e <- roc(entrenamiento[["EN"]], probs_e)
plot(ROC_e, main = "RCA modelo inicial Conjunto de entrenamienito")

# 4 -------------Agregar predictores------------------------------------------


#Ajustar modelo completo.
cat("\n\n")
completo <- glm(EN ~ ., family = binomial(link = "logit"),
                data = entrenamiento)

#Se analiza los VIF de las variables con el fin de eliminar la que tiene un mayor VIF
vifs <- vif(completo)
print(vifs)
muestra_8var["Biacromial.diameter"] <- NULL
entrenamiento <- muestra_8var[muestra, ]
prueba <- muestra_8var[-muestra, ]
completo <- glm(EN ~ ., family = binomial(link = "logit"),
                data = entrenamiento)

#Ajustar modelo con regresión escalonada.
cat("Modelo con regresi?n escalonada\n")
cat("--------------------------------------\n")
mejor <- step(modelo , scope = list(lower = modelo, upper = completo),
              direction = "both", trace = 1, steps =2)


#Tras realizar el modelo con regresión escalonado, se observa que se eligió como predictores las variables
#Thigh.Girth, Waist.Girth, Shoulder.Girth y Wrist.Minimum.Girth 



# 5 -------------Verificación de condiciones------------------------------------------

# Verificación de multicolinealidad .
cat ("Verificación de colinealidad \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
cat ("\n VIF :\n")
vifs <- vif (mejor)
print ( vifs )
cat ("\n Promedio VIF: ")
print ( mean ( vifs ) )

#Se realizaran pruebas de multicolinealidad con 5 predictores, sin embargo, de 
#esta forma, el valor VIF es cercano a 5, lo cual ciertos autores consideran que es un 
#valor demasiado alto. Por tanto, se procedió a realizar la misma prueba eliminando el predictor 
#de menos importancia, sin embargo, el valor VIF estaba en torno a 2.6, y según 
#algunos autores, si el valor es mayor que 2.5 se debe preocupar. Por lo que finalmente
#se descartó el predictor con menos relevancia, quedando solo 3 predictores adicionales, 
#y de esta forma, el valor VIF entregado es de 2.300574, lo cual, según la mayoría de 
#autores, se considera oportuno teniendo en cuanta los valores obtenidos previamente.

cat("\n\n")
#Comprobar  independencia  de los  residuos.
cat("Prueba  de Durbin -Watson  para  autocorrelaciones ")
cat("entre  errores :\n")
pruebaDurbin <- durbinWatsonTest(mejor)
print(pruebaDurbin)
cat("\n\n")

#Los predictores selecionados son lo suficientemente independientes, esto gracias
#a los entregado en la prueba de Durbin -Watson, donde el valor p entregado por esta
#es 0.69, siendo este mayor que alpha (0.05), por lo que se prueba la indepedencia
#para este caso.

# 6 -------------Evaluar poder predictivos------------------------------------------

#Evaluar el modelo con el conjunto de entrenamiento
cat (" Evaluación del modelo a partir del conjunto de entrenamiento :\n")
probs_p <- predict(mejor, entrenamiento , type = "response")

preds_p <- sapply(probs_p , function (p) ifelse ( p >= umbral , "1", "0") )
preds_p <- factor(preds_p , levels = levels ( muestra_8var [["EN"]]) )

ROC_p <- roc(entrenamiento[["EN"]] , probs_p)
plot(ROC_p, main = "RCA modelo con 3 predictores Conjunto de entrenamienito")

matriz_p_entrenamiento <-confusionMatrix(preds_p , entrenamiento[["EN"]])
print(matriz_p_entrenamiento)


#Evaluar el modelo con el conjunto de prueba.
cat (" Evaluación del modelo a partir del conjunto de prueba :\n")
probs_p <- predict(mejor, prueba , type = "response")

preds_p <- sapply(probs_p , function (p) ifelse ( p >= umbral , "1", "0") )
preds_p <- factor(preds_p , levels = levels ( muestra_8var [["EN"]]) )

ROC_p <- roc(prueba[["EN"]] , probs_p)
plot(ROC_p, main = "RCA modelo con 3 predictores Conjunto de prueba")

matriz_p_prueba <-confusionMatrix(preds_p , prueba[["EN"]])
print(matriz_p_prueba)

#Al analizar los graficos RCA, se observa que al predecir en los conjuntos de entrenamiento, el modelo 
#obtenido con regresión lineal está mucho más alejado de la recta, por lo cual predice mucho mejor. 
#Además al analizar la recta sta vez con los conjuntos de prueba, se obtiene un resultado bastante similar 
#al RCA con el conjunto de prueba, por lo cual el modelo funciona con datos con lo cual no fue entrenada.
#Lo anterior también se comprueba al analizar la matriz de confusion y comprobar que registra una mayor 
#precisión, sensitividad y especificidad.

