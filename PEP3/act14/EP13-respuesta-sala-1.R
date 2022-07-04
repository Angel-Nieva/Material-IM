library(ggpubr)
library(ggplot2)
library(dplyr)
library(leaps)
library(car)
library(lmtest)
library(caret)
library(pROC)
library(caret)

#Se especifica y almacena la ruta del directorio de la base de datos.
basename <- "EP13 Datos.csv"
file <- file.path("C:/Users/Dell PC/Desktop/IME-2022/Actividades/act14",basename)
datos <- read.csv2(file = file)

# Se realiza la lectura de los datos, se escpecifica el formato de codificaci?n UTF-8
#datos <- read.csv(arch, fileEncoding = "UTF-8")

######################
##### PREGUNTA 1 #####
######################

# Se establece la semilla de acuerdo al integrante de menor edad del equipo
# (?ltimos 4 d?gitos de su run)
semilla <- 2166
set.seed(semilla)

# Se procede a filtrar los datos s?lo por mujeres.
datos_mujeres <- filter(datos, Gender == 0)
#Se elimina la columna g?nero al ya no ser ?til.
datos_mujeres["Gender"] <- NULL

#Se calcula el IMC de las personas en la muestra
IMC <- datos_mujeres[["Weight"]]/((datos_mujeres[["Height"]]/100)**2)

#Se crea la variable EN (estado nutricional) donde sobrepeso ser? 1 y no sobrepeso es 0

estado_nutricional <- function(x){
  if(x >= 25.0){
    return(1)
  }else{
    return(0)
  }
}

EN <- sapply(IMC, estado_nutricional)

#Se pegan ambas columnas de datos a los datos de la muestra
datos_mujeres <- cbind(datos_mujeres, IMC, EN)

#Ahora se filtra de la muestra aquellas mujeres con EN = 0 y EN = 1
datos_filtrados0 <- filter(datos_mujeres, EN == 0)
datos_filtrados1 <- filter(datos_mujeres, EN == 1)

#Se obtiene una muestra aleatoria de 25 mujeres con EN = 0 y 25 mujeres con EN = 1
datos_filtrados0 <- datos_filtrados0[sample(nrow(datos_filtrados0), 25, replace = F), ]
datos_filtrados1 <- datos_filtrados1[sample(nrow(datos_filtrados1), 25, replace = F), ]

#Se juntan ambas muestras para formar una ?nica muestra aleatoria de 50 mujeres.
datos_filtrados_final <- rbind(datos_filtrados0, datos_filtrados1)

# A continuaci?n se selecciona de forma aleatoria 8 variables predictoras de la parte anterior, de acuerdo
# a la muestra seleccionada (muestra_mujeres)
muestra_mujeres <- datos_filtrados_final

#Se guarda la columna Weight, IMC y EN por si se utilizan m?s adelante.
peso <- muestra_mujeres["Weight"]
altura <- muestra_mujeres["Height"]
imc <- muestra_mujeres["IMC"]
enu <- muestra_mujeres["EN"]
  
#Se obtienen las 8 variables, primero eliminando las columnas de las variables que no se utilizaron en el ejercicio anterior.
muestra_mujeres["Weight"] <- NULL
muestra_mujeres["Height"] <- NULL
muestra_mujeres["IMC"] <- NULL
muestra_mujeres["EN"] <- NULL
columnas_muestra <- colnames(muestra_mujeres)

#Ahora, se seleccionan aleatoria mente las 8 variables
predictores_aleatorios <- sample(columnas_muestra,8)

#De las variables restantes se selecciona un predictor que pueda predecir de mejor manera la variable EN
predictores_restantes <- setdiff(columnas_muestra, predictores_aleatorios)
muestra_mujeres <- muestra_mujeres[predictores_restantes]
muestra_mujeres <- cbind(muestra_mujeres, enu)

#Se realiza el ajuste del modelo nulo y completo
nulo <- glm(EN ~ 1, family = binomial(link = "logit"), data = muestra_mujeres)
completo <- glm(EN ~ ., family = binomial(link = "logit"), data = muestra_mujeres)

#Se realiza un ajuste con selecci?n hacia delante
adelante <- step(nulo, scope = list(upper = completo), direction = "forward",
                 trace = 1, steps = 2)

#Con lo anterior, se tiene que una variable predictora adecuada es
# "Thigh.Girth" (Grosor promedio de ambos muslos bajo el pliegue del gluteo). 
#Esto dado que es la variable que presenta menor AIC, lo que significa que es la 
# variable que penaliza en menor medida el modelo (genera un mejor modelo).
#Esta selecci?n se realiz? en el dominio de predictores que no est?n 
# en los 8 seleccionados aleatoriamente ni tampoco los predictores Weight, Height 
# e IMC.

#Se construye el modelo
modelo <- glm(EN ~ Thigh.Girth, family = binomial(link = "logit"), data = muestra_mujeres)
print(summary(modelo))

#Se grafica el modelo.
g <- ggscatter(muestra_mujeres, x = "Thigh.Girth", y = "EN", color = "blue",
               xlab = "Grosor promedio de ambos muslos bajo el pliegue del gluteo", ylab = "Estado nutricional")
print(g)

#Se utilizar?n predictores de los 8 seleccionados aleatoriamente para incorporar al modelo,
# para ello se incorporan "EN" y "Thigh.Girth" nuevamente.
thigh <- muestra_mujeres["Thigh.Girth"]
muestra_mujeres_2 <- datos_filtrados_final[predictores_aleatorios]
muestra_mujeres_2 <- cbind(muestra_mujeres_2, thigh, enu)

#Se crea el nuevo modelo con los dos predictores iniciales (EN y Thigh.Girth)
modelo_2 <- glm(EN ~ Thigh.Girth, family = binomial(link = "logit"), data = muestra_mujeres_2)
print(summary(modelo_2))
completo_2 <- glm(EN ~ ., family = binomial(link = "logit"), data = muestra_mujeres_2)

#Para ello se realiza un ajuste con selecci?n hacia delante
modelo_final <- step(modelo_2, scope = list(upper = completo_2), direction = "forward",
                   trace = 1)

#Con esto, se agregaron tres predictores al modelo: Waist.Girth (Grosor a la altura de 
# la cintura), Ankle.Minimum.Girth (Grosor promedio de la parte m?s delgada de ambos tobillos)
# y Hip.Girth (Grosor a la altura de las caderas), esta cantidad queda dentro del rango 
# solicitado de agregar 2-5 predictores. Estos predictores se agregan dado que minimizan 
# el valor de AIC (es decir, mejoran el modelo).

#Con el modelo creado, se procede a evaluar los modelos (simple y logistico)
#Esto en torno a un nivel de significancia de
alfa <- 0.05

############# Se realiza la comprobaci?n de condiciones para el RLS ###############
#Comprobaci?n de que los datos presentan una relaci?n lineal.
#Este se puede verificar con:
correlaciones <- round(cor(x = muestra_mujeres, method = "pearson"), 3)

#Donde el R obtenido para el par EN ~ Thigh.Girth corresponde a 0.771, lo que indica
# que existe una relaci?n relativamente fuerte, as? se puede comprobar que los datos siguen
# una tendencia lineal.

#Los residuos deben ser independientes entre s?.
# Comprobaci?n de independencia de los residuos.
cat("\nPrueba de Durbin-Watson para autocorrelaciones.")
cat("entre errores:\n")
print(durbinWatsonTest(modelo))

############# Se realiza la comprobaci?n de condiciones para el RL ###############
#Debe existir una relaci?n lineal entre los predictores y la respuesta transformada.
correlaciones <- round(cor(x = muestra_mujeres, method = "pearson"), 3)
#Al analizar las correlaciones con la variable de respuesta (EN)
# se puede ver que presentan valores muy cercanos a 0.5 y 0.7 (relacionados).

#Los residuos deben ser independientes entre s?.
# Comprobaci?n de independencia de los residuos.
cat("\nPrueba de Durbin-Watson para autocorrelaciones.")
cat("entre errores:\n")
print(durbinWatsonTest(modelo_final))
#Se cumple con la independencia de los residuos, puesto que el p-value obtenido
# est? muy por sobre el nivel de significancia, por lo que se puede concluir que 
# en efecto los residuos son independientes.

#Comprobaci?n de multicolinealidad
vifs <- vif(modelo_final)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs: \n")
print(vifs)
cat("- Tolerancias:\n")
print(1 / vifs)
cat("- VIF medio:", mean(vifs), "\n")

#En el caso de la multicolinealidad, los datos recabados sugieren que el modelo
# podr?a estar sesgado, puesto que la mitad de los predictores resultaron en una
# tolerancia menor a 0.4, donde adem?s el VIF promedio supera por mucho el valor 2.5,  
# lo que aumenta la preocupaci?n en este aspecto.
modelo_final_corregido <- update(modelo_final, . ~ .  -Chest.diameter)

#Comprobaci?n de multicolinealidad
vifs <- vif(modelo_final_corregido)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs: \n")
print(vifs)
cat("- Tolerancias:\n")
print(1 / vifs)
cat("- VIF medio:", mean(vifs), "\n")

#En as? como se corrige el modelo, se quita el ?ltimo predictor a?adido (Chest.diameter),
# obteniendose as? un resultado favorable que comprueba la condici?n de multicoli-
# nealidad.

#Se revisan las curvas Roc
comparar_umbral <- function(x){
  umbral <- 0.5
  if(x >= umbral){
    return(1)
  }else{
    return(0)
  }
} 

######################## PARA RLS #######################
#Se crea el conjunto de entrenamiento para la validaci?n.
muestra_mujeres$EN <- factor(muestra_mujeres$EN)
nRLS <- nrow(muestra_mujeres)
n_entrenamientoRLS <- floor(0.7 * nRLS)
muestraRLS <- sample.int(n=nRLS, size=n_entrenamientoRLS, replace = F)
entrenamientoRLS <- muestra_mujeres[muestraRLS, ]
pruebaRLS <- muestra_mujeres[-muestraRLS, ]

modelo_RLS <- glm(EN ~ Thigh.Girth, family = binomial(link = "logit"), 
                  data = entrenamientoRLS)
print(summary(modelo_RLS))

#Se evalua el modelo RLS con el conjunto de entrenamiento.
probs_RLS_entrenamiento <- predict(modelo_RLS, entrenamientoRLS, type = "response")
preds_RLS_entrenamiento <- sapply(probs_RLS_entrenamiento, comparar_umbral)
preds_RLS_entrenamiento <- factor(preds_RLS_entrenamiento, levels = levels(muestra_mujeres[["EN"]]))

#Se evalua el modelo RL con el conjunto de prueba
probs_RLS_prueba <- predict(modelo_RLS, pruebaRLS, type = "response")
preds_RLS_prueba <- sapply(probs_RLS_prueba, comparar_umbral)
preds_RLS_prueba <- factor(preds_RLS_prueba, levels = levels(muestra_mujeres[["EN"]]))

#Se revisa la curva ROC
roc_RLS_entrenamiento <- roc(entrenamientoRLS[["EN"]], probs_RLS_entrenamiento)
plot(roc_RLS_entrenamiento)

roc_RLS_prueba <- roc(pruebaRLS[["EN"]], probs_RLS_prueba)
plot(roc_RLS_prueba)

#Evaluar modelo para prueba
matrizRLS_prueba <- confusionMatrix(preds_RLS_prueba, pruebaRLS[["EN"]])
print(matrizRLS_prueba)

#Evaluar modelo para entrenamiento
matrizRLS_entrenamiento <- confusionMatrix(preds_RLS_entrenamiento, entrenamientoRLS[["EN"]])
print(matrizRLS_entrenamiento)

######################## PARA RL #######################
#Se crea el conjunto de entrenamiento para la validaci?n.
muestra_mujeres_2$EN <- factor(muestra_mujeres_2$EN)
nRL <- nrow(muestra_mujeres_2)
n_entrenamientoRL <- floor(0.7 * nRL)
muestraRL <- sample.int(n=nRL, size=n_entrenamientoRL, replace = F)
entrenamientoRL <- muestra_mujeres_2[muestraRL, ]
pruebaRL <- muestra_mujeres_2[-muestraRL, ]

modelo_RL <- glm(EN ~ Thigh.Girth + Waist.Girth + Ankle.Minimum.Girth, family = binomial(link = "logit"),
                 data = entrenamientoRL)
print(summary(modelo_RL))

#Se evalua el modelo RL con el conjunto de entrenamiento.
probs_RL_entrenamiento <- predict(modelo_RL, entrenamientoRL, type = "response")
preds_RL_entrenamiento <- sapply(probs_RL_entrenamiento, comparar_umbral)
preds_RL_entrenamiento <- factor(preds_RL_entrenamiento, levels = levels(muestra_mujeres_2[["EN"]]))

#Se evalua el modelo RL con el conjunto de prueba
probs_RL_prueba <- predict(modelo_RL, pruebaRL, type = "response")
preds_RL_prueba <- sapply(probs_RL_prueba, comparar_umbral)
preds_RL_prueba <- factor(preds_RL_prueba, levels = levels(muestra_mujeres_2[["EN"]]))

#Se revisa la curva ROC
roc_RL_entrenamiento <- roc(entrenamientoRL[["EN"]], probs_RL_entrenamiento)
plot(roc_RL_entrenamiento)

roc_RL_prueba <- roc(pruebaRL[["EN"]], probs_RL_prueba)
plot(roc_RL_prueba)

#Evaluar modelo para prueba
matrizRL_prueba <- confusionMatrix(preds_RL_prueba, pruebaRL[["EN"]])
print(matrizRL_prueba)

#Evaluar modelo para entrenamiento
matrizRL_entrenamiento <- confusionMatrix(preds_RL_entrenamiento, entrenamientoRL[["EN"]])
print(matrizRL_entrenamiento)

#En base a las curvas obtenidas, se puede decir que ambos modelos tienen buenos 
# resultados, puesto que ambos se alejan bastante de la diagonal, lo que significa que
# existe una gran (buena precisi?n).
#Aun as?, existe diferencia, pues el modelo RL se aleja mas que la RLS de la diagonal,
# por lo que se puede decir que el modelo que contempla mas predictores genera
# un mejor resultado que el que posee solo un predictor (RLS)

#Se realiza comparaci?n entre el modelo RLS y modelo RL.
comparacion <- anova(modelo_RLS, modelo_RL, test = "LRT")
print(comparacion)

#Con estos resultados, al verificar la columna de Resid. Dev, donde el segundo
# modelo que posee tres predictores (RL) reduce la varianza de los residuos de forma
# significativa en comparaci?n al modelo que solo posee un predictor (RLS).
#Esto nos dice que el segundo modelo (RL) est? mejor ajusto que el primer
# modelo (RLS)















