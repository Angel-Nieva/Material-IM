# LIBRERIAS
library(dplyr)
library(car)
library(mlogit)
library(pROC)

#----------------- Funciones -----------------#

#Función que según el IMC indica si esta sobrepeso o no. 
determinarEN <- function(i, dato){
  if(dato[i] >= 25){
    return (1)
  }
  else{
    return (0)
  }
}


#----------------- Desarrollo -----------------#


# Consideramos un alfa = 0.05

#Se lee la información desde el archivo csv entregado.
basename <- "EP13 Datos.csv"
file <- file.path("C:/Users/fabia/Desktop/01-2022/IME/IME_equipo1/EP14", basename)
datos <- read.csv2(file = file)

#Se define los datos de altura
altura <- datos$Height / 100

#Se define los datos IMC
IMC <- datos$Weight / altura**2

#Definimos la variable dicotomica EN (Estado Nutricional)
# 1: sobrepeso
# 0: no sobrepeso
EN <- sapply(1:length(IMC), determinarEN, IMC)

#La añadimos a los datos
datos <- cbind(datos, EN, IMC)

#Se define la semilla
set.seed(8091)

#Se obtiene la muestra de las personas con sobrepeso
hombres.sobrepeso <- sample_n(datos%>%filter(Gender == 1 & EN == 1), 60)
#Se obtiene la muestra de las persona sin sobrepeso (no sobrepeso)
hombres.nosobrepeso <- sample_n(datos%>%filter(Gender == 1 & EN == 0), 60)

#Se define las muestras de entrenamiento y prueba
# Junto a ello se realiza un sample para reordenar las variables
muestra.entrenamiento <- rbind(hombres.nosobrepeso[1:40, ],hombres.sobrepeso[1:40, ])
muestra.entrenamiento <- sample_n(muestra.entrenamiento, nrow(muestra.entrenamiento))
muestra.prueba <- rbind(hombres.sobrepeso[41:60, ], hombres.nosobrepeso[41:60, ])
muestra.prueba <- sample_n(muestra.prueba, nrow(muestra.prueba))


#Recordando las 8 variables del trabajo anterior


# "Knee.Girth"  "Biiliac.diameter"  "Thigh.Girth"  "Height"  "Calf.Maximum.Girth"  "Navel.Girth"  "Knees.diameter" 
# "Forearm.Girth"  "Waist.Girth"


variables <- c("Knee.Girth", "Biiliac.diameter", "Thigh.Girth", "Calf.Maximum.Girth",
              "Navel.Girth", "Knees.diameter", "Forearm.Girth")


#Se selecciona la variable Waist.Girth
#Debido a que el estado nutricional hace referencia a si una persona esta en sobrepeso o no,
# y se espera que una persona que tenga sobrepeso también sea más ancha a la altura de la cintura
# comparada a una persona sin sobrepeso.


#Ajuste del modelo de regresión logística
modelo <- glm(EN ~ Waist.Girth, family = binomial(link = "logit"), data = muestra.entrenamiento)
print(summary(modelo))

#Se agrega la variable seleccionada
variables <- c(variables, "Waist.Girth")
fstr <- paste("EN", paste(variables, collapse = " + "), sep = " ~ ")
#Se transforma el string en una fórmula, para ser usado en el modelo ajustado.
formula_m <- formula(fstr)

#Se crea el modelo completo
modelo.completo <- glm(formula_m, family = binomial(link = "logit"), data = muestra.entrenamiento)

#En base a las variables se crea el nuevo modelo 
#Utilizando la función step para la seleccion de los mejores predictores
nuevo.modelo <- step(modelo, scope = list(lower = modelo, upper = modelo.completo), direction = "forward", trace = 0)
print(summary(nuevo.modelo))

#Evaluación modelo entrenamiento
prob_e <- predict(modelo, muestra.entrenamiento, type = "response")

umbral <- 0.5
preds_e <- sapply(prob_e, function(p) ifelse(p >= umbral, "1", "0"))
preds_e <- factor(preds_e, levels = levels(datos[["EN"]]))

ROC_e <- roc(muestra.entrenamiento[["EN"]], prob_e)
plot(ROC_e)

#Evaluación modelo prueba
prob_p <- predict(modelo, muestra.prueba, type = "response")

preds_p <- sapply(prob_e, function(p) ifelse(p >= umbral, "1", "0"))
preds_p <- factor(preds_e, levels = levels(datos[["EN"]]))

ROC_p <- roc(muestra.prueba[["EN"]], prob_p)
plot(ROC_p)


# ------ ANÁLISIS DEL MODELO ------


# La curva de ROC tanto para el conjunto de entrenamiento
# como el conjunto de prueba, son alejados de la diagonal
# pero presentan acercamientos en ciertos sectores.


# ------ EVALUACIÓN DEL NUEVO MODELO ------


#Obtener los residuos y las estadísticas
# Obtener los residuos y las estadísticas .
output <- data.frame (predicted.probabilities = fitted(nuevo.modelo))
output [["standardized.residuals"]] <- rstandard(nuevo.modelo)
output [["studentized.residuals"]] <- rstudent( nuevo.modelo )
output [["cooks.distance"]] <- cooks.distance(nuevo.modelo)
output [["dfbeta"]] <- dfbeta(nuevo.modelo )
output [["dffit"]] <- dffits(nuevo.modelo)
output [["leverage"]] <- hatvalues(nuevo.modelo)

# Evaluar residuos estandarizados que escapen a la normalidad.
# 95 % de los residuos estandarizados deberían estar entre
# -1.96 y 1.96 , y 99 % entre -2.58 y 2.58.
sospechosos1 <- which (abs(output[["standardized.residuals"]]) > 1.96)
sospechosos1 <- sort(sospechosos1 )
cat ("\n\n")
cat (" Residuos estandarizados fuera del 95 % esperado \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - -\n")
print(rownames(muestra.entrenamiento[sospechosos1, ]) )

# Revisar casos con distancia de Cook mayor a uno.
sospechosos2 <- which(output[["cooks.distance"]] > 1)
sospechosos2 <- sort(sospechosos2)
cat ("\n\n")
cat ("Residuales con una distancia de Cook alta \n")
cat ("- - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - -\n")
print(rownames(muestra.entrenamiento[sospechosos2, ]))

# Revisar casos cuyo apalancamiento sea más del doble
# o triple del apalancamiento promedio .
leverage.promedio <- ncol(muestra.entrenamiento)/nrow(datos)
sospechosos3 <- which(output [["leverage "]] > leverage.promedio)
sospechosos3 <- sort(sospechosos3)

cat ("\n\n")

cat (" Residuales con levarage fuera de rango ( > ")
cat (round(leverage.promedio, 3) , ")", "\n", sep = "")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print(rownames(muestra.entrenamiento[sospechosos3, ]) )

# Revisar casos con DFBeta >= 1.
sospechosos4 <- which(apply(output[["dfbeta"]] >= 1 ,1 ,any))
sospechosos4 <- sort(sospechosos4)
names(sospechosos4 ) <- NULL
cat ("\n\n")
cat (" Residuales con DFBeta sobre 1\n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - -\n")
print(rownames(muestra.entrenamiento[sospechosos4 , ]))

# Detalle de las observaciones posiblemente atípicas .
sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4)
sospechosos <- sort (unique(sospechosos))
cat ("\n\n")
cat (" Casos sospechosos \n")
cat (" - - - - - - - - - - -- - - - - -\n")
print(muestra.entrenamiento[sospechosos, ])
cat("\n\n")
print(output[sospechosos , ])


# ------ VERIFICACIÓN DE LAS CONDICIONES ------


# Verificación de multicolinealidad .
cat ("Verificación de colinealidad \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
cat ("\n VIF :\n")
vifs <- vif (nuevo.modelo)
print ( vifs )
cat ("\n Promedio VIF: ")
print ( mean ( vifs ) )
# En base a los valores VIF obtenidos
# todos son menores a 10 por lo que lo que descartan los valores sesgados

# Independencia de los residuos.
cat (" Verificación de independencia de los residuos \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print(durbinWatsonTest(nuevo.modelo) )
# Al obtener el valor p = 0.608, lo cual es mayor a nuestro alfa
# por lo que con un 95% de confianza los residuos son independietnes

# ----------- EVALUAR PODER PREDICTIVO DEL NUEVO MODELO -------
prob_n <- predict(nuevo.modelo, muestra.entrenamiento, type = "response")

preds_n <- sapply(prob_n, function(p) ifelse(p >= umbral, "1", "0"))
preds_n <- factor(preds_n, levels = levels(datos[["EN"]]))

ROC_n <- roc(muestra.entrenamiento[["EN"]], prob_n)
plot(ROC_n)

# Al observar los gráficos de la curva de ROC para los grupos 
# de entrenamiento y de prueba, éstas se van alejando de la 
# diagonal, lo que indica una mejora en el modelo

#----------------------------- COMPARANDO LOS MODELOS -------------------------------------- 

# Hipótesis a contrastar                  

# H0: Ambos modelos son iguales
# HA: Un modelo es mejor que otra

# Comparar los modelos RLS y el RLM
cat ("\n\n")
cat (" Likelihood Ratio Test para los modelos \n")
cat (" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print (anova(modelo, nuevo.modelo, test = "LRT") )

# Observando los resultados obtenidos al comparar los dos modelos,
# se obtiene un p = 0.00012, valor muy significativo en comparación
# al alfa definido (0.05), con ello podemos concluir con 95% de confianza
# que el modelo con 4 predictores (el modelo nuevo) es mejor que modelo nulo.s