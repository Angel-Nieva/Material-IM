library(ggpubr)
library(ggplot2)
library(dplyr)
library(leaps)
library(car)
library(lmtest)
library(caret)

#Se especifica y almacena la ruta del directorio de la base de datos.
basename <- "EP13 Datos.csv"
file <- file.path("C:/Users/Dell PC/Desktop/IME-2022/Actividades/act13",basename)
datos <- read.csv2(file = file)

######################
##### PREGUNTA 1 #####
######################

# PRIMER PASO
# Se establece la semilla de acuerdo a integrante de menor edad del equipo
# (?ltimos 4 dig?tos de su rut)
semilla <- 2166
set.seed(semilla)

# SEGUNDO PASO
# Se procede a crear una muestra de 50 mujeres en relaci?n al valor de la semilla
# establedico
datos_filtrados <- filter(datos, Gender == 0)
datos_filtrados <- datos_filtrados[sample(nrow(datos_filtrados), 50, replace = F), ]
datos_filtrados["Gender"] <- NULL
# TERCER PASO
# A continuaci?n se seleciona de forma aleatoria 8 variables predictoras de acuerdo
# a la muestra seleccionada (nuestra_mujeres)
muestra_mujeres <- datos_filtrados

#Se guarda la columna peso para utilizarla mas adelante.
peso <- muestra_mujeres["Weight"]


#Se obtienen las 8 variables
muestra_mujeres["Weight"] <- NULL
columnas_muestra <- colnames(muestra_mujeres)
predictores_aleatorios <- sample(columnas_muestra,8)

# CUARTO PASO
# Se seleciona una variable predictora de las otras retantes
# que podr?a ser ?til para predecir la variable peso ("Weight")

#Se almacenan los posibles predictores que pueden ser seleccionados
predictores_posibles <- setdiff(columnas_muestra, predictores_aleatorios)

#Se acota el data.frame solo con los predictores a seleccionar
muestra_mujeres <- muestra_mujeres[predictores_posibles]

#Se realiza el ajuste del modelo nulo y completo (ser? utilizado para el cuarto paso)
#Para ello, se agrega nuevamente la columna peso
muestra_mujeres <- cbind(muestra_mujeres, peso)
nulo <- lm(Weight ~ 1, data = datos_filtrados)
completo <- lm(Weight ~ ., data = muestra_mujeres)

#Para ello se realiza un ajuste con selecci?n hacia delante
adelante <- step(nulo, scope = list(upper = completo), direction = "forward",
                 trace = 1, steps = 2)

#Con lo anterior, se tiene que una variable predictora adecuada es
# "Waist.Girth" (Grosor a la altura de la cintura). Esto dado que es la variable
# que presenta menor AIC, lo que significa que es la variable que penaliza en
# menor medida el modelo (genera un mejor modelo).
#Esta selecci?n se realiz? en el dominio de predictores que no est?n 
#en los 8 seleccionados aleatoriamente ni tampoco los predictores Gender y Weight.

#Adicionalmente, con la funci?n cor se obtiene el nivel de correlaci?n que existe
# entre las variables del data.frame, as?, se ve que Waist.Girth presenta la
# correlaci?n mas fuerte con Weight, esto motiva a?n mas a seleccionar dicha variable
# como predictor del modelo.
correlaciones <- round(cor(x = muestra_mujeres, method = "pearson"), 3)

# QUINTO PASO
# Posteriormente, se construye un modelo de regresi?n simple con el
# predictor seleccionado en el paso anterior:

#Se construye el modelo
modelo <- lm(Weight ~ Waist.Girth, data = muestra_mujeres)
print(summary(modelo))

#Se grafica el modelo.
g <- ggscatter(muestra_mujeres, x = "Waist.Girth", y = "Weight", color = "blue",
               xlab = "Grosor a la altura de la cintura", ylab = "Peso")
print(g)


# SEXTO PASO
#Usando herramientas para la exploraci?n de modelos del entorno R, buscar entre dos y cinco
# predictores de entre las variables seleccionadas al azar en el punto 3, para agregar al modelo de
# regresi?n l?neal simple obtenido en el paso 5

#Se crea un nuevo modelo con las 8 variables aleatorias seleccionadas
muestra_mujeres_2 <- datos_filtrados
waist <- muestra_mujeres_2["Waist.Girth"]
muestra_mujeres_2["Weight"] <- NULL

#Se acota el data.frame solo con los predictores a seleccionar
muestra_mujeres_2 <- muestra_mujeres_2[predictores_aleatorios]

#Se realiza el ajuste del modelo nulo y completo (ser? utilizado para el cuarto paso)
#Para ello, se agrega nuevamente la columna peso
muestra_mujeres_2 <- cbind(muestra_mujeres_2, peso)
muestra_mujeres_2 <- cbind(muestra_mujeres_2, waist)

modelo_2 <- lm(Weight ~ Waist.Girth, data = muestra_mujeres_2)
print(summary(modelo_2))

#Se grafica el modelo.
g_2 <- ggscatter(muestra_mujeres_2, x = "Waist.Girth", y = "Weight", color = "blue",
               xlab = "Grosor a la altura de la cintura", ylab = "Peso")
print(g_2)

completo_2 <- lm(Weight ~ ., data = muestra_mujeres_2)

#Para ello se realiza un ajuste con selecci?n hacia delante
adelante_2 <- step(modelo_2, scope = list(upper = completo_2), direction = "forward",
                 trace = 1)

#Con esto, se agregan cuatro predictores al modelo: Thigh.Girth (Grosor promedio de ambos 
# muslos bajo el pliegue del gluteo), Wrists.diameter (Suma de los di?metros de las mu?ecas),
# Calf.Maximum.Girth (Grosor promedio de la parte m?s ancha de ambas pantorrillas) y 
# Shoulder.Girth (Grosor de los hombros sobre los m?sculos deltoides), esta cantidad
# queda dentro del rango solicitado de agregar 2-5 predictores. Estos predictores se
# agregan dado que minimizan el valor de AIC (es decir, mejoran el modelo).

modelo_final <- update(modelo_2, . ~ . + Thigh.Girth + Wrists.diameter + 
                         Calf.Maximum.Girth + Shoulder.Girth)

# SEPTIMO PASO
#Evaluar los modelos y "arreglarlos" en caso de que tengan alg?n problema con las condiciones que
# deben cumplir.
#Esto en torno a un nivel de significancia de
alfa <- 0.05

#Se realiza la comprobaci?n de condiciones para el RLS

#Comprobaci?n de que los datos presentan una relaci?n lineal.
#Este se puede verificar con:
correlaciones <- round(cor(x = muestra_mujeres, method = "pearson"), 3)

#Donde el R obtenido para el par Weight ~ Waist.Girth corresponde a 0.890, lo que indica
# que existe una relaci?n relativamente fuerte, as? se puede comprobar que los datos siguen
# una tendencia lineal.

#Comprobaci?n de la distribuci?n de los residuos (aproxima a la normal)
cat("Prueba de normalidad para los residuos\n")
print(shapiro.test(modelo$residuals))

#Se cumple con la normalidad de residuos, puesto que el p-value obtenido est?
# por sobre el nivel de significancia, por lo que se puede concluir que los resi
# -duos tienen un comportamiento aproximado a normal.

#Comprobaci?n de la variabilidad de los puntos entorno a la l?nea de m?nimos cuadrados
# debe ser aproximadamente constante.
b_1 <- modelo$coefficients[2]
b_0 <- modelo$coefficients[1]
residuos <- muestra_mujeres[["Weight"]] - (b_1 * muestra_mujeres[["Waist.Girth"]] + b_0)
muestra_mujeres <- data.frame(muestra_mujeres, residuos)
g_var <- ggscatter(muestra_mujeres, x = "Waist.Girth", y = "residuos", color = "blue", fill = "blue",
               xlab = "Grosor a la altura de la cintura", ylab = "Residuos")
g_var <- g_var + geom_hline(yintercept = 0, colour = "red")
print(g_var)

#Ante esto, al observar la gr?fica se puede apreciar que la variabilidad de de los
# residuos es relativamente constante.

#Comprobaci?n de que las observaciones deben ser independientes entre si.
#En este caso, las observaciones son independientes entre s?, pues han sido
# seleccionadas de manera aleatoria y corresponden a menos del 10% de la poblaci?n.

#Se realiza la comprobaci?n de condiciones para el RLM.
#Las variables predictoras deben ser cuantitativas o dicot?micas.
#   Si, son cuantitativas, ya que cada uno de los predictores son 'medidas' num?ricas.
#
#La variable de respuesta debe ser cuantitativa y continua, sin restricciones para su variabilidad.
#   Si, la variable respuesta(peso) es cuantitativa y adem?s continua.
#
#Los predictores deben tener alg?n grado de variabilidad (no pueden ser constantes).
#   Si, los predictores poseen variabilidad; no son constantes (son medidas que var?an).
#
#Cada predictor se relaciona linealmente con la variable de respuesta.
#   Si, puesto que al analizar las correlaciones con la variable de respuesta (peso)
#   se puede ver que presentan valores muy cercanos a 1 (fuertemente relacionados).
correlaciones_2 <- round(cor(x = muestra_mujeres_2, method = "pearson"), 3)

#Comprobaci?n de independencia de los residuos.
cat("\nPrueba de Durbin-Watson para autocorrelaciones.")
cat("entre errores:\n")
print(durbinWatsonTest(modelo_final))

#Comprobaci?n de normalidad de los residuos.
cat("\nPrueba de normalidad para los residuos:\n")
print(shapiro.test(modelo_final$residuals))

#Comprobaci?n de homocedasticidad de los residuos.
cat("\nPrueba de homocedasticidad para los residuos:\n")
print(ncvTest(modelo_final))

#Comprobaci?n de multicolinealidad
vifs <- vif(modelo_final)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs: \n")
print(vifs)
cat("- Tolerancias:\n")
print(1 / vifs)
cat("- VIF medio:", mean(vifs), "\n")

#Ante esto se puede ver que:
# i)Se cumple con la independencia de los residuos, puesto que el p-value obtenido
#   est? muy por sobre el nivel de significancia, por lo que se puede concluir que 
#   en efecto los residuos son independientes.
#
# ii)Se cumple con la normalidad de residuos, puesto que el p-value obtenido est?
#   por sobre el nivel de significancia, por lo que se puede concluir que los resi
#   -duos tienen un comportamiento aproximado a normal.
#
# iii)Se cumple con la homocedasticidad de los residuos, puesto que el p-value obtenido
#   est? muy por sobre el nivel de significancia, por lo que se puede concluir que
#   los residuos tienen varianzas similares para cada nivel de los predictores.
#
# iv)En el caso de la multicolinealidad, los datos recabados sugieren que el modelo
#   podr?a estar sesgado, puesto que las tolerancias no superan (en la mayor?a de 
#   los casos) el valor 0.4, donde adem?s el VIF promedio supera el valor 2.5, lo 
#   que aumenta la preocupaci?n en este aspecto.
modelo_final_corregido <- update(modelo_final, . ~ .  -Shoulder.Girth - Calf.Maximum.Girth)

#Comprobaci?n de multicolinealidad
vifs_corregido <- vif(modelo_final_corregido)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs: \n")
print(vifs_corregido)
cat("- Tolerancias:\n")
print(1 / vifs_corregido)
cat("- VIF medio:", mean(vifs_corregido), "\n")

#Con lo anterior, eliminando dos predictores se pudo corregir el modelo, obteniendo
# valores que permiten verificar la multicolinealidad.


# OCTAVO PASO
#Evaluar el poder predictivo de los modelos en datos no utilizados para construirlo 
# (o utilizando validaci?n cruzada)

#PARA RLS
#Se crea el conjunto de entrenamiento para la validaci?n.
nRLS <- nrow(muestra_mujeres)
n_entrenamientoRLS <- floor(0.8 * nRLS)
muestraRLS <- sample.int(n=nRLS, size=n_entrenamientoRLS, replace = F)
entrenamientoRLS <- muestra_mujeres[muestraRLS, ]
pruebaRLS <- muestra_mujeres[-muestraRLS, ]

modelo_cruzadaRLS <- train(Weight ~ Waist.Girth, data = entrenamientoRLS, method = "lm",
                        trControl = trainControl(method = "cv", number = 5))
print(summary(modelo_cruzadaRLS))

#Se obtiene el MSE (error cuadr?tico medio) para el conjunto entrenamiento.
mse_entrenamientoRLS <- modelo_cruzadaRLS$results$RMSE

#Se realizan las predicciones
prediccionesRLS <- predict(modelo_cruzadaRLS, pruebaRLS)

#Se calcula el error cuadr?tico medio para el conjunto de prueba.
errorRLS <- pruebaRLS[["Weight"]] - prediccionesRLS
mse_pruebaRLS <- sqrt(mean(errorRLS ** 2))

#PARA RLM
#Se crea el conjunto de entrenamiento para la validaci?n.
nRLM <- nrow(muestra_mujeres_2)
n_entrenamientoRLM <- floor(0.8 * nRLM)
muestraRLM <- sample.int(n=nRLM, size=n_entrenamientoRLM, replace = F)
entrenamientoRLM <- muestra_mujeres_2[muestraRLM, ]
pruebaRLM <- muestra_mujeres_2[-muestraRLM, ]

modelo_cruzadaRLM <- train(Weight ~ Waist.Girth + Thigh.Girth + Wrists.diameter, data = entrenamientoRLM, method = "lm",
                           trControl = trainControl(method = "cv", number = 5))
print(summary(modelo_cruzadaRLM))

#Se obtiene el MSE (error cuadr?tico medio) para el conjunto entrenamiento.
mse_entrenamientoRLM <- modelo_cruzadaRLM$results$RMSE

#Se realizan las predicciones
prediccionesRLM <- predict(modelo_cruzadaRLM, pruebaRLM)

#Se calcula el error cuadr?tico medio para el conjunto de prueba.
errorRLM <- pruebaRLM[["Weight"]] - prediccionesRLM
mse_pruebaRLM <- sqrt(mean(errorRLM ** 2))  

#Errores
cat("\nError cuadr?tico medio RLS para entrenamiento: ", mse_entrenamientoRLS, "\n")
cat("\nError cuadr?tico medio RLS para prueba: ", mse_pruebaRLS, "\n")
cat("\nError cuadr?tico medio RLM para entrenamiento: ", mse_entrenamientoRLM, "\n")
cat("\nError cuadr?tico medio RLM para prueba: ", mse_pruebaRLM, "\n")


#En base a los resultados obtenidos para el modelo de RLS se tiene una diferencia
# bastante aceptable, pues son valores bastante cercanos, por lo que se puede decir
# que el modelo es generalizable, es decir, el modelo genera buenos resultados
# predictivos para el predictor seleccionado, lo cual tiene sentido pues se consider?
# un predictor que 'mejora' el modelo.

#As? mismo, los resultados para el modelo de RLM tambi?n denotan una diferencia
# peque?a (menor que la de RLS incluso), por lo que se puede decir que el modelo
# tambi?n es generalizable, esto pues genera buenos resultados predictivos para 
# los predictores seleccionados, lo que cobra sentido, puesto que se han seleccionado
# aquellos predictores que fueron agregados al modelo RLS, los que disminuyeron 
# su valor AIC, con lo que el modelo 'mejora'.









