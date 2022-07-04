library(dplyr) # Filter
library(ggpubr)
library(car) #durbinWatsonTest
library(pROC) # ROC

# Lectura de los datos

basename <- "EP13 Datos.csv"
file <- file.path("C:/Users/Dell PC/Desktop/IME-2022/Actividades/act14",basename)
datos <- read.csv2(file = file)

# Para esta actividad usaremos la misma muestra de medidas anatómicas seleccionada para el ejercicio
# práctico anterior desde los datos recolectados por Heinz et al. (2003). La Como este ejercicio requiere de
# una variable dicotómica, vamos a realizar lo siguiente:

# 1. El equipo crea la variable IMC (índice de masa corporal) como el peso de una persona (en kilogramos) 
#    dividida por el cuadrado de su estatura (en metros).

# La nueva variable se agrega usando mutate()
# se divide la altura por 100 para pasar de centímetros a metros.

datos <- datos %>% mutate(IMC = Weight/((Height/100)^2))

# Si bien esta variable se usa para clasificar a las personas en varias clases de estado nutricional 
# (bajo peso, normal, sobrepeso, obesidad, obesidad mórbida), para efectos de este ejercicio, 
# usaremos dos clases: sobrepeso (IMC ≥ 25,0) y no sobrepeso (IMC ≤ 25,0).

# 3. El equipo crea la variable dicotómica EN (estado nutricional) de acuerdo al valor de IMC de cada persona.

# Crear la variable dicotómica EN (estado nutricional) de acuerdo al valor de IMC de cada persona,
# siendo 1 Sobrepeso y 0 No sobrepeso.

datos <- datos %>% mutate(EN = ifelse(IMC >= 25.0, 1, 0))

# Ahora podemos construir un modelo de regresión logística para predecir la variable EN, de acuerdo con 
# las siguientes instrucciones:

# 1. Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos del RUN (sin considerar el dígito verificador) 
#    del integrante de mayor edad del equipo.

semilla <- 4097
set.seed(semilla)


# 2. Seleccionar una muestra de 120 mujeres (si la semilla es un número par) o 120 hombres (si la semilla es impar), asegurando que 
# la mitad tenga estado nutricional “sobrepeso” y la otra mitad “no sobrepeso”. Dividir esta muestra en dos conjuntos: los datos de 
# 80 personas (40 con EN “sobrepeso”) para utilizar en la construcción de los modelos y 40 personas (20 con EN “sobrepeso”) para 
# poder evaluarlos.

datos_hombres <- datos %>% filter(Gender == "1")

# Se escogen 60 muestras con valor EN == "Sobrepeso"
variableSobrepeso <- datos_hombres%>%filter(EN == 1)
variableSobrepeso2 <- variableSobrepeso[sample(nrow(variableSobrepeso), 60),]

# Se escogen 60 muestras con valor EN == "No sobrepeso"
variableNoSobrepeso <- datos_hombres%>%filter(EN == 0)
variableNoSobrepeso2 <- variableNoSobrepeso[sample(nrow(variableNoSobrepeso), 60),]

# Se escogen 40 muestras con sobrepeso y 40 muestras sin sobrepeso para crear el conjunto de entrenamiento

conSobrepeso_1 <- variableSobrepeso2[1:40,] 
sinSobrepeso_1 <- variableNoSobrepeso2[1:40,]

# Se escogen 20 muestras con sobrepeso y 20 muestras sin sobrepeso para crear el conjunto de prueba

conSobrepeso_2 <- variableSobrepeso2[41:60,] 
sinSobrepeso_2 <- variableNoSobrepeso2[41:60,]

# Se deja tabla final para conjunto de entrenamiento y prueba

muestra_entrenamiento <- merge(x = conSobrepeso_1, y = sinSobrepeso_1, all = TRUE)
muestra_prueba <- merge(x = conSobrepeso_2, y = sinSobrepeso_2, all = TRUE)

# 3. Recordar las ocho posibles variables predictoras seleccionadas de forma aleatoria en 
#    el ejercicio anterior.

# Se guarda la columna EN ya que será la variable respuesta y se eliminan las columnas
# Weight y Height ya que están relacionadas con la variable EN. Además se elimina Gender
# ya que no se utilizará.

en <- muestra_entrenamiento["EN"]
muestra_entrenamiento["IMC"] <- NULL
muestra_entrenamiento["Weight"] <- NULL
muestra_entrenamiento["Height"] <- NULL
muestra_entrenamiento["EN"] <- NULL
muestra_entrenamiento["Gender"] <- NULL
columnas_muestra <- colnames(muestra_entrenamiento)

# Se repite el proceso para los datos de prueba
en_2 <- muestra_prueba["EN"]
muestra_prueba["IMC"] <- NULL
muestra_prueba["Weight"] <- NULL
muestra_prueba["Height"] <- NULL
muestra_prueba["EN"] <- NULL
muestra_prueba["Gender"] <- NULL


# predictores aleatorios: 
# 1) Ankle.Minimum.Girth
# 2) Wrists.diameter
# 3) Waist.Girth
# 4) Chest.Girth
# 5) Biiliac.diameter
# 6) Navel.Girth
# 7) Age
# 8) Elbows.diameter

predictores_aleatorios <- c("Ankle.Minimum.Girth", "Wrists.diameter", "Waist.Girth", "Chest.Girth",   
                            "Biiliac.diameter", "Navel.Girth", "Age", "Elbows.diameter")

# 4. Seleccionar, de las otras variables, una que el equipo considere que podría ser útil 
#    para predecir la clase EN, justificando bien esta selección.

# Se almacenan los posibles predictores que pueden ser seleccionados
variables_restantes <- setdiff(columnas_muestra, predictores_aleatorios)

# Se reduce el data.frame con las columnas a utilizar.
muestra_entrenamiento <- muestra_entrenamiento[variables_restantes]
muestra_prueba <- muestra_prueba[variables_restantes]

# Se agrega la columna EN a la muestra obtenida.
muestra_entrenamiento <- cbind(muestra_entrenamiento, en)
muestra_prueba <- cbind(muestra_prueba, en_2)

# Ajustar un modelo nulo.
nulo <- glm(EN ~ 1, family = binomial(link = "logit"), data = muestra_entrenamiento)

# Ajustar modelo completo.
suppressWarnings(completo <- glm(EN ~ ., family = binomial(link = "logit"), data = muestra_entrenamiento))

# Evaluar la variable para incorporar
print(add1(nulo, scope = completo))

# Se obtiene que el mejor predictor para incorporar al modelo nulo es Hip.Girth, ya que según la función
# add1, es el predictor que retorna el menor AIC al ser incorporado al modelo (64.766).

# Además, con la ayuda de la función cor, podemos ver que la variable predictora con la mayor 
# correlación es Hip.Girth (Grosor a la altura de las caderas), con un valor R = 0.671. 
# Por lo que la variable predictora Hip.Girth puede ser útil para predecir la variable EN 	
correlaciones <- round(cor(x = muestra_entrenamiento, method = "pearson"), 3)

# 5. Usando el entorno R y paquetes estándares, construir un modelo de regresión logística con el predictor 
#    seleccionado en el paso anterior y utilizando de la muestra obtenida.

modelo <- glm(EN ~ Hip.Girth, family = binomial(link = "logit"),data = muestra_entrenamiento)
print(summary(modelo))

# Se grafica el modelo.
g <- ggscatter(muestra_entrenamiento, x = "Hip.Girth", y = "EN", color = "blue",
               xlab = "Grosor a la altura de las caderas", ylab = "Estado Nutricional")
print(g)

# 6. Usando herramientas estándares para la exploración de modelos del entorno R, buscar 
#    entre dos y cinco predictores de entre las variables seleccionadas al azar, recordadas 
#    en el punto 3, para agregar al modelo obtenido en el paso 5.

# Se reduce el data.frame con las columnas a utilizar y se guarda la columna Hip.Girth .
muestra_entrenamiento_2 <- merge(x = conSobrepeso_1, y = sinSobrepeso_1, all = TRUE)
muestra_prueba_2 <- merge(x = conSobrepeso_2, y = sinSobrepeso_2, all = TRUE)

girth <- muestra_entrenamiento_2["Hip.Girth"]
girth_2 <- muestra_prueba_2["Hip.Girth"]
muestra_entrenamiento_2 <- muestra_entrenamiento_2[predictores_aleatorios]
muestra_prueba_2 <- muestra_prueba_2[predictores_aleatorios]

# Se agrega la columna EN y Hip.Girt
muestra_entrenamiento_2 <- cbind(muestra_entrenamiento_2, en)
muestra_entrenamiento_2 <- cbind(muestra_entrenamiento_2, girth)

muestra_prueba_2 <- cbind(muestra_prueba_2, en_2)
muestra_prueba_2 <- cbind(muestra_prueba_2, girth_2)

# Ajustar modelo completo
completo2 <- glm(EN ~ ., family = binomial(link = "logit"), data = muestra_entrenamiento_2)

# Ajustar modelo con eliminación hacia atrás
modelo_final <- step(object = completo2, scope = list(lower = modelo),
                  direction = "backward",
                  trace = 0)
print(summary(modelo_final))
#AIC(backwards) = 53.147

# Utilizando eliminación hacia atrás, se agregan 3 predictores al modelo:
# Waist.Girth, Chest.Girth y Age.

# 7. Evaluar la confiabilidad de los modelos (i.e. que tengan un buen nivel de ajuste y son 
#    generalizables) y “arreglarlos” en caso de que tengan algún problema.

# Se define un nivel de significación:
alfa <- 0.01

# Se comprueban las condiciones para el primer modelo de RLogS:

# Condiciones:
# 1. Los datos deben presentar una relación lineal.
# 2. Los residuos deben ser independientes entre si.
# 3. Información incompleta.
# 4. Sin separación perfecta.

# Verificación de condiciones:
# 1) Se puede saber si existe una relación lineal entre la variable predictora (Hip.Girth)
#    y respuesta (EN) mediante la correlación entre las dos variables.
correlaciones <- round(cor(x = muestra_entrenamiento, method = "pearson"), 3)
cat("\n\n")

# Se obtiene una correlación de 0.671 para EN~Hip.Girth, lo que significa que existe
# una relación lineal directa relativamente fuerte entre la variable respuesta y predictora.

# 2) Comprobando la independencia de los residuos:

print(durbinWatsonTest(modelo))
# p-value = 0.54 > alfa, por lo que podemos concluir que los residuos son, en efecto, independientes.

# 3) Comprobando la cantidad de observaciones:
# Se tiene un total de 80 observaciones, como se necesita de un mínimo de 15 observaciones por
# predictor entonces se cumple la condición.

# 4) Verificando Warning al crear el modelo simple.
#    Al crear el modelo RLogS con la función glm, no se retornó ningún warning advirtiendo
#    sobre que el algoritmo no converge, por lo que se cumple condición.

# En consecuencia, se verifica el cumplimiento de todas las condiciones necesarias para
# emplear un modelo de RLogS. Por lo tanto, el modelo puede ser generalizado.

print(summary(modelo))
#AIC(backwards) = 64.766

# Detectar posibles valores atípicos .
cat (" Identificación de posibles valores atípicos \n")
cat (" --------------------------------------\n")
plot ( modelo )

# El grafico muestra 2 posibles instancias cuyo residuo esta algo más alejado de las 
# demás, correspondiente a las observaciones 26 y 55 (Residuals vs Fitted) ademas de 
# estar algo más alejados de la recta Q-Q. Al observar el grafico Residuals vs Leverage
# se puede observar que las instancias no ejercen un apalancamiento, por lo que el modelo 
# está bien ajustado.

# Se comprueban las condiciones para el segundo modelo de RLogM:

# Condiciones:
# 1. Los datos deben presentar una relación lineal.
# 2. Los residuos deben ser independientes entre si.
# 3. Información incompleta.
# 4. Sin separación perfecta.
# 5. No debe existir multicolinealidad. Esto significa que no deben existir 
#    relaciones lineales fuertes entre dos o más predictores 
#    (coeficientes de correlación altos). 

# Verificación de condiciones:
# 1) Se puede saber si existe una relación lineal entre las variables predictoras (Hip.Girth,
#    Waist.Girth, Chest.Girth y Age) y respuesta (EN) mediante la correlación entre las variables.
correlaciones_2 <- round(cor(x = muestra_entrenamiento_2, method = "pearson"), 3)
cat("\n\n")

# Se puede ver que existe una relación lineal fuerte entre la mayoría de los predictores y 
# la variable respuesta (EN), a excepción del predictor Age que cuenta con una correlación de 
# 0.115, por lo que se elimina del modelo.

modelo_final <- update(modelo_final, . ~ . - Age )
# Modelo final con los predictores:
# - Waist.Girth
# - Chest.Girth
# - Hip.Girth

# 2) Comprobando la independencia de los residuos:

print(durbinWatsonTest(modelo_final))
# p-value = 0.038 > alfa, por lo que podemos concluir que los residuos son, en efecto, independientes.

# 3) Comprobando la cantidad de observaciones:
# Se tiene un total de 80 observaciones, como se necesita de un mínimo de 15 observaciones por
# predictor entonces se cumple la condición.

# 4) Verificando Warning al crear el modelo multiple
#    Al crear el modelo RLogM con la función glm, no se retornó ningún warning advirtiendo
#    sobre que el algoritmo no converge, por lo que se cumple la condición.

# 5) Comprobando la multicoleanidad:

vifs <- vif(modelo_final)
cat("\nVerificar la multicolinealidad:\n")
cat("- VIFs: \n")
print(vifs)
cat("- Tolerancias:\n")
print(1 / vifs)
cat("- VIF medio:", mean(vifs), "\n")

# Si se miran los factores de inflacion de varianza, en general no parecen ser preocupantes (vifs < 4).
# Los estadisticos de la tolerancia  no parecen ser preocupantes, ya que todos superan el valor
# de 0.4. Adicionalmente, el VIF promedio es bastante cercano a 1 (VIF medio: 1.246374), por lo 
# que podemos concluir que la multicolinealidad se cumple. 

# En consecuencia, se verifica el cumplimiento de todas las condiciones necesarias para
# emplear un modelo de RLogM. Por lo tanto, el modelo puede ser generalizado.

print(summary(modelo_final))
#AIC(backwards) = 53.375

# Detectar posibles valores atípicos .
cat (" Identificación de posibles valores atípicos \n")
cat (" --------------------------------------\n")
plot ( modelo_final )

# El grafico muestra 3 posibles instancias cuyo residuo esta algo más alejado de las 
# demás, correspondiente a las observaciones 46, 68 y 55 (Residuals vs Fitted) ademas de 
# estar algo más alejados de la recta Q-Q. Al observar el grafico Residuals vs Leverage
# se puede observar que las instancias no ejercen un apalancamiento, por lo que el modelo 
# está bien ajustado.

# 8. Usando código estándar, evaluar el poder predictivo de los modelos con los datos de las 40 personas 
# que no se incluyeron en su construcción en términos de sensibilidad y especificidad

# Se recrea el modelo RLogS con los datos de prueba
modelo_2 <- glm(EN ~ Hip.Girth, data = muestra_prueba)
print(summary(modelo_2))
# AIC: 50.696

# Se recrea el modelo RLogM con los datos de prueba
modelo_final_2  <- glm(EN ~ Waist.Girth + Chest.Girth + Hip.Girth, data = muestra_prueba_2)
print(summary(modelo_final_2))
# AIC: 42.29

# Se evalua el poder predictivo del modelo RLogS utilizando la curva ROC para los datos de prueba
probs_P_S <- predict(modelo_2, muestra_prueba, type = "response")
ROC_P_S <- roc(muestra_prueba[["EN"]], probs_P_S)
plot(ROC_P_S)

# Se evalua el poder predictivo del modelo RLogS utilizando la curva ROC para los datos de prueba
probs_P_M <- predict(modelo_final_2, muestra_prueba_2, type = "response")
ROC_P_M <- roc(muestra_prueba_2[["EN"]], probs_P_M)
plot(ROC_P_M)

# Comparando las curvas ROC para los modelos de RLogs y RLogM con datos de prueba, se puede observar
# que ambos modelos tienen buenos resultados, ya que ambos se alejan bastante de la diagonal,
# sin embargo la curva se aleja en mayor medida para el modelo RLogM, por lo que se puede decir
# que se trata de un mejor modelo en terminos de sensibilidad y especifidad.

# Se comparan los modelos con la prueba de Likelihood Ratio Test(LRT)

# Comparar los modelos RLogS y RlogM
cat("\n\n")
cat("Likelihood Ratio Test para los modelos\n")
cat("--------------------------------------\n")
comparacion <- anova(modelo_2, modelo_final_2, test = "LRT") 
print(comparacion)
# p = 0.001437
    
# Observando los datos entregados por la prueba Likelihood, con un p-valor = 0.001437 < alfa, 
# se concluye que el modelo con 3 predictores (RLogM) ofrece una mejora significativa en
# el ajuste que el primer modelo (RLogs).

