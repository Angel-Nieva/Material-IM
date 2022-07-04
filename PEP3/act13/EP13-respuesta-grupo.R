library(dplyr) # Filter

basename <- "EP13 Datos.csv"
file <- file.path("C:/Users/Dell PC/Desktop/IME-2022/Actividades/act14",basename)
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
datos_filtrados <- filter(datos, Gender == 1)
datos_filtrados <- datos_filtrados[sample(nrow(datos_filtrados), 50, replace = F), ]
datos_filtrados["Gender"] <- NULL

# TERCER PASO
# Seleccionar de forma aleatoria ocho posibles variables predictoras.
muestra_hombres <- datos_filtrados

# Se guarda la columna peso ya que sera la variable respuesta.
peso <- muestra_hombres["Weight"]

# Se obtienen las 8 variables
muestra_hombres["Weight"] <- NULL
columnas_muestra <- colnames(muestra_hombres)
predictores_aleatorios <- sample(columnas_muestra,8)

# CUARTO PASO
# Seleccionar, de las otras variables, una que el equipo considere que podría ser útil 
# para predecir la variable Peso, justificando bien esta selección.

# Se almacenan los posibles predictores que pueden ser seleccionados
variables_restantes_c <- setdiff(columnas_muestra, predictores_aleatorios)

# Se reduce el data.frame con las columnas a utilizar.
variables_restantes <- muestra_hombres[variables_restantes_c]

# Se agrega la columna peso a la muestra obtenida.
variables_restantes <- cbind(variables_restantes, peso)
muestra_hombres <- cbind(muestra_hombres, peso)

# Se utiliza la función add1, la cual evalúa la incorporación de cada nuevo predictor potencial
# a un modelo base y entrega algunas métricas para el efecto que tiene en su incorporación.

# Ajustar un modelo nulo.
nulo <- lm(Weight ~ 1, data = datos_filtrados)
# Ajustar modelo completo.
completo <- lm(Weight ~ ., data = variables_restantes)

# Evaluar la variable para incorporar
print(add1(nulo, scope = completo))

# Se obtiene que el mejor predictor para incorporar al modelo nulo es Hip.Girth, ya que según la función
# add1, es el predictor que retorna el menor AIC al ser incorporado al modelo.

# Adicionalmente, con la ayuda de la funci?n cor,
correlaciones <- round(cor(x = muestra_hombres, method = "pearson"), 3)

# Generando modelo con la variable seleccionada
modelo <- lm(Weight ~ Hip.Girth, data = variables_restantes)

predictores_aleatorios <- c("Hip.Girth", predictores_aleatorios)

fstr <- paste("Weight", paste(predictores_aleatorios, collapse = " + "), sep = " ~ ")
formula <- formula(fstr)

completo <- lm(formula, data=muestra_hombres)
nulo <- lm(Weight ~ 1, data=muestra_hombres)
backwards <- step(object = completo, scope = list(lower = nulo),
                direction = "backward",
                trace = 0)

#elegidos <- backwards$terms
cinco<- drop1(object = backwards,scope = backwards)



