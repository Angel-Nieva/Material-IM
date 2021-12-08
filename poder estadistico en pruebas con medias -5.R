# ******************************************** ENUNCIADO ******************************************** 
# Se sabe que el proceso de fabricación de barras de acero para concreto reforzado producen barras con
# medidas de dureza que siguen una distribución normal con desviación estándar de 10 kilogramos de
# fuerza por milímetro cuadrado. Usando una muestra aleatoria de tamaño 50, un ingeniero quiere averiguar
# si una línea de producción está generando barras con dureza media de 170 [kgf mm-2].
# ****************************************************************************************************

library(TeachingDemos)
library(ggpubr)

#Hipotesis:
# H0: Barras con dureza media de 170 [kgf mm^-2]
# Ha: Barras con dureza media distinta a 170 [kgf mm^-2]
# H0: µ = 170
# Ha: µ != 170

# *************************************** PREGUNTA 1 *************************************************
# Si el ingeniero está seguro que la verdadera dureza media no puede ser menor a los 170 [kgf mm-2] y
# piensa rechazar la hipótesis nula cuando la muestra presente una media mayor a 173 [kgf mm-2], ¿cuál es
# la probabilidad de que cometa un error de tipo 1?

# Fijar Valores conocidos
sigma <- 10
media_nula <- 170
n <- 50

# Calcular el error estándar
SE <- sigma / sqrt(n)

# Grafico de la distribución muestral de la media si la hipótesis nula fuera verdadera.
x <- seq(160,180,0.01)
y <- dnorm(x, mean=media_nula, sd = SE)
df <- data.frame(x,y)

g <- ggplot(data = df, aes(x))

g <- g + stat_function(
  fun = dnorm,
  args = list(mean = media_nula, sd = SE),
  colour = "red", size = 1)

g <- g + ylab("")
g <- g + scale_y_continuous(breaks = NULL)
g <- g + scale_x_continuous(name = "Durezas medias[kgf mm-2]",
                            breaks = seq(160,180, 2))
g <- g + theme_pubr()

# Como el ingeniero está seguro que la verdadera dureza media no puede ser menor a los 170 se toma
# únicamente la cola superior.

# Colorear la región de rechazo de la hipótesis nula
q_critico_superior  <- 173

g <- g + geom_area(data = subset(df , x > q_critico_superior),
                   aes(y = y),
                   colour = "red",
                   fill = "red"
)
print(g)

# El error de tipo 1 es cuando se rechaza H0 en favor de H1, siendo H0 verdadera. 
# alfa = probabilidad de cometer error tipo 1.

# Como se muestra en el grafico, para que se cometa un error de tipo 1 se debe rechazar H0 si la dureza de las barras
# es mayor a 173.

# Se calcula alfa tomando la región roja del gráfico.
alfa <- pnorm (173,mean = media_nula ,sd = SE,lower.tail = FALSE )

cat("1) La probabilidad de cometer un error de tipo 1 es de:",alfa*100,"%\n")
# alfa = 0.01694

# *************************************** PREGUNTA 2 *************************************************
# Si la verdadera dureza media de la línea de producción fuera 172 [kgf mm-2], ¿cuál sería la probabilidad de
# que el ingeniero, que obviamente no conoce este dato, cometa un error de tipo 2?

library(pwr)

diferencia <- 172 - 170
# alfa = 0.01694

# El error de tipo 2 es cuando no se rechaza H0 en favor de H1, siendo H1 verdadera. 
# beta = probabilidad de cometer error tipo 2.

# Se calcula el poder con la función:
poder <- power.t.test(n=n,
                          delta = diferencia,
                          sd = sigma,
                          sig.level = alfa,
                          power = NULL,
                          type = "one.sample", # Una sola muestra
                          alternative = "two.sided")$power

beta <- 1 - poder 
cat("2) La probabilidad de cometer un error de tipo 2 es de:",beta*100,"%\n")
# beta = 0.8448

# *************************************** PREGUNTA 3 *************************************************
# Como no se conoce la verdadera dureza media, genere un gráfico del poder estadístico con las
# condiciones anteriores, pero suponiendo que las verdaderas durezas medias podrían variar de 170 a 178
# [kgf mm-2].

# Se calcula la curva de poder para un:
# tamaño de efecto = 8
#  n = 50
# alfa = 0.01694

# Vector con un rango de valores para el efecto de medias
efecto <- seq(-8, 8, 0.01)

# Calculo del poder con los datos entregados y alfa de los ejercicios anteriores
poder <- power.t.test(n = n,
                      delta = efecto,
                      sd = sigma,
                      sig.level = alfa,
                      power = NULL,
                      type = "one.sample",
                      alternative = "two.sided")$power

# Matriz de datos
datos <- data.frame(efecto,poder)

# Graficar la curva de poder
g <- ggplot(datos, aes(efecto, poder))
g <- g + geom_line(colour = "blue")
g <- g + ylab("Poder estadistico")
g <- g + xlab("Tamaño efecto")
g <- g + theme_pubr()
g <- g + ggtitle(" Poder vs durezas medias")

print(g)
# *************************************** PREGUNTA 4 *************************************************
# ¿Cuántas barras deberían revisarse para conseguir un poder estadístico de 0,90 y un nivel de significación
# de 0,05?

# diferencia = 2 (172 - 170)
alfa <- 0.05
poder <- 0.90

# Calcular el tamaño de la muestra:
barras <- power.t.test(n = NULL,
                       delta=diferencia,
                       sd = desv_est,
                       sig.level = alfa,
                       power = poder,
                       type = "one.sample",
                       alternative = "two.sided")$n

cat("4) Se necesitan",barras,"barras para conseguir un poder estadístico de 0.90 y 
    un nivel de significación de 0.05 \n")

# *************************************** PREGUNTA 5 *************************************************
# ¿Y si quisiera ser bien exigente y bajar la probabilidad de cometer un error de tipo 1 a un 1% solamente?

alfa <- 0.01
poder <- 0.90

# Calcular el tamaño de la muestra:
barras <- power.t.test(n = NULL,
                       delta=diferencia,
                       sd = desv_est,
                       sig.level = alfa,
                       power = poder,
                       type = "one.sample",
                       alternative = "two.sided")$n

cat("5) Se necesitan",barras,"barras para conseguir un poder estadístico de 0.90 y 
    un nivel de significación de 0.01 \n")

