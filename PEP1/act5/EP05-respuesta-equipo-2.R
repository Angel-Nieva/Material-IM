library(ggpubr)

# ******************************************** ENUNCIADO ********************************#
# Se sabe que una máquina que envasa detergentes industriales llena bidones con un 
# volumen de producto que sigue una distribución normal con desviación estándar de 1 litro. 
# Usando una muestra aleatoria de 100 botellas, el ingeniero a cargo de la planta requiere 
# determinar si la máquina está llenando los bidones con una media de 10 litros.

# Como se necesita determinar si la máquina llena los bidones con una media de 10 litros,
# nuestro valor nulo es 10.


# ******************************************** PREGUNTA 1 ********************************#
# Si el ingeniero está seguro de que el verdadero volumen medio no puede ser inferior     #
# a 10 litros y piensa rechazar la hipótesis nula cuando la muestra presente una          #
# media mayor a 10,3 litros, ¿cuál es la probabilidad de que cometa un error de tipo I?   #                                               #
# ****************************************************************************************#

# Como el ingeniero está seguro de que el verdadero volumen medio no puede ser inferior
# a 10 litros, se realiza una hipótesis unilateral. 

# Se formula la hipótesis:
# H0: media = 10 ; (El volumen medio de los bidones es exactamente 10 litros)
# HA: media > 10; (El volumen medio de los bidones es mayor a 10 litros)

# Objetivo:
# Se busca determinar el valor alfa, ya que éste corresponde a la probabilidad 
# de cometer un error de tipo I.

# Fijar valores conocidos .
sigma <- 1
n <- 100
media_nula <- 10

# Calcular el error estándar .
SE <- sigma / sqrt (n)

# Graficar la distribución muestral del volumen medio de los bidones si
# la hipótesis nula fuera verdadera.

x <- seq (9 , 11 , 0.01)
y <- dnorm (x, mean = media_nula , sd = SE)
g <- ggplot ( data = data.frame (x, y), aes(x))

g <- g + stat_function (
  fun = dnorm ,
  args = list ( mean = media_nula , sd = SE),
  colour = "red ", size = 1)
  
g <- g + ylab ("")
g <- g + scale_y_continuous ( breaks = NULL )
g <- g + scale_x_continuous ( name = " Volumen medio de los bidones [L]",
                                   breaks = seq (9, 11, 0.1))

g <- g + ggtitle("Distribución del volumen medio de los bidones")
g <- g + geom_vline ( xintercept = media_nula , linetype = "dashed")
g <- g + theme_pubr ()



# Se colorea el área del grafico de la región de rechazo de la hipótesis nula.
q_critico_superior <- 10.3

g <- g + geom_area ( data = subset (data.frame(x,y) , x > q_critico_superior ),
                          aes (y = y),
                          colour = "red ",
                          fill = "red",
                          alpha = 0.5)
print (g)

# Usando la función de pnorm, se entrega la probabilidad de que la variable 
# tome valores menores o iguales que un valor dado.
# Si lower.tail = FALSE, pnorm opera con la cola superior, es decir,
# entrega la probabilidad de que la variable tome valores mayores
# a un valor dado.

alfa <- pnorm (q_critico_superior,mean = media_nula,sd = SE,lower.tail = FALSE )
# Si es binominal se suma:
# +  pnorm (q_critico_inferior,mean = media_nula ,sd = SE,lower.tail = TRUE )

cat (" Alfa = ", alfa , "\n")
# alfa = 0.001349898

# Para comprobar si el alfa obtenida anteriormente es correcto, se realiza el proceso
# inverso para obtener el q crítico (10.3): 

# Si es bilateral, seria alfa/2
Z_critico <- qnorm ( alfa , mean = 0 , sd = 1, lower.tail = FALSE)
# Z_critico = 3
q_critico_superior <- media_nula + Z_critico*SE
cat("q crítico superior = ", q_critico_superior, "\n")
# q crítico superior =  10.3

# Recordando la regla empírica de la distribución normal, aproximadamente 
# el 97,3% de las observaciones se encuentran a una distancia de 
# 3 errores estándar de la media. Como 1-a es aprox. 98%, se puede suponer
# razonable el resultado.

# Con el valor alfa calculado, la probabilidad de que el ingeniero cometa un error tipo 1
# es de 0,13% cada ves que el estimador puntual esté a 3 o más errores estándar
# del parámetro de la población.

# ******************************************** PREGUNTA 2 ********************************#
# Si el verdadero volumen medio de los bidones fuera de 10,2 litros,                      #
# ¿cuál sería la probabilidad de que el ingeniero, que obviamente no conoce este dato,    #
# cometa un error de tipo II?                                                             #
# ****************************************************************************************#

# Objetivo:
# Se busca determinar el valor beta, ya que éste corresponde a la probabilidad 
# de cometer un error de tipo II.

# Se utiliza el alfa obtenido de la pregunta 1.
# alfa = 0.001349898

# Graficar la distribución muestral del volumen medio de los bidones si
# la hipótesis nula fuera verdadera.

x <- seq (9 , 11 , 0.01)
y <- dnorm (x, mean = media_nula , sd = SE)
g <- ggplot ( data = data.frame (x, y), aes(x))

g <- g + stat_function (
  fun = dnorm ,
  args = list ( mean = media_nula , sd = SE),
  colour = "red ", size = 1)

g <- g + ylab ("")
g <- g + scale_y_continuous ( breaks = NULL )
g <- g + scale_x_continuous ( name = " Volumen medio de los bidones [L]",
                              breaks = seq (9, 11, 0.1))

g <- g + ggtitle("Distribución del volumen medio de los bidones")
g <- g + geom_vline ( xintercept = media_nula , linetype = "dashed")
g <- g + theme_pubr ()



# Se colorea el área del grafico de la región de rechazo de la hipótesis nula.
q_critico_superior <- 10.3

g <- g + geom_area ( data = subset (data.frame(x,y) , x > q_critico_superior ),
                     aes (y = y),
                     colour = "red ",
                     fill = "red",
                     alpha = 0.5)

media_efecto = 10.2

# Superponer la distribución muestral de la media de los bidones
# si la media fuera de 10,2 
g <- g + stat_function (fun = dnorm ,
                        args = list ( mean = media_efecto , sd = SE ) ,
                        colour = "blue", size = 1)

# Colorear la región de la nueva curva situada en la región de
# rechazo de la curva original.
x1 <- seq ( 9 , 11 , 0.01)
y1 <- dnorm (x , mean = media_efecto , sd = SE )
g <- g + geom_area ( data = subset ( data.frame (x1,y1 ) ,x > q_critico_superior ),
                     aes ( x = x1 , y = y1 ),
                     colour = "blue", fill = "blue", alpha = 0.5)
g <- g + geom_vline ( xintercept = media_efecto , linetype = "dashed")
print (g)

# Calcular el poder de acuerdo al análisis teórico.
poder <- pnorm (q_critico_superior,mean = media_efecto ,sd = SE,lower.tail = FALSE )

cat("Poder = ", poder, "\n")

# Calcular la probabilidad de cometer un error tipo II.

beta <- 1 - poder
cat("Beta = ", beta, "\n")

# El área roja del grafico corresponde a la región de rechazo si la hipótesis nula fuera verdadera,
# mientras que la región azul corresponde al poder de la prueba t.
# Puesto que el poder corresponde a la probabilidad de no cometer un error de tipo II, se tiene que beta
# es igual a 0.8413.
# En conclusión, el ingeniero no sería capaz de detectar una diferencia
# en el tamaño del efecto del volumen medio un 84,13% de las veces.

# La probabilidad de cometer un error de tipo II es muy grande, sin embargo, una manera de solucionar
# este problema es aumentar el tamaño de la muestra para que el error disminuya
# progresivamente.

# ******************************************** PREGUNTA 3 ********************************#
# Como no se conoce el verdadero volumen medio, genere un gráfico del poder estadístico   # 
# con las condiciones anteriores, pero suponiendo que el verdadero volumen medio          #
# podría variar de 10 a 10,5 litros.                                                      #
# ****************************************************************************************#

# Se pide construir un gráfico de poder estadístico v/s el tamaño del efecto, ya que 
# se hace el supuesto de que las medias están en un cierto rango, y conocemos la media nula, 
# la cual corresponde 10 [L]. En el gráfico, el eje y corresponde al poder de la prueba t
# y el eje x corresponde al tamaño del efecto.

# Se utiliza el alfa obtenido en la pregunta 1
# alfa = 0.001349898

# Generar un vector con un rango de valores para las medias
medias <- seq(10, 10.5, 0.01)

# Se obtiene el tamaño del efecto, el cual corresponde a la diferencia
# entre la media muestral y el valor nulo, no estandarizada.
efecto <- (medias - media_nula)

# Se calcula el poder con el cual se detecta el tamaño de efecto para cada
# tamaño de la muestra, asumiendo una prueba unilateral para
# una sola muestra.
poder <- power.t.test(n = n, delta = efecto, sd = sigma, sig.level = alfa, 
                      type="one.sample", alternative = "one.sided")$power
# Crear un data frame.
datos <- data.frame(efecto, poder)

# Graficar la curva de poder.

g <- ggplot(datos, aes(efecto, poder))
g <- g + geom_line(colour="light blue")
g <- g + ylab("Poder estadístico")
g <- g + xlab("Tamaño del efecto")
g <- g + theme_pubr()
g <- g + ggtitle("Relación entre el poder y el tamaño del efecto")
g <- g + geom_vline ( xintercept = 0 , linetype = "dashed")
print(g)

# Al observando el grafico, se puede notar que a medida que aumenta el tamaño 
# del efecto, el poder se va acercando a 1. En conclusión, a medida 
# que aumenta el tamaño del efecto, aumentará la probabilidad de 
# correctamente rechazar H0 cuando es falsa.

# ******************************************** PREGUNTA 4 ********************************#
# Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían revisarse para    #
# conseguir un poder estadístico de 0,75 y un nivel de significación de 0,05?             #                                         #
# ****************************************************************************************#

# Se necesita determinar el tamaño de la muestra con un volumen medio de 10 litros,
# poder de 0.75 y un alfa de 0.05. Para calcular el tamaño de la muestra, 
# se puede utilizar la prueba de poder, dejando como NULL el parámetro “n”.

library(pwr)

diferencia = 10.3 - media_nula
# diferencia = 0.3

# Cálculo del tamaño de la muestra usando la función power.t.test.
poder_muestra <- power.t.test(n = NULL,
                              delta = diferencia,
                              sd = sigma,
                              sig.level = 0.05,
                              power = 0.75,
                              type = "one.sample",
                              alternative = "one.sided")
print(poder_muestra)
n <- ceiling(poder_muestra [["n"]])
cat("n = ", n, "\n")

# Se deben revisar un total de 62 bidones para conseguir un poder estadístico de 0.75 y 
# nivel de significación 0.05. Esto significa que se necesitan 62 bidones para cometer
# un error de tipo I el 5% de las veces y un error de tipo II el 25% de las veces.   

# ******************************************** PREGUNTA 5 ********************************#
# ¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer un  #
# error de tipo I a un 1% solamente?                                                      #  
# ****************************************************************************************#

# Se necesita determinar el tamaño de la muestra con un volumen medio de 10 litros,
# poder de 0.75 y un alfa de 0.01. Para calcular el tamaño de la muestra, 
# se puede utilizar la prueba de poder, dejando como NULL el parámetro “n”.

# Cálculo del tamaño de la muestra usando la función power.t.test.
poder_muestra <- power.t.test(n = NULL,
                              delta = diferencia,
                              sd = sigma,
                              sig.level = 0.01,
                              power = 0.75,
                              type = "one.sample",
                              alternative = "one.sided")
print(poder_muestra)
n <- ceiling(poder_muestra [["n"]])
cat("n = ", n, "\n")

# Se deben revisar un total de 103 bidones para conseguir un poder estadístico de 0.75 y 
# nivel de significación 0.01. Esto significa que se necesitan 103 bidones para cometer
# un error de tipo I el 1% de las veces y un error de tipo II el 25% de las veces.

# Como se puede observar, a menor valor de alfa (disminuir error tipo I), se necesita un 
# tamaño de muestra mayor para mantener el poder de 0.75.

# Se crea un grafico para representar la relación entre el poder y el tamaño de efecto 
# de la muestra con un nivel de significación de 0.05 y 0.01, dejando constante 
# un poder de 0.75.

library(tidyverse)

n1 = 62
n2 = 103
poder = 0.75
efecto <- diferencia
# efecto = 0.3
# sigma = 1

# Generar un vector con un rango de valores para el tamaño de las muestras
n <- seq(5, 200, 0.1)

alfa_05 <- power.t.test(n = n, delta=efecto, sd = sigma, sig.level = 0.05, 
                        type="one.sample", alternative = "one.sided")$power
alfa_01 <- power.t.test(n = n, delta=efecto, sd = sigma, sig.level = 0.01, 
                        type="one.sample", alternative = "one.sided")$power

datos <- data.frame(n, alfa_05, alfa_01)

# Llevar a formato largo 
datos <- datos %>% pivot_longer(!n, names_to="nombre_alfa", values_to="valor_alfa")

niveles <- c("alfa_05", "alfa_01")
etiquetas <- c("alfa = 0.05", "alfa = 0.01")

datos[["nombre_alfa"]] <- factor(datos[["nombre_alfa"]], levels = niveles, labels = etiquetas)

g <- ggplot(datos, aes(n, valor_alfa, colour = factor(nombre_alfa)))
g <- g + geom_line()
g <- g + labs(colour = " ")
g <- g + ylab("Poder estadístico")
g <- g + xlab("Tamaño de la muestra")

g <- g + scale_color_manual(values=c("red", "blue"))
g <- g + scale_y_continuous(breaks = seq(0,1,0.25))
g <- g + scale_x_continuous(breaks = seq(0,200,20))
g <- g + geom_vline ( xintercept = n1 , linetype = "dashed")
g <- g + geom_vline ( xintercept = n2 , linetype = "dashed")
g <- g + geom_hline ( aes ( yintercept = poder ) ,
                      color = "red", linetype = 2)
g <- g + theme_pubr()
g <- g + ggtitle("Relación entre el poder y el tamaño de la muestra")

print(g)

# Observando el grafico de poder V/S tamaño de la muestra, se puede concluir 
# gráficamente lo dicho anteriormente. A menor sea tu alfa, se necesita un 
# mayor tamaño de muestra para aumentar el poder estadístico y disminuir el 
# riesgo de cometer un error de tipo II.
