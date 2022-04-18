# Se sabe que el proceso de fabricaci?n de barras de acero para concreto reforzado producen barras con
# medidas de dureza que siguen una distribuci?n normal con desviaci?n est?ndar de 10 kilogramos de
# fuerza por mil?metro cuadrado. Usando una muestra aleatoria de tama?o 25, un ingeniero quiere averiguar
# si una l?nea de producci?n est? generando barras con dureza media de 170 [kgf mm^-2].

# Observaci?n: dado que es una muestra aleatoria a la cual se le conoce la desviaci?n est?ndar
# y sigue una distribuci?n normal, es posible aplicar la prueba t de Student para una muestra.

# Formulaci?n de hip?tesis:
# H0: La dureza media de las barras es de  exactamente 170 [kgf mm^-2].
# HA: La dureza media de las barras es distinta de 170 [kgf mm^-2].

# En este caso, el valor nulo es ??0 = 170 [kgf mm^-2]. Matem?ticamente, 
# las hip?tesis anteriores se pueden formular como:
# H0: ?? = ??0, esto es ?? = 170
# HA: ?? ??? ??0, esto es ?? ??? 170


# *********************************** PREGUNTA 1 ***********************************
#Si el ingeniero piensa rechazar la hip?tesis nula cuando la muestra presente una media menor a 
#167 [kgf mm^-2] o mayor a 173 [kgf mm^-2], ?cu?l es la probabilidad de que cometa un error de tipo 1?

# Se pide determinar el valor de alfa, ya que ?ste corresponde a la probabilidad de cometer
# un error de tipo I.

#El ?rea roja del gr?fico corresponde a la regi?n de rechazo si la hip?tesis nula fuera verdadera

library(ggpubr)
library(pwr)

# Fijar valores conocidos
sigma <- 10
n <- 25

# Calcular el error est?ndar
SE <- sigma/sqrt(n)
media_nula <- 170

# Graficar la distribuci?n muestral de la dureza media de las barras si
# la hip?tesis nula fuera verdadera
x <- seq(160, 180, 0.01 )

y <- dnorm(x, mean = media_nula, sd = SE)

g <- ggplot(data = data.frame(x,y), aes(x))

g <- g + stat_function(fun = dnorm, 
                       args = list(mean = media_nula, sd = SE),
                       colour = "red", size = 1)

g <- g + ylab(" ")
g <- g + scale_y_continuous(breaks =  NULL)
g <- g + scale_x_continuous(name = "Dureza media de las barras [kgf mm^-2]", breaks = seq(160,180,2))

g <- g + ggtitle("Distribuci?n de la dureza media de las barras")
g <- g + geom_vline ( xintercept = media_nula , linetype = "dashed")
g <- g + theme_pubr()


# Se colorea el ?rea del gr?fico correspondiente a la regi?n de rechazo de la hip?tesis nula
g <- g + geom_area ( data = subset (data.frame(x,y), x < 167) ,
                     aes ( y = y ) ,
                     colour = "red",
                     fill = "red",
                     alpha = 0.5)

g <- g + geom_area ( data = subset (data.frame(x,y) , x > 173 ) ,
                     aes ( y = y ) ,
                     colour = "red",
                     fill = "red",
                     alpha = 0.5)

print(g)


# pnorm entrega la probabilidad de que la variable tome valores menores
# o iguales que un valor dado.
# Si lower.tail = FALSE, pnorm opera con la cola superior, es decir,
# entrega la probabilidad de que la variable tome valores mayores
# a un valor dado
alfa <- pnorm (167,mean = media_nula,sd = SE,lower.tail = TRUE ) + 
  pnorm (173,mean = media_nula ,sd = SE,lower.tail = FALSE )
cat (" Alfa = ", alfa , "\n")

# Para comprobar si el alfa obtenido anteriormente est? bien calculado, se realiza el proceso 
# inverso para as? obtener los q cr?ticos, es decir, 167 y 173, los cuales resultan ser as?.
media_nula <- 170
sigma <- 10
n <- 25
SE <- sigma/sqrt(n)
alfa <- 0.133614402537716
Z_critico <- qnorm ( alfa/2 , mean = 0 , sd = 1, lower.tail = FALSE)
q_critico_inferior <- media_nula - Z_critico*SE
q_critico_superior <- media_nula + Z_critico*SE
cat("q cr?tico inferior = ", q_critico_inferior, "\n")
cat("q cr?tico superior = ", q_critico_superior, "\n")

# Con esto se obtiene que la probabilidad de que el investigador cometa error tipo 1
# es de 0.13, es decir, un 13% de las veces cometer? error de tipo I cada vez que el estimador
# puntual est? a 1.5 o m?s errores est?ndar del par?metro de la poblaci?n. 
# Cabe mencionar que esto puede ocurrir un 6.5% en cada cola de la distribuci?n, ya que corresponde
# a una prueba bilateral. 


# *********************************** PREGUNTA 2 ***********************************
# Si la verdadera dureza media de la l?nea de producci?n fuera 172 [kgf mm^-2], ?cu?l ser?a la probabilidad de
# que el ingeniero, que obviamente no conoce este dato, cometa un error de tipo 2?

# Se pide determinar el valor de beta, ya que ?ste corresponde a la probabilidad de cometer
# un error de tipo II.

library(ggpubr)
library(pwr)

# Fijar valores conocidos
sigma <- 10
n <- 25
media_nula <- 170
# Calcular el error est?ndar
SE <- sigma/sqrt(n)
#Utilizando el alfa obtenido de la pregunta 1
alfa <- pnorm (167,mean = media_nula,sd = SE,lower.tail = TRUE ) + 
  pnorm (173,mean = media_nula ,sd = SE,lower.tail = FALSE )



# Graficar la distribuci?n muestral de la dureza media si
# la hip?tesis nula fuera verdadera
x <- seq(160, 180, 0.01 )
y <- dnorm(x, mean = media_nula, sd = SE)
g <- ggplot(data = data.frame(x,y), aes(x))

g <- g + stat_function(fun = dnorm, 
                       args = list(mean = media_nula, sd = SE),
                       colour = "red", size = 1)

g <- g + ylab(" ")
g <- g + scale_y_continuous(breaks =  NULL)
g <- g + scale_x_continuous(name = "Dureza media de las barras [kgf mm^-2]", breaks = seq(160,180,2))
g <- g + geom_vline ( xintercept = media_nula , linetype = "dashed")
g <- g + theme_pubr()


Z_critico <- qnorm ( alfa /2 , mean = 0 , sd = 1 , lower.tail = FALSE )
q_critico_inferior <- media_nula - SE*Z_critico
q_critico_superior <- media_nula + SE*Z_critico

g <- g + geom_area ( data = subset (data.frame(x,y), x < q_critico_inferior ) ,
                     aes ( y = y ) ,
                     colour = "red",
                     fill = "red", alpha = 0.5)

g <- g + geom_area ( data = subset (data.frame(x,y) , x > q_critico_superior ) ,
                     aes ( y = y ) ,
                     colour = "red",
                     fill = "red", alpha = 0.5)


media_efecto = 172
# Superponer la distribuci?n muestral de la media de las diferencias
# si la media fuera de 172
g <- g + stat_function (fun = dnorm ,
                        args = list ( mean = media_efecto , sd = SE ) ,
                        colour = "blue", size = 1)

# Se colorea la regi?n de la nueva curva situada en la regi?n de
# rechazo de la curva original.
x1 <- seq ( 160, 180, 0.01)
y1 <- dnorm (x , mean = media_efecto , sd = SE )
g <- g + geom_area ( data = subset (data.frame (x1 ,y1 ), x < q_critico_inferior ) ,aes ( x = x1 , y = y1 ) ,
                     colour = "blue",
                     fill = "blue",
                     alpha = 0.5)


g <- g + geom_area ( data = subset ( data.frame (x1,y1 ) ,x > q_critico_superior ),
                     aes ( x = x1 , y = y1 ),
                     colour = "blue", fill = "blue", alpha = 0.5)
g <- g + geom_vline ( xintercept = media_efecto , linetype = "dashed")
print (g)

poder <- pnorm (q_critico_inferior,mean = media_efecto,sd = SE,lower.tail = TRUE ) + 
  pnorm (q_critico_superior,mean = media_efecto ,sd = SE,lower.tail = FALSE )

cat("Poder = ", poder, "\n")

beta <- 1 - poder

cat("Beta = ", beta, "\n")

# El ?rea roja del gr?fico corresponde a la regi?n de rechazo si la hip?tesis nula fuera verdadera,
# mientras que la regi?n azul corresponde al poder de la prueba t.
# Con lo anterior tenemos que la probabilidad de cometer un error tipo II es de 0.685252795948237 o 68.525% aprox.
# lo que quiere decir que m?s de un 68% de las veces el investigador no detecta una 
# diferencia del tama?o del efecto de las durezas medias (2 [kgf mm-2])
# de las barras.
# Como se puede ver en los resultados, el valor de beta es una probabilidad muy grande, 
# esto es debido a que la muestra es muy peque?a, sin embargo,
# la probabilidad de cometer un error tipo II disminuir? a medida que el tama?o de la muestra crezca.



# *********************************** PREGUNTA 3 ***********************************
# Como no se conoce la verdadera dureza media, genere un gr?fico del poder estad?stico con las
# condiciones anteriores, pero suponiendo que las verdaderas durezas medias podr?an variar de 162 a 178
# [kgf mm^-2].

# Se pide construir un gr?fico de poder estad?stico v/s el tama?o del efecto, ya que 
# se hace el supuesto de que las medias est? en un cierto rango, y conocemos la media nula, 
# la cual corresponde 170 [kgf mm^-2]. En el gr?fico, el eje y corresponde al poder de la prueba t
# y el eje x corresponde al tama?o del efecto.

library(ggpubr)
library(tidyverse)

# Datos entregados por el enunciado
media_nula <- 170
sigma <- 10
n <- 25
SE <- sigma/sqrt(n)

# Se utiliza el alfa obtenido de la pregunta 1
alfa <- pnorm (167,mean = media_nula,sd = SE,lower.tail = TRUE ) + 
  pnorm (173,mean = media_nula ,sd = SE,lower.tail = FALSE )
# Generar un vector con un rango de valores para las medias
medias <- seq(162, 178, 0.01)

# Se obtiene el tama?o del efecto, el cual corresponde a la diferencia
# entre la media muestral y el valor nulo, no estandarizada
efecto <- (medias - media_nula)
poder <- power.t.test(n = n, delta = efecto, sd = sigma, sig.level = alfa, 
                       type="one.sample", alternative = "two.sided")$power
datos <- data.frame(efecto, poder)


g <- ggplot(datos, aes(efecto, poder))
g <- g + geom_line(colour="red")
g <- g + ylab("Poder estad?stico")
g <- g + xlab("Tama?o del efecto")
g <- g + scale_x_continuous(breaks = seq(-10,10,2))
g <- g + theme_pubr()

g <- g + ggtitle("Relaci?n entre el poder y el tama?o del efecto")
g <- g + geom_vline ( xintercept = 0 , linetype = "dashed")
print(g)

# Como se puede notar en el gr?fico, a medida que el valor absoluto del tama?o del 
# efecto aumenta, el poder se va acercando cada vez m?s a 1, es decir,
# a medida que aumenta el tama?o del efecto, aumentar? la probabilidad de 
# correctamente rechazar H0 cuando es falsa. 

# *********************************** PREGUNTA 4 ***********************************
# ?Cu?ntas barras deber?an revisarse para conseguir un poder estad?stico de 0,80 y un nivel de significaci?n
# de 0,05?

# Se pide determinar el tama?o de la muestra con las condiciones indicadas, para lo 
# cual se utiliza la prueba de poder, dejando como NULL el par?metro "n".

# Obtener el tama?o de la muestra mediante la funcion de power.t.test
poder_muestra <- power.t.test(n = NULL,
                              delta = 172 - 170,
                              sd = 10,
                              sig.level = 0.05,
                              power = 0.80,
                              type = "one.sample",
                              alternative = "two.sided")
print(poder_muestra)
n <- ceiling(poder_muestra [["n"]])
cat("n = ", n, "\n")


# Deber?an revisarse un total de 199 barras para conseguir un poder estadistico de 0.80 y 
# nivel de significaci?n 0.05.
# Esto quiere decir que se necesitan 199 barras para cometer un error de tipo 1 el 5% de las veces, 
# y un error de tipo 2 el 20% de las veces. 


# *********************************** PREGUNTA 5 ***********************************
# ?Y si quisiera ser bien exigente y bajar la probabilidad de cometer un error de tipo 1 a un 1% solamente?

# Se pide determinar el tama?o de la muestra con las condiciones indicadas, para lo 
# cual se utiliza la prueba de poder, dejando como NULL el par?metro "n"..

poder_muestra <- power.t.test(n = NULL,
                              delta = 172 - 170,
                              sd = 10,
                              sig.level = 0.01,
                              power = 0.80,
                              type = "one.sample",
                              alternative = "two.sided")
print(poder_muestra)
n <- ceiling(poder_muestra [["n"]])
cat("n = ", n, "\n")


# Utilizandola funci?n pwr.t.test del paquete pwr, se obtiene el mismo tama?o de muestra, 
# a diferencia que el tama?o de la muestra en este caso corresponde a la d de Cohen
library (pwr)
resultado <- pwr.t.test ( n = NULL ,
                          d = (172 - 170)/10 ,
                          sig.level = 0.01 ,
                          power = 0.80,
                          type = "one.sample",
                          alternative = "two.sided")

print (resultado)
n <- ceiling(resultado[["n"]])
cat("n (usando pwr.t.test) =", n, "\n")

# En conclusi?n, deber?an revisarse un total de 296 barras para conseguir un poder estadistico de 0.80 y 
# nivel de significaci?n 0.01.
# Esto quiere decir que se necesitan 296 barras para cometer un error de tipo 1 el 1% de las veces, 
# y un error de tipo 2 el 20% de las veces. 


# Como podemos notar, al usar un valor de alfa m?s exigente , es decir,disminuir el porcentaje de error de 
# tipo I obtenemos que se necesita un tama?o de muestra mayor para conseguir un poder de 0.80, con lo que podr?amos decir
# que mientras mayor sea el tama?o de muestra, menor ser?n los porcentajes de error de tipo I.

# El siguiente gr?fico presenta la relaci?n entre el poder y el tama?o de la muestra con un nivel de 
# significaci?n de 0.05 y 0.01

library(ggpubr)
library(tidyverse)
tam1 = 199
tam2 = 296
poder = 0.8
efecto = 172 - 170 
sigma = 10
# Generar un vector con un rango de valores para el tama?o de las muestras
n <- seq(5, 500, 0.01)

alfa_05 <- power.t.test(n = n, delta=efecto, sd = sigma, sig.level = 0.05, 
                            type="one.sample", alternative = "two.sided")$power
alfa_01 <- power.t.test(n = n, delta=efecto, sd = sigma, sig.level = 0.01, 
                            type="one.sample", alternative = "two.sided")$power

datos <- data.frame(n, alfa_05, alfa_01)
datos <- datos %>% pivot_longer(!n, names_to="nombre_alfa", values_to="valor_alfa")

niveles <- c("alfa_05", "alfa_01")
etiquetas <- c("alfa = 0.05", "alfa = 0.01")

datos[["nombre_alfa"]] <- factor(datos[["nombre_alfa"]], levels = niveles, labels = etiquetas)

g <- ggplot(datos, aes(n, valor_alfa, colour = factor(nombre_alfa)))
g <- g + geom_line()
g <- g + labs(colour = " ")
g <- g + ylab("Poder estad?stico")
g <- g + xlab("Tama?o de la muestra")

g <- g + scale_color_manual(values=c("red", "blue"))
g <- g + scale_y_continuous(breaks = seq(0,1,0.1))
g <- g + scale_x_continuous(breaks = seq(0,500,50))
g <- g + geom_vline ( xintercept = tam1 , linetype = "dashed")
g <- g + geom_vline ( xintercept = tam2 , linetype = "dashed")
g <- g + geom_hline ( aes ( yintercept = poder ) ,
                        color = "red", linetype = 2)
g <- g + theme_pubr()

g <- g + ggtitle("Relaci?n entre el poder y el tama?o de la muestra")

print(g)

# Al complementar con el gr?fico de poder v/s el tama?o de la muestra, se comprueba lo dicho anteriormente.
# Si bien para ambos valores de alfa, el poder estad?stico crece asint?ticamente a 1, 
# para el caso de un alfa igual a 5%, desde un tama?o de muestra igual a 199, el poder comienza 
# a ser de un 0.8, mientras que para el caso de un alfa igual a 1%, el poder comienza a ser 0.8
# desde un tama?o de muestra igual a 296. 
# Para el caso del alfa igual a 0.01 podemos ver que su curva es m?s baja que la curva correspondiente
# al alfa de 0.05.




