library(tidyverse)
library(dplyr)

# ******************************************** PREGUNTA 2 ********************************#
# ¿Existen diferencias entre las puntuaciones obtenidas para los diferentes envases de 
# chocolate? De ser así, ¿cuál(es) envase(s) se diferencia(n) de los demás?
# ****************************************************************************************#

# Dado que se tienen 3 muestras correlacionadas y se pide determinar si 
# existe un algoritmo mejor o peor que los otros, se piensa utilizar 
# la prueba de Friedman.

#Condiciones para utilizar la prueba de Friedman:
#1. La variable independiente debe ser categórica y tener a lo menos tres niveles.
#2. La escala de la variable dependiente debe ser, a lo menos, ordinal.
#3. Los sujetos son una muestra aleatoria e independiente de la población.

#La primera condición se cumple puesto que la variable independiente es el diseño 
#del envase, la cual efectivamente es una variable categórica y posee 4 niveles.
#La segunda condición se cumple puesto que la escala de la variable dependiente
#(puntaje) es una escala Likert, la cual es ordinal.
#La tercera condición se cumple puesto que en el enunciado se especifica que los
#participantes son escogidos aleatoriamente y no dependen uno del otro.

#Definimos nuestras hipótesis

#H0: no existen diferencias entre las puntuaciones para los diferentes envases
#de chocolate
#H1: al menos un envase de chocolate presenta una diferencia entre las puntuaciones

#Deinimos el valor de alfa
alfa <- 0.05

#Seleccionamos los datos que utilizaremos y procedemos a realizar la prueba de
#Friedman

datos <- datos %>% filter(Producto == "Chocolate")
datos <- datos %>% select(Id, Diseno, Puntaje)

prueba <- friedman.test(Puntaje~Diseno | Id, data = datos)
print(prueba)
#p.valor = 0.4008

#En conclusión considerando que el p.valor > alfa=0.05, se puede afirmar con un 95%
#de confianza que no se rechaza la hipótesis nula, por lo tanto no existe diferencia
#entre las puntuaciones para los diferentes envases de chocolate.

if (prueba$p.value < alfa){
  post_hoc <- pairwise.wilcox.test(datos$Puntaje,
                                   datos$Diseno,
                                   p.adjust.method = "holm",
                                   paired = TRUE)
  print(post_hoc)
}

#En este caso, no se realiza la prueba post-hoc ya que de la prueba de friedman
#se puede concluir que no existen diferencias entre las puntuaciones. De haber
#obtenido un p.valor < alfa, se realiza la prueba de holm para saber en cuál(es)
#de los envases existen diferencias.