library(ggpubr)
library(pwr)
library(ggplot2)
library(tidyverse)
library(ez)
#Nota: se recomienda ejecutar el código por bloques (preguntas) para evitar sobreescritura de variables

###################################################################################################
########################################### Pregunta 1 ############################################
###################################################################################################
#El siguiente código R carga los datos que aparecen en una tabla que compara las mejores soluciones
#encontradas por cuatro algoritmos para instancias del problema del vendedor viajero con solución óptima
#conocida, tomados desde una memoria de título del DIINF. Con estos datos responda la pregunta de
#investigación: ¿hay algoritmos mejores que otros?

texto <- ("
Instancia Optimo R R2 R3 G
'brock400_2' 29 39.4 38.7 37.8 36
'brock400_4' 33 49.2 46.7 45.6 44
'C2000.9' 80 102.8 100.4 97.6 94
'c-fat500-10' 126 127 127 127 126
'hamming10-2' 512 680.8 604.9 601.6 512
'johnson32-2-4' 16 16 16 16 16
'keller6' 59 83.5 75 72.5 69.8
'MANN_a81' 1100 1117.8 1117.8 1117.8 1104.1
'p-hat1500-1' 12 17.1 15.9 15.1 14
'p-hat1500-3' 94 112.2 110.3 103.4 102
'san1000' 15 22.4 22.4 22.3 20
'san400_0.7_1' 40 60.4 59.5 59.5 59
'san400_0.9_1' 100 155.9 145.6 143.6 108
'frb100-40' 100 133.6 123.3 119.5 118
'frb59-26-1' 59 78.8 72.1 69.7 70
'1et.2048' 316 399.6 363.6 351.1 339.6
'1zc.4096' 379 484.2 464.8 450.6 429.5
'2dc.2048' 24 32.4 29.3 28.1 27
")
datos <- read.table(textConnection(texto), header = TRUE)

#Para ello, previamente se establecen las hipótesis a estudiar:
#H0: el tiempo de ejecución promedio respecto al tiempo óptimo es igual para todos los algoritmos.
#HA: el tiempo de ejecución promedio respecto al tiempo óptimo es diferente para al menos uno de los algoritmos.

#Para realizar una prueba Anova de una vía para muestras correlacionadas, se deben cumplir las siguientes
# condiciones:
#
# 1) La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos
# iguales.
#     Para esto, se debe realizar una 'normalización' respecto del valor óptimo, para esto se realiza
#     un ajuste respecto del error (es decir, la diferencia entre el valor Xi del algoritmo y el valor óptimo,
#     luego se divide esta diferencia por el valor óptimo)
#     Con esto, se obtiene una escala de valores mucho mas representativa de cada grupo de datos respecto
#     del valor a comparar (óptimo)

datos[["R"]] <- (datos[["R"]] - datos[["Optimo"]]) / datos[["Optimo"]]
datos[["R2"]] <- (datos[["R2"]] - datos[["Optimo"]]) / datos[["Optimo"]]
datos[["R3"]] <- (datos[["R3"]] - datos[["Optimo"]]) / datos[["Optimo"]]
datos[["G"]] <- (datos[["G"]] - datos[["Optimo"]]) / datos[["Optimo"]]
datos[["Optimo"]] <- datos[["Optimo"]]/datos[["Optimo"]]

#
# 2) Las mediciones son independientes al interior de cada grupo.
#     Si, puesto que (se asume que fueron seleccionadas de manera independiente) el trabajo de 'memoria'
#     llevado a cabo se encargó de realizar observaciones independientes para cada grupo (algoritmo)
#
# 3) Se puede suponer razonablemente que la(s) población(es) de origen sigue(n) una distribución normal.
#     Para ello, se realiza la gráfica de los grupos de estudio, en este caso, según tipo de algoritmo
#     y se analiza si existen valores atípicos.

#Se pivotean las variables
datos <- datos %>% pivot_longer(c("R", "R2", "R3", "G"),
                                names_to = "algoritmo",  
                                values_to = "tiempo"
                                )
datos[["algoritmo"]] <- factor(datos[["algoritmo"]])
datos[["Instancia"]] <- factor(datos[["Instancia"]])

#   Comprobación de normalidad .
g <-   ggqqplot(datos, x = "tiempo", y = "algoritmo", color = "algoritmo") 
g <-   g + facet_wrap (~ algoritmo )
g <-   g + rremove("x.ticks") + rremove("x.text") 
g <-   g + rremove("y.ticks") + rremove("y.text") 
g <-   g + rremove("axis.title")
print(g)

#   En base a lo anterior, dado que existen valores atípicos en todos los grupos
#   es que se debe proceder con cautela, por lo que se define un nivel de significancia de 0.01

alfa <- 0.01

# 4) La matriz de varianzas-covarianzas es esférica.
#     Para la comprobación de esta condición, la función ezAnova() entrega el resultado de la 
#     Prueba de esfericidad de Mauchly.

#Se establecen las hipótesis para el test de esfericidad de Mauchly.
#H0: las varianzas-covarianzas de las muestras son iguales para los grupos.
#HA: al menos una de las muestras tiene varianza-covarianza diferente a alguna de los demás grupos. 

cat("\n\nProcedimiento ANOVA usando ezANOVA\n\n")
pruebaEzAnova <- ezANOVA(data = datos,
                         dv = tiempo,
                         within = algoritmo,
                         wid = Instancia,
                         type = 2,
                         return_aov = TRUE)
print(pruebaEzAnova)

#Gráfico del tamaño del efecto.
g2 <- ezPlot(data = datos, 
             dv = tiempo,
             wid = Instancia, 
             within = algoritmo,
             x_lab = "Algoritmo",
             y_lab = "Diferencia promedio de tiempo de ejecución respecto al óptimo", 
             x = algoritmo)
print(g2)

#En base a la prueba realizada, obteniendo el p-value de la prueba de esfericidad y el p-value
# asociado a la corrección, se rechaza la hipótesis nula en favor de la anternativa de la prueba
# de esfericidad de Mauchly, por lo que se puede asegurar con un 99% de confianza que existe 
# al menos una de las muestras que tiene varianza diferente a alguna de los demás grupos, por
# lo que la prueba de esfericidad de Mauchly falla, lo que motiva a realizar un factor de 
# corrección al p-value de la prueba Anova
# Este nuevo p-value corregido también es menor que el nivel de significación establecido (alfa)
# por lo que se rechaza la hipotesis nula en favor de la hipótesis alternativa del estudio Anova,
# por lo que se puede asegurar con un 99% de confianza que el tiempo de ejecución promedio es 
#diferente para al menos uno de los algoritmos.

#Con todo lo anterior, se procede a realizar el análisis Post-Hoc con el fin de poder determinar
# que grupos presentan diferencias significativas en las muestras correlacionadas.

#En este caso se realiza el procedimiento de Bonferroni.
#Procedimiento post-hoc de Bonferroni.
bonferroni <- pairwise.t.test(datos[["tiempo"]],
                              datos[["algoritmo"]],
                              p.adj = "bonferroni", 
                              paired = TRUE)

cat("Corrección de Bonferroni\n") 
print(bonferroni)


#En base a estos resultados, considerando a su vez la gráfica de tamaño del efecto, se desprende que
# si existen algoritmos mejores que otros. Específicamente, entre G y R3 no existen diferencias
# significativas, donde ambos tienen un promedio de diferencias respecto al óptimo bastante bajas,
# no así R y R2 que presentan diferencias respecto al óptimo bastante mas altas. Así, al momento
# de escoger entre estos cuatro algoritmos se recomienda elegir G o R3, ya que estos presentan mejores
# resultados de tiempo que R y R2.




###################################################################################################
########################################### Pregunta 2 ############################################
###################################################################################################

#El siguiente es (un resumen de) la descripción de un famoso experimento:
#  Naming the ink color of color words can be difficult. For example, if asked to name the color of
#  the word "blue" is difficult because the answer (red) conflicts with the word "blue." This
#  interference is called "Stroop Interference" after the researcher who first discovered the
#  phenomenon. This case study is a classroom demonstration. Students in an introductory
#  statistics class were each given three tasks. In the "words" task, students read the names of 60
#  color words written in black ink; in the "color" task, students named the colors of 60 rectangles;
#  in the "interference" task, students named the ink color of 60 conflicting color words. The times
#  to read the stimuli were recorded.
#
#El siguiente código R carga los datos que se obtuvieron en este estudio. Con estos datos, responda la
#siguiente pregunta de investigación: ¿hay diferencias en los tiempos entre tareas?

texto2 <- ("
words colors interfer
9 25 38
16 15 29
23 17 37
18 19 44
16 16 36
21 17 35
16 15 32
21 19 32
13 22 47
26 24 33
21 19 44
15 26 42
19 15 31
17 23 34
9 14 42
15 19 38
")
datos2 <- read.table(textConnection(texto2), header = TRUE)

instancia <- factor(1:nrow(datos2))
datos2 <- datos2 %>% add_column(instancia, .before = "words")
#Se pivotean las variables
datos2 <- datos2 %>% pivot_longer(c("words", "colors","interfer"),
                                names_to = "tarea",  
                                values_to = "tiempo"
)
datos2[["tarea"]] <- factor(datos2[["tarea"]])

#Para ello, previamente se establecen las hipótesis a estudiar:
#H0: el tiempo promedio es igual para todas las tareas
#HA: el tiempo promedio es diferente para al menos una de las tareas.

#Para realizar una prueba Anova de una vía para muestras correlacionadas, se deben cumplir las siguientes
# condiciones:
#
# 1) La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos
# iguales.
#     Si, puesto que el tiempo, como toda magnitud física, tiene una escala de intérvalos iguales.
#
# 2) Las mediciones son independientes al interior de cada grupo.
#     Si; puesto que al tratarse de un experimento famoso, se asume que las instancias de tiempo fueron
#     registradas de manera independiente.
#
# 3) Se puede suponer razonablemente que la(s) población(es) de origen sigue(n) una distribución normal.
#     Para ello, se realiza la gráfica de los grupos de estudio, en este caso, según tipo de tarea
#     y se analiza si existen valores atípicos.

#   Comprobación de normalidad .
g3 <-   ggqqplot(datos2, x = "tiempo", y = "tarea", color = "tarea") 
g3 <-   g3 + facet_wrap (~ tarea )
g3 <-   g3 + rremove("x.ticks") + rremove("x.text") 
g3 <-   g3 + rremove("y.ticks") + rremove("y.text") 
g3 <-   g3 + rremove("axis.title")
print(g3)

# En base a lo anterior, dado que no existen valores atípicos en los grupos, se puede asegurar
# que los grupos tienen una distribución que aproxima a la normal. Por lo que se establece un 
# nivel de significancia de 0.05

alfa <- 0.05

# 4) La matriz de varianzas-covarianzas es esférica.
#     Para la comprobación de esta condición, la función ezAnova() entrega el resultado de la 
#     Prueba de esfericidad de Mauchly.

#Se establecen las hipótesis para el test de esfericidad de Mauchly.
#H0: las varianzas-covarianzas de las muestras son iguales para los grupos.
#HA: al menos una de las muestras tiene varianza-covarianza diferente a alguna de los demás grupos. 

cat("\n\nProcedimiento ANOVA usando ezANOVA\n\n")
pruebaEzAnova2 <- ezANOVA(data = datos2,
                         dv = tiempo,
                         within = tarea,
                         wid = instancia,
                         type = 2,
                         return_aov = TRUE)
print(pruebaEzAnova2)

#Gráfico del tamaño del efecto.
g4 <- ezPlot(data = datos2, 
             dv = tiempo,
             wid = instancia, 
             within = tarea,
             x_lab = "Tarea",
             y_lab = "Tiempo promedio de las tareas [s]", 
             x = tarea)
print(g4)

#Con este resultado, el p-value obtenido para la prueba de esfericidad de Mauchly resultado mayor que el  
# nivel de significación establecido, con esto se falla al rechazar la hipótesis nula, por lo tanto se  
# puede asegurar con un 95% de confianza que las varianzas-covarianzas de las muestras son iguales para 
# los grupos.

#Por otro lado, el p-value de la prueba Anova es mucho menor que el nivel de significancia (p ~ 3.82x10^-13)
# por lo que se rechaza la hipótesis nula en favor de la hipótesis alternativa, con lo que se puede asegurar
# con un 95% de confianza que el tiempo promedio es diferente para al menos una de las tareas.

#Por lo anterior, se procede a realizar un análisis post-hoc para estudiar cual o cuales de los grupos 
#presentan diferencias significacitvas respecto de los demás.

#En este caso se realiza el procedimiento de Bonferroni.
#Procedimiento post-hoc de Bonferroni.
bonferroni2 <- pairwise.t.test(datos2[["tiempo"]],
                              datos2[["tarea"]],
                              p.adj = "bonferroni", 
                              paired = TRUE)

cat("Corrección de Bonferroni\n") 
print(bonferroni2)

#Del resultado obtenido por bonferroni y el análisis de la gráfica de tamaño de efecto
# se desprende que existen diferencias significativas entre  "interfer-colors" 
# e "interfer-words", de aquí se puede concluir que el tiempo promedio que presenta
# una diferencia significativa corresponde a la tarea "interfer", esto quiere decir que
# al grupo estudiado le cuesta más (toma mayor tiempo realizar) la tarea "interfer" que
# las tareas "words" y "colors".







