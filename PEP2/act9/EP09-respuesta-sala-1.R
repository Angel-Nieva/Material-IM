library(ggpubr)
library(pwr)
library(ggplot2)
library(tidyverse)
library(ez)
#Nota: se recomienda ejecutar el c�digo por bloques (preguntas) para evitar sobreescritura de variables

###################################################################################################
########################################### Pregunta 1 ############################################
###################################################################################################
#El siguiente c�digo R carga los datos que aparecen en una tabla que compara las mejores soluciones
#encontradas por cuatro algoritmos para instancias del problema del vendedor viajero con soluci�n �ptima
#conocida, tomados desde una memoria de t�tulo del DIINF. Con estos datos responda la pregunta de
#investigaci�n: �hay algoritmos mejores que otros?

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

#Para ello, previamente se establecen las hip�tesis a estudiar:
#H0: el tiempo de ejecuci�n promedio respecto al tiempo �ptimo es igual para todos los algoritmos.
#HA: el tiempo de ejecuci�n promedio respecto al tiempo �ptimo es diferente para al menos uno de los algoritmos.

#Para realizar una prueba Anova de una v�a para muestras correlacionadas, se deben cumplir las siguientes
# condiciones:
#
# 1) La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos
# iguales.
#     Para esto, se debe realizar una 'normalizaci�n' respecto del valor �ptimo, para esto se realiza
#     un ajuste respecto del error (es decir, la diferencia entre el valor Xi del algoritmo y el valor �ptimo,
#     luego se divide esta diferencia por el valor �ptimo)
#     Con esto, se obtiene una escala de valores mucho mas representativa de cada grupo de datos respecto
#     del valor a comparar (�ptimo)

datos[["R"]] <- (datos[["R"]] - datos[["Optimo"]]) / datos[["Optimo"]]
datos[["R2"]] <- (datos[["R2"]] - datos[["Optimo"]]) / datos[["Optimo"]]
datos[["R3"]] <- (datos[["R3"]] - datos[["Optimo"]]) / datos[["Optimo"]]
datos[["G"]] <- (datos[["G"]] - datos[["Optimo"]]) / datos[["Optimo"]]
datos[["Optimo"]] <- datos[["Optimo"]]/datos[["Optimo"]]

#
# 2) Las mediciones son independientes al interior de cada grupo.
#     Si, puesto que (se asume que fueron seleccionadas de manera independiente) el trabajo de 'memoria'
#     llevado a cabo se encarg� de realizar observaciones independientes para cada grupo (algoritmo)
#
# 3) Se puede suponer razonablemente que la(s) poblaci�n(es) de origen sigue(n) una distribuci�n normal.
#     Para ello, se realiza la gr�fica de los grupos de estudio, en este caso, seg�n tipo de algoritmo
#     y se analiza si existen valores at�picos.

#Se pivotean las variables
datos <- datos %>% pivot_longer(c("R", "R2", "R3", "G"),
                                names_to = "algoritmo",  
                                values_to = "tiempo"
                                )
datos[["algoritmo"]] <- factor(datos[["algoritmo"]])
datos[["Instancia"]] <- factor(datos[["Instancia"]])

#   Comprobaci�n de normalidad .
g <-   ggqqplot(datos, x = "tiempo", y = "algoritmo", color = "algoritmo") 
g <-   g + facet_wrap (~ algoritmo )
g <-   g + rremove("x.ticks") + rremove("x.text") 
g <-   g + rremove("y.ticks") + rremove("y.text") 
g <-   g + rremove("axis.title")
print(g)

#   En base a lo anterior, dado que existen valores at�picos en todos los grupos
#   es que se debe proceder con cautela, por lo que se define un nivel de significancia de 0.01

alfa <- 0.01

# 4) La matriz de varianzas-covarianzas es esf�rica.
#     Para la comprobaci�n de esta condici�n, la funci�n ezAnova() entrega el resultado de la 
#     Prueba de esfericidad de Mauchly.

#Se establecen las hip�tesis para el test de esfericidad de Mauchly.
#H0: las varianzas-covarianzas de las muestras son iguales para los grupos.
#HA: al menos una de las muestras tiene varianza-covarianza diferente a alguna de los dem�s grupos. 

cat("\n\nProcedimiento ANOVA usando ezANOVA\n\n")
pruebaEzAnova <- ezANOVA(data = datos,
                         dv = tiempo,
                         within = algoritmo,
                         wid = Instancia,
                         type = 2,
                         return_aov = TRUE)
print(pruebaEzAnova)

#Gr�fico del tama�o del efecto.
g2 <- ezPlot(data = datos, 
             dv = tiempo,
             wid = Instancia, 
             within = algoritmo,
             x_lab = "Algoritmo",
             y_lab = "Diferencia promedio de tiempo de ejecuci�n respecto al �ptimo", 
             x = algoritmo)
print(g2)

#En base a la prueba realizada, obteniendo el p-value de la prueba de esfericidad y el p-value
# asociado a la correcci�n, se rechaza la hip�tesis nula en favor de la anternativa de la prueba
# de esfericidad de Mauchly, por lo que se puede asegurar con un 99% de confianza que existe 
# al menos una de las muestras que tiene varianza diferente a alguna de los dem�s grupos, por
# lo que la prueba de esfericidad de Mauchly falla, lo que motiva a realizar un factor de 
# correcci�n al p-value de la prueba Anova
# Este nuevo p-value corregido tambi�n es menor que el nivel de significaci�n establecido (alfa)
# por lo que se rechaza la hipotesis nula en favor de la hip�tesis alternativa del estudio Anova,
# por lo que se puede asegurar con un 99% de confianza que el tiempo de ejecuci�n promedio es 
#diferente para al menos uno de los algoritmos.

#Con todo lo anterior, se procede a realizar el an�lisis Post-Hoc con el fin de poder determinar
# que grupos presentan diferencias significativas en las muestras correlacionadas.

#En este caso se realiza el procedimiento de Bonferroni.
#Procedimiento post-hoc de Bonferroni.
bonferroni <- pairwise.t.test(datos[["tiempo"]],
                              datos[["algoritmo"]],
                              p.adj = "bonferroni", 
                              paired = TRUE)

cat("Correcci�n de Bonferroni\n") 
print(bonferroni)


#En base a estos resultados, considerando a su vez la gr�fica de tama�o del efecto, se desprende que
# si existen algoritmos mejores que otros. Espec�ficamente, entre G y R3 no existen diferencias
# significativas, donde ambos tienen un promedio de diferencias respecto al �ptimo bastante bajas,
# no as� R y R2 que presentan diferencias respecto al �ptimo bastante mas altas. As�, al momento
# de escoger entre estos cuatro algoritmos se recomienda elegir G o R3, ya que estos presentan mejores
# resultados de tiempo que R y R2.




###################################################################################################
########################################### Pregunta 2 ############################################
###################################################################################################

#El siguiente es (un resumen de) la descripci�n de un famoso experimento:
#  Naming the ink color of color words can be difficult. For example, if asked to name the color of
#  the word "blue" is difficult because the answer (red) conflicts with the word "blue." This
#  interference is called "Stroop Interference" after the researcher who first discovered the
#  phenomenon. This case study is a classroom demonstration. Students in an introductory
#  statistics class were each given three tasks. In the "words" task, students read the names of 60
#  color words written in black ink; in the "color" task, students named the colors of 60 rectangles;
#  in the "interference" task, students named the ink color of 60 conflicting color words. The times
#  to read the stimuli were recorded.
#
#El siguiente c�digo R carga los datos que se obtuvieron en este estudio. Con estos datos, responda la
#siguiente pregunta de investigaci�n: �hay diferencias en los tiempos entre tareas?

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

#Para ello, previamente se establecen las hip�tesis a estudiar:
#H0: el tiempo promedio es igual para todas las tareas
#HA: el tiempo promedio es diferente para al menos una de las tareas.

#Para realizar una prueba Anova de una v�a para muestras correlacionadas, se deben cumplir las siguientes
# condiciones:
#
# 1) La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos
# iguales.
#     Si, puesto que el tiempo, como toda magnitud f�sica, tiene una escala de int�rvalos iguales.
#
# 2) Las mediciones son independientes al interior de cada grupo.
#     Si; puesto que al tratarse de un experimento famoso, se asume que las instancias de tiempo fueron
#     registradas de manera independiente.
#
# 3) Se puede suponer razonablemente que la(s) poblaci�n(es) de origen sigue(n) una distribuci�n normal.
#     Para ello, se realiza la gr�fica de los grupos de estudio, en este caso, seg�n tipo de tarea
#     y se analiza si existen valores at�picos.

#   Comprobaci�n de normalidad .
g3 <-   ggqqplot(datos2, x = "tiempo", y = "tarea", color = "tarea") 
g3 <-   g3 + facet_wrap (~ tarea )
g3 <-   g3 + rremove("x.ticks") + rremove("x.text") 
g3 <-   g3 + rremove("y.ticks") + rremove("y.text") 
g3 <-   g3 + rremove("axis.title")
print(g3)

# En base a lo anterior, dado que no existen valores at�picos en los grupos, se puede asegurar
# que los grupos tienen una distribuci�n que aproxima a la normal. Por lo que se establece un 
# nivel de significancia de 0.05

alfa <- 0.05

# 4) La matriz de varianzas-covarianzas es esf�rica.
#     Para la comprobaci�n de esta condici�n, la funci�n ezAnova() entrega el resultado de la 
#     Prueba de esfericidad de Mauchly.

#Se establecen las hip�tesis para el test de esfericidad de Mauchly.
#H0: las varianzas-covarianzas de las muestras son iguales para los grupos.
#HA: al menos una de las muestras tiene varianza-covarianza diferente a alguna de los dem�s grupos. 

cat("\n\nProcedimiento ANOVA usando ezANOVA\n\n")
pruebaEzAnova2 <- ezANOVA(data = datos2,
                         dv = tiempo,
                         within = tarea,
                         wid = instancia,
                         type = 2,
                         return_aov = TRUE)
print(pruebaEzAnova2)

#Gr�fico del tama�o del efecto.
g4 <- ezPlot(data = datos2, 
             dv = tiempo,
             wid = instancia, 
             within = tarea,
             x_lab = "Tarea",
             y_lab = "Tiempo promedio de las tareas [s]", 
             x = tarea)
print(g4)

#Con este resultado, el p-value obtenido para la prueba de esfericidad de Mauchly resultado mayor que el  
# nivel de significaci�n establecido, con esto se falla al rechazar la hip�tesis nula, por lo tanto se  
# puede asegurar con un 95% de confianza que las varianzas-covarianzas de las muestras son iguales para 
# los grupos.

#Por otro lado, el p-value de la prueba Anova es mucho menor que el nivel de significancia (p ~ 3.82x10^-13)
# por lo que se rechaza la hip�tesis nula en favor de la hip�tesis alternativa, con lo que se puede asegurar
# con un 95% de confianza que el tiempo promedio es diferente para al menos una de las tareas.

#Por lo anterior, se procede a realizar un an�lisis post-hoc para estudiar cual o cuales de los grupos 
#presentan diferencias significacitvas respecto de los dem�s.

#En este caso se realiza el procedimiento de Bonferroni.
#Procedimiento post-hoc de Bonferroni.
bonferroni2 <- pairwise.t.test(datos2[["tiempo"]],
                              datos2[["tarea"]],
                              p.adj = "bonferroni", 
                              paired = TRUE)

cat("Correcci�n de Bonferroni\n") 
print(bonferroni2)

#Del resultado obtenido por bonferroni y el an�lisis de la gr�fica de tama�o de efecto
# se desprende que existen diferencias significativas entre  "interfer-colors" 
# e "interfer-words", de aqu� se puede concluir que el tiempo promedio que presenta
# una diferencia significativa corresponde a la tarea "interfer", esto quiere decir que
# al grupo estudiado le cuesta m�s (toma mayor tiempo realizar) la tarea "interfer" que
# las tareas "words" y "colors".







