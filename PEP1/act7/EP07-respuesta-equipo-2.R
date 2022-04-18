
# ******************************************** PREGUNTA 1 ********************************#
# Se ha realizado un estudio acerca de la prevalencia de trastornos del lenguaje con 
# un grupo de 9 niñas y 11 niños de segundo básico. Los datos obtenidos muestran 
# que 10 de los niños presentan dificultades, mientras que solo 3 de las niñas 
# lo hacen. ¿Existe relación entre el sexo y la prevalencia de trastornos del lenguaje?
# ****************************************************************************************#  

# Objetivo:
# Se desea determinar si las variables categóricas sexo y trastornos, de una misma población,
# son estadísticamente independientes o si, por el contrario, están relacionadas.

# Como las observaciones esperadas son menores a 5 y ambas variables son dicotómicas se procede a usar
# la prueba de Fisher:

# Como nos indica el enunciado, se puede suponer que los niños fueron seleccionados de manera
# aleatoria, por lo que son independientes entre sí.

# Se formula la hipótesis:

# H0: Las variables son independientes entre sí; (No existe una relación entre el sexo de los niños y 
#                                                 la prevalencia de trastornos del lenguaje)
# Ha: Las variables están relacionadas; (Si existe una relación entre el sexo de los niños y 
#                                                 la prevalencia de trastornos del lenguaje)

# Crear tabla de contingencia:
sexo <- c(rep("Niñas", 9), rep("Niños", 11))
transtorno <- c(rep("Si", 13), rep("No", 7))
datos <- data.frame(transtorno, sexo)
tabla <- xtabs(~., datos)
print(tabla)

# Nivel de significacion
alfa <- 0.05

# Se aplica la prueba de Fisher 
prueba <- fisher.test(tabla, 1-alfa)
print(prueba)
# p-value = 0.004721

# Conclusion:

# Como p < alfa, se rechaza la hipótesis nula en favor de la hipótesis alternativa,
# por lo que se concluye con un 95% de confianza, que si existe una relación entre el 
# sexo de los niños y la prevalencia de trastornos del lenguaje.

# ******************************************** PREGUNTA 2 ********************************#
# Siempre tenaz en su lucha para erradicar a los vampiros de la faz de la tierra, Van Helsing desea probar
# una vacuna que, según él, causará una grave enfermedad en estos seres una vez que beban la sangre de
# sus víctimas. Para ello, ha almacenado una gran cantidad de dosis de su propia sangre, separadas en dos
# grupos: uno de ellos contiene el químico de la vacuna, mientras el otro está completamente limpio.
# Adicionalmente, Van Helsing cuenta con 13 vampiros cautivos, a los que alimentó con sangre limpia por
# una semana. Luego de un periodo de limpieza (durante el cual los vampiros fueron alimentados con su
# dieta normal, por lo que eliminaron todo rastro de la sangre de Van Helsing), repitió el experimento con la
# sangre que contiene la vacuna. Para ambos casos, registró cuántos vampiros enfermaron, con los
# siguientes resultados:
#  * 3 vampiros no presentaron enfermedad alguna con ninguna de las dietas de Van Helsing.
#  * 2 vampiros enfermaron tras ambas dietas de Van Helsing.
#  * 2 vampiros enfermaron con la sangre limpia de Van Helsing, pero no con la sangre que contiene la
#   vacuna.
#  * 6 vampiros enfermaron con la sangre que contiene la vacuna, pero no con la sangre limpia de Van
#  Helsing.
#  ¿Es posible decir que la vacuna de Van Helsing está asociada a una enfermedad en los vampiros?
# ****************************************************************************************# 

# Objetivo:
# Se desea determinar si, dado dos mediciones distintas para la misma muestra (vampiros), 
# la vacuna creada por Van Helsing produce un cambio significativo (enferma o no a los vampiros).

# Como nos indica el enunciado, se puede suponer que los vampiros fueron seleccionados de manera
# aleatoria, por lo que son independientes entre sí.

# Se formula la hipótesis:

# H0: No hay cambios significativos en las respuestas; (No hay un cambio significativo en los vampiros luego de probar
#                                                       la sangre limpia y la sangre con la vacuna)
# Ha: Sí hay cambios significativos en las respuestas; (Si hay un cambio significativo en los vampiros luego de probar
#                                                       la sangre limpia y la sangre con la vacuna)


# Se construye la tabla de contingencia
vampiros <- seq(1:13)
limpia <- c(rep("sano",3), rep("enfermo",2), rep("enfermo",2), rep("sano",6)) 
vacuna <- c(rep("sano",3), rep("enfermo",2), rep("sano",2), rep("enfermo",6))
tabla2 <- table(vacuna, limpia)
print(tabla2)

# Nivel de significacion
alfa <- 0.05

#Se aplica la prueba
prueba2 <- mcnemar.test(tabla2)
print(prueba2)
# p-value = 0.2888

# Como p > alfa, se falla en rechazar la hipótesis nula en favor de la hipótesis alternativa,
# por lo que se concluye con un 95% de confianza, que no hay un cambio significativo en los vampiros 
# luego de probarla sangre limpia y la sangre con la vacuna (la vacuna no está asociada a una enfermedad
# en los vampiros o la vacuna no es efectiva ante vampiros).

# ******************************************** PREGUNTA 3 ********************************#
# El 21 de marzo de 2022 se realizó un estudio acerca de la aprobación al presidente Gabriel Boric entre los
# estudiantes de una prestigiosa universidad a fin de comparar los resultados con los obtenidos en la misma
# encuesta a nivel nacional, obteniéndose los resultados que se muestran en la tabla. ¿Refleja la opinión
# estudiantil la percepción del país?
# ****************************************************************************************# 

# Objetivo:
# Se desea determinar si la opinion de la muestra de estudiantes universitarios es representativa a la encuesta
# nacional acerca de la aprobación al presidente Gabriel Boric.

# Se formula la hipótesis:

# H0: Las proporciones de las opiniones de los estudiantes son iguales para las que se registran a nivel nacional. 
# Ha: Las proporciones de las opiniones de los estudiantes son diferentes para las que se registran a nivel nacional.

# Dado que se necesita comprobar si la distribución de frecuencias observadas (opinión de estudiantes) se 
# asemeja a una distribución esperada (encuesta nacional) se utilizará la prueba chi-cuadrado de
# bondad de ajuste.


# Condiciones a verificar:
# Las observaciones deben ser independientes entre sí.
# Debe haber, a lo menos, 5 observaciones esperadas en cada grupo.

# Puesto que la muestra de estudiantes representa menos del 10% de la poblacion y fue elegida de manera aleatoria,
# las observaciones son independientes entre si.

# Se crea una tabla de contingencia para observar si hay más de 5 observaciones en cada grupo.

estudiantes <- c(208, 7, 2)
nacional <- c(5046, 3421, 706)
tabla3 <- as.table(rbind(estudiantes, nacional))

dimnames(tabla3) <- list(poblacion = c("Estudiante", "Nacional"),
                         aprobacion = c("Aprueba", "Desaprueba", "Ninguna"))

print(tabla3)

# Se verifican si se esperan más de 5 observaciones en cada grupo
n_nacional <- sum(nacional)
n_estudiantes <- sum(estudiantes)
proporciones <- round(nacional/n_nacional, 3)
esperados <- round(proporciones*n_estudiantes, 3)
print(esperados)
# 119.350 80.941 16.709

# Por lo tanto, se comprueba que existen mas de 5 valores esperados para cada grupo.

#Ahora se realiza la prueba chi-cuadrado de bondad de ajuste

alfa = 0.05

prueba3 <- chisq.test(tabla3, correct = FALSE)
print(prueba3)
# p < 2.2e-16

# Como p < alfa, se rechaza la hipótesis nula en favor de la hipótesis alternativa,
# por lo que se concluye con un 95% de confianza que las proporciones de las opiniones 
# de los estudiantes son diferentes para las que se registran a nivel nacional.
# La opinión estudiantil no refleja la percepción nacional.

# ******************************************** PREGUNTA 4 ********************************#
# La Facultad de Ingeniería desea saber si existe diferencia significativa en el desempeño de los estudiantes
# en asignaturas críticas de primer semestre. Para ello, le ha entregado un archivo de datos que, para 3
# asignaturas, indica si una muestra de 50 estudiantes aprobó o reprobó. ¿Qué puede concluir la Facultad?
# Indicación: obtenga la muestra a partir del archivo “EP07 Datos.csv” que se encuentra en el directorio
# compartido, usando la semilla 440. Considere un nivel de significación α=0,05.
# ****************************************************************************************# 

set.seed(440)
alfa = 0.05

# Lectura de archivo 
dir <- "C:/Users/Dell PC/Desktop/IME-2022/Actividades/act7"

basename <- "EP07 Datos.csv"
file <- file.path(dir, basename)
poblacion <- read.csv2(file = file, fileEncoding = "UTF-8")

# Objetivo:
# Se desea determinar si existe una diferencia significativa en el desempeño de los estudiantes en asignaturas
# críticas del primer semestre (aprobación en los ramos de Calculo, Algebra y Física).

# Se formula la hipótesis:

# H0: la proporción de "éxitos" es la misma para todos los grupos; (No hay un cambio significativo entre las asignaturas criticas
#                                                                    de primer semestre)
#
# Ha: la proporción de "éxitos" es distinta para al menos un grupo; (Si hay un cambio significativo entre las asignaturas criticas
#                                                                    de primer semestre)
#

# Dado que la variable de respuesta es dicotómica y la variable independiente tiene más de dos observaciones 
# pareadas (Calculo, Algebra, Física), se procede a utilizar la prueba Q de Cochran.

# Condiciones:
# 1) La variable de respuesta es dicotómica.
# 2) La variable independiente es categórica.
# 3) Las observaciones son independientes entre sí.
# 4) El tamaño de la muestra es lo suficientemente grande. Glen (2016a) sugiere que n*k >= 24, donde n es
#    el tamaño de la muestra (la cantidad de instancias, para el ejemplo) y k, la cantidad de niveles en la
#    variable independiente.

# Identificando las respectivas variables:

# variable de respuesta (dicotómica): 
# A, si el estudiante aprobó el curso
# R, si el estudiante reprobó el curso

# variable independiente (categórica):
# Corresponde a las tres compañías mencionadas (Calculo, Algebra y Física)

# Se puede suponer razonable que las 50 muestras fueron seleccionadas de manera aleatoriamente, por lo que
# son independientes entre sí.

# El tamaño de la muestra es de 1500, muy superior a los 24 necesarios. Con esto se cumplen todas las condiciones,
# por lo que se procede a utilizar la prueba Q de Cochran.

library ( tidyverse )
library ( RVAideMemoire )
library ( rcompanion )

# Llevar matriz de datos a formato largo .
poblacion <- poblacion %>% pivot_longer(c( "Calculo", "Algebra", "Fisica"),
                                        names_to = "cursos",
                                        values_to = "resultado")


poblacion[["Id"]] <- factor(poblacion[["Id"]])
poblacion[["cursos"]] <- factor(poblacion[["cursos"]])

# Hacer prueba Q de Cochran .
prueba4 <- cochran.qtest ( resultado ~ cursos | Id, data = poblacion , alpha = alfa)
print (prueba4)
# p = 9.53e-08

# Como p < alfa, se rechaza la hipótesis nula en favor de la hipótesis alternativa,
# por lo que se concluye con un 95% de confianza que si hay un cambio significativo entre las asignaturas de primer año.
# Al menos una de las asignaturas tiene una proporcion de exito distinta a las demas.

# Como no podemos saber cual de las asignaturas presenta una diferente proporcion de "exitos", se procede
# a utilizar una prueba post-hoc.

# Procedimiento post -hoc con corrección de Holm .
post_hoc <- pairwiseMcnemar( resultado ~ cursos | Id ,
                            data = poblacion , method = "holm")

cat("\nProcedimiento post-hoc con corrección de Holm \n")
print( post_hoc)

# Dado el procedimiento post-hoc, se puede concluir que existen diferencias significativas
# entre todas las asignaturas, Algebra - Calculo, Algebra - Física, Calculo -Física, ya que
# los valores p ajustados son menores a alfa para todos los casos.
