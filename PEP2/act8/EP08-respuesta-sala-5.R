

#EJERCICIO PRACTICO N°8

#NOTA: PARA EVITAR POSIBLES ERRORES AL MOMENTO DE EJECUTAR EL SCRIPT NO SE
#ESCRIBEN PALABRAS CON TILDE NI ENIE.

##################################################################################################################
#ENUNCIADO#


# El conjunto de datos birthwt del paquete MASS contiene registros de 189 nacimientos en un centro
# medico de Springfield, Massachusetts, en 1986. Estos registros incluyen varias variables interesantes,
# entre ellas la raza de la madre y el peso de los bebes al nacer (en libras). Un importante equipo medico
# desea saber si existen diferencias significativas entre los pesos inicial de los bebes segun la raza de la
# madre.

#DESARROLLO#

#Se importan librerias necesarias

library(MASS)
library(tidyverse)
library(ggpubr)


#Se establece el nivel de significacion en 0.05
a <- 0.05

#Se importan los datos
datosOriginales <-birthwt

#Se seleccionan datos referentes a la raza y al peso, analizando los datos se puede ver que razas toman valores 1,2 o 3
datos <- datosOriginales %>% dplyr::select(race, bwt)

raza1 <- datos %>% filter(race==1)
raza1 <- unlist(raza1 %>% dplyr::select(bwt))

#Se calcula la varianza
raza1Var <- var(raza1)

raza2 <- datos %>% filter(race==2)
raza2 <- unlist(raza2 %>% dplyr::select(bwt))

#Se calcula la varianza
raza2Var <- var(raza2)

raza3 <- datos %>% filter(race==3)
raza3 <- unlist(raza3 %>% dplyr::select(bwt))

#Se calcula la varianza
raza3Var <- var(raza3)

#Se cambian las dimensiones de las razas 
length(raza2) <- length(raza1)
length(raza3) <- length(raza1)

comparacion<- data.frame(raza1, raza2, raza3)



datos <- comparacion %>% pivot_longer(c("raza1","raza2","raza3"),
                                            names_to = "Razas",
                                            values_to = "Pesos")

datos[["Razas"]] <- factor(datos[["Razas"]])

#Para realizar el analisis de los pesos se proceder? a usar el procedimiento ANOVA. Sin embargo, primero se deben cumplir
#ciertas condiciones:

#1) La escala con la que se mide la variable dependiente tiene las propiedades de una escala de intervalos iguales

# Se cumple la condicion anterior debido a que si en una instancia el peso para una raza es 108 y para otra es 98 existe
# una diferencia de 10, de igual forma que en otra instancia para las mismas razas (123 y 113 respectivamente). Por lo
# tanto se asume que la variable dependiente (pesos) se mide en base a propiedades de una escala de intervalos iguales.


#2) Las muestras se obtienen de forma aleatoria e independiente

# Se cumple la condicion debido a que se puede asumir que las muestras fueron escogidas aleatoriamente pues se trata de una
# institucion establecida, y por lo tanto se llevo a cabo un estudio serio; y por otra parte, la muestra es menor al 10% de 
# la poblacion.

#3) Se puede suponer que la poblacion de origen sigue una distribucion normal

# Para estudiar esta condicion se mostrara el grafico de analisis de normalidad Q-Q

g <- ggqqplot(datos,
              x = "Pesos",
              y = "Razas",
              color = "blue")

g <- g + facet_wrap(~ Razas)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# Se puede observar la inexistencia de valores at?picos, por lo que la suposicion de normalidad
# es correcta.

#4) Las k muestras tienen una varianza aproximadamente iguales

# Para verificar esta condicion se obtiene la proporcion entre la maxima y minima varianza
maximo <- max(raza1Var, raza2Var, raza3Var)

minimo <- min(raza1Var, raza2Var, raza3Var)

pruebaN <- maximo/minimo

# Como el valor de la division es menor a 1.5 se estima que las muestras tienen varianzas
# aproximadamente iguales

#Es asi como se procede con el m?todo ANOVA de una via para muestras independientes, planteando las hipotesis
#del problema.

#Hipotesis:

#En lenguaje natural:

#H0: No existen diferencias significativas entre los pesos iniciales de los bebes de acuerdo a sus razas
#HA: Para almenos un tipo de raza existe una variacion ignificativa con respecto a los pesos iniciales de los bebes (gr)

# Se procede con la prueba ANOVA

cat("Procedimiento ANOVA aov\n\n")
prueba <- aov(Pesos ~ Razas, data = datos, type=3)
print(summary(prueba))

# Es asi como el p-valor es igual a 0.00834 el cual es menor al nivel de significacion a = 0.05, por lo
# tanto hay evidencia suficiente para rechazar la hipotesis nula, por lo que se puede concluir que
# existen diferencias significativas entre los pesos. Ahora se procedera a realizar el procedimiento
# POST-HOC HSD de Tukey, debido a que es mas conservadora.

post_hoc <- TukeyHSD(prueba,
                     "Razas",
                     ordered = TRUE,
                     conf.level = 1- a)

print(post_hoc)

#post_hoc2 <- pairwise.t.test(datos[["Pesos"]] ,
#                            datos[["Razas"]], 
#                            p.adj = "holm",
#                            pool.sd = TRUE,
#                            paired = FALSE,
#                            conf.level = 1 - a)
#print(post_hoc2)

# A partir de la variable post_hoc se concluye que existen diferencias significativas entre los pesos
# de las razas 1-2 y 1-3, pues sus valores p de la prueba post-hoc son menores al alfa (0.0428< 0.05 y
# 0.0260< 0.05)

