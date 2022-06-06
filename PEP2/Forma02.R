#
library(dplyr)
library(ggpubr)
library(ggplot2)
library(simpleboot)
library(ez)

dir <- "C:/Users/Dell PC/Desktop/inferencia/IME/Material-IME/PEP2/Respuesta"
base <- "Datos PEP 2.csv"
arch <- file.path(dir,base)
datos <- read.csv2(arch, fileEncoding = "UTF-8")

##############################
#####     Pregunta 1     #####
##############################
# (23 puntos) Lord Vader desea saber si los niveles de exigencia con que los capitanes de las 
# diferentes divisiones evalúan a los nuevos soldados son similares, por lo que le ha solicitado 
# estudiar si existen diferencias significativas en el promedio de la evaluación realizada por 
# el capitán entre las distintas divisiones. El Lord Sith ha sido muy claro al solicitar un 
# reporte de aquellas divisiones en las que se observen diferencias. 

##############################
#####     Desarrollo     #####
##############################

#Objetivo: se necesita determinar si existen diferencias en los niveles de exigencia con que los capitanes
# de las diferentes divisiones evaluan a los nuevos soldados.

# Las hipótesis a formular:
# H0: Las evaluaciones promedio hechas por los capitanes en las distintas divisiones son iguales.
# HA: Las evaluaciones promedio hechas por los capitanes en las distintas divisiones es diferente para
#     al menos un algoritmo.

datos <- datos %>% select(id, division, eval_capitan)
datos[["id"]] <- factor(datos[["id"]])
datos[["division"]] <- factor(datos[["division"]])


g <- ggqqplot(datos,
              x = "eval_capitan",
              y = "division",
              color = "division")
g <- g + facet_wrap(~ division)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

alfa <- 0.05
homo <- ezANOVA(data = datos,
                  dv = eval_capitan,
                  between = division,
                  wid = id,
                  return_aov = TRUE)
cat ("El resultado de la prueba de homocedasticidad de Levene :\n\n")
print(homo["Levene's Test for Homogeneity of Variance"])
# p = 0.4717192

prueba <- aov( eval_capitan ~ division , data = datos )

print(summary(prueba))
# p = 2e-16

# Según los resultados de la prueba aov, como p < alfa, se rechaza la hipótesis nula en favor de 
# la hipótesis alternativa, por lo que se concluye con un 99% de confianza que el

# Ahora se procederá a realizar el procedimiento
# POST-HOC HSD de Tukey, debido a que es más poderosa que los factores de corrección
# de Bonferromi y Holm.

# Grafico del tamaño de efecto:
g2 <- ezPlot( data = datos,
              dv = eval_capitan,
              between = division,
              wid = id,
              y_lab = "Evaluacion capitan [s]",
              x = division
)
print(g2)

# Se puede apreciar una fuerte diferencia de SnowTrooper y 
# Sandtrooper entre todas las demas divisiones.
 
post_hoc <- TukeyHSD(prueba,
                     "division",
                     ordered = TRUE,
                     conf.level = 1 - alfa)
print(post_hoc)

# Segun los resultados post-hoc, se pueden ver dos grandes
# diferencias. Primero, existe una diferencia significativa
# entre las evaluaciones hechas por los capitanes para la division
# de Santrooper y todas las demas divisiones (p = 0 < alfa)
# y la division de Snowtrooper y todas las demas divisiones
# (p = 0 < alfa).

