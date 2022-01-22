#
library(dplyr)
library(ggpubr)
library(ggplot2)
library(simpleboot)
library(ez)

dir <- "C:/Users/Dell PC/Desktop/IME/Material-IME/PEP2/Respuesta"
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

# Las hipótesis a formular:
# H0: Las evaluaciones promedio hechas por los capitanes en las distintas divisiones son iguales.
# HA: Las evaluaciones promedio hechas por los capitanes en las distintas divisiones es diferente para
#     al menos un algoritmo.

# Se obtienen las distintas divisiones:
cavetrooper  <- datos %>% filter(division == "Cavetrooper")
snowtrooper  <- datos %>% filter(division == "Snowtrooper")
lavatrooper  <- datos %>% filter(division == "Lavatrooper")
shoretrooper  <- datos %>% filter(division == "Shoretrooper")
spacetrooper  <- datos %>% filter(division == "Spacetrooper")
sandtrooper  <- datos %>% filter(division == "Sandtrooper")
flametrooper  <- datos %>% filter(division == "Flametrooper")
recontrooper  <- datos %>% filter(division == "Recontrooper")

# Se obtienen las evaluaciones de los soldados por sus capitanes:
cavetrooper  <- cavetrooper$eval_capitan
snowtrooper  <- snowtrooper$eval_capitan
lavatrooper  <- lavatrooper$eval_capitan
shoretrooper <- shoretrooper$eval_capitan
spacetrooper <- spacetrooper$eval_capitan
sandtrooper  <- sandtrooper$eval_capitan
flametrooper <- flametrooper$eval_capitan
recontrooper <- recontrooper$eval_capitan

# Se estudia la normalidad de las tropas:
n1 <- shapiro.test(cavetrooper)
n2 <- shapiro.test(snowtrooper)
n3 <- shapiro.test(lavatrooper)
n4 <- shapiro.test(shoretrooper)
n5 <- shapiro.test(spacetrooper)
n6 <- shapiro.test(sandtrooper)
n7 <- shapiro.test(flametrooper)
n8 <- shapiro.test(recontrooper)

# n1 p-value = 0.6662
# n2 p-value = 0.641
# n3 p-value = 0.5838
# n4 p-value = 0.1939
# n5 p-value = 0.554
# n6 p-value = 0.2406
# n7 p-value = 0.8942
# n8 p-value = 0.6213

# Al observar los p-value de la prueba de normalidad podemos concluir que las
# tropas de las diferentes divisiones se comportan de manera normal.

datos <- data.frame(cavetrooper, snowtrooper, lavatrooper, shoretrooper, spacetrooper, 
                    recontrooper, sandtrooper, flametrooper)

# Llevar data frame a formato largo .
datos <- datos %>% pivot_longer ( c("cavetrooper", "snowtrooper", "lavatrooper",
                                    "shoretrooper","spacetrooper", "recontrooper",
                                    "sandtrooper", "flametrooper"),
                                  names_to = "division",
                                  values_to = "evaluación")

datos [["instancia"]] <- factor (1: nrow ( datos ))

# Procedimiento ANOVA con ezANOVA ().
cat ("\n\ nProcedimiento ANOVA usando ezANOVA \n\n")
prueba <- ezANOVA (
  data = datos ,
  dv = evaluación ,
  between = division ,
  wid = instancia ,
  return_aov = TRUE )

print ( prueba2 )
cat("\n\nY factores de correcion para cuando no se cumple la\ncondicion de esfericidad:\n\n")
print(prueba$'Sphericity Corrections')

#cat("\n\nY factores de correcion para cuando no se cumple la\ncondicion de esfericidad:\n\n")
#print(prueba$'Sphericity Corrections')


