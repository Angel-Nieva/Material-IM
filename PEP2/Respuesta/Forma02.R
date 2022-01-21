#
library(readxl)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(simpleboot)

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


# Se obtienen las distintas divisiones:
cavetrooper  <- datos %>% filter(division == "Cavetrooper")
snowtrooper  <- datos %>% filter(division == "Snowtrooper")
lavatrooper  <- datos %>% filter(division == "Lavatrooper")
shoretrooper  <- datos %>% filter(division == "Shoretrooper")
spacetrooper  <- datos %>% filter(division == "Spacetrooper")
sandtrooper  <- datos %>% filter(division == "Sandtrooper")
flametrooper  <- datos %>% filter(division == "Flametrooper")
recontrooper  <- datos %>% filter(division == "Recontrooper")

#cavetrooper <- cavetrooper %<% filter()