library(dplyr)
#Comentario...
dir <- "C:/Users/Dell PC/Desktop/inferencia/clase1"
base <- "clase1.csv"
arch <- file.path(dir, base)

datos <- read.csv(arch)

magallanes <- datos %>% filter(Region == "Magallanes")
magallanesT <- t(magallanes[, 2:ncol(magallanes)])

fechas <- row.names(magallanesT)

#Posiciones fechas entre el 01-jun-2020 y el 31-dic-2020
i <- fechas >= "x2020.06.01" & fechas <= "x2020.12.31"

rango.fechas <- fechas[i]
rango.casos <- magallanesT[i, ]

#Dia con mayor casos con sintomas en la region de
#   Magallanes entre el 01-jun-2020 y el 31-dic-2020

imax <- which.max(rango.casos)
print(rango.fechas[imax])
