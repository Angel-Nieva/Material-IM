library(dplyr)


dir <- "C:/Users/Dell PC/Desktop/IME/Material-IME/PEP2/Respuesta"
base <- "Datos PEP 2.csv"
arch <- file.path(dir,base)
datos <- read.csv2(arch, fileEncoding = "UTF-8")