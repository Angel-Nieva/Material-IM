library(dplyr)
library(ggpubr)
library(modeest)
# Leer un dataframe desde archivo csv .

dir  <- "C:/Users/Dell PC/Desktop/IME-2022/Actividades/act2"
base <- "EP02 Datos Casen 2017.csv"
arch <- file.path(dir, base)

datos <- read.csv2(arch, fileEncoding = "UTF-8")

hombres_rm <- datos %>% filter(region == "Región Metropolitana de Santiago" & sexo == "Hombre") 

# Se filtran los sueldos:
sueldos <- hombres_rm[["ytot"]]

# Estudiando las medidas estadísticas:
mediana <- median(sueldos)
# 400000
media <- mean(sueldos)
# 754535.5
moda <- mfv(sueldos)
# 11091
min <- min(sueldos)
#83
max <- max(sueldos)
# 72691664

# Se puede inferir que existe una gran brecha en los ingresos de los hombres

# Histograma para la variable sueldos .
g1 <- gghistogram (hombres_rm ,
                     x = "ytot",
                     bins = 30,
                     add = "mean",
                     xlab = "Ingresos total",
                     ylab = "Frecuencia",
                     color = "blue",
                     fill = "blue")
print (g1)

# Observando el histograma:

# Desviacion estandar
# varianza
# IQR
