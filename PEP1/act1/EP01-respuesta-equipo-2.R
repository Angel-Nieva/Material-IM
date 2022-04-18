# Instalar libreria
# install.packages("dplyr")

library(dplyr)

# Leer un dataframe desde archivo csv .

dir  <- "C:/Users/Dell PC/Desktop/IME-2022/Actividades/act1"
base <- "EP01 Datos Covid.csv"
arch <- file.path(dir, base)

datos <- read.csv2(arch)

# ¿qué variables se han cargado?
# Region y fechas casos diarios Covid

# ¿qué tipo tiene cada una de estas variables?
# Region: Categórica ordinal
# Fechas Covid: Numérica discreta

# ¿qué escala parecen tener estas variables?
# Region: Escala nominal
# Fechas Covid: Escala de razón

# Las variables "Fechas Covid" son de tipo int, por lo que son del tipo Numérica.
# La variable "Region" es de tipo chr, por lo que se le da un formato categórico.

datos [["Region"]] <- factor ( datos [["Region"]])


# Preguntas grupo 2:

# ¿Qué día se produjo el mayor número de casos con síntomas en la región del Maule entre el 01-oct-2020 y el 31-may-2021?

tarapaca <- datos %>% filter(Region == "Tarapacá")
