#Se define la carpeta a utilizar, donde deben ir almacenados
#el archivo .csv y este script.
#Modificar la ruta para ser utilizado
setwd("C:\Users\Dell PC\Desktop\IME-2022\Actividades\act1")

#Se importa la librer?a a utilizar.
library(dplyr)

#Lectura de la tabla del archivo .csv sobre el covid.
datos <- read.csv2("EP01 Datos Covid.csv")

#Obtener los casos de contagio para la regi?n de ?uble.
nuble <- datos %>% filter(Region == "Ñuble")

#Obtener los datos del rango de d?as asignado
periodoAsignado <- select(nuble,X01.11.2020:X31.05.2021)

#Ordenar la cantidad de contagios por orden decreciente
ordenado <- sort(periodoAsignado,decreasing = TRUE)

#Imprimir por pantalla el valor de la primera columna, que correspode
#al mayor n?mero de casos con s?ntomas en ?uble
head(ordenado[1,1])


