library(dplyr)

#Cargar conjuntos de datos
datos <- mtcars

#Renombrar las columnas 
datos <- datos %>% rename(Rendimiento = mpg, Cilindrada = cyl, Desplazamiento = disp,
                          Potencia = hp, Eje = drat, Peso = wt, Cuarto_milla = qsec,
                          Motor = vs, Transmision = am, Cambios = gear, 
                          Carburadores = carb)

#Dar formato categorico a las variables, renombrando sus niveles
datos[["Motor"]] <- factor(datos[["Motor"]], levels = c(0, 1),
                           labels = c("V","Recto"))

datos[["Transmision"]] <- factor(datos[["Transmision"]], levels = c(0, 1),
                           labels = c("Automático","Manual"))

#Dar formato ordinal a las variables, renombrando sus niveles
datos[["Cilindrada"]] <- factor(datos[["Cilindrada"]], levels = c(4, 6, 8),
                                 labels = c("4 cilindros","6 cilindros",
                                            "8 cilindros"))

datos[["Cambios"]] <- factor(datos[["Cambios"]], levels = c(3, 4, 5),
                                labels = c("3 cambios","4 cambios",
                                           "5 cambios"))

write.csv2(datos, "C:/Users/Dell PC/Desktop/inferencia/clase1/Mtcars.csv")


