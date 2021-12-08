library(dplyr)
# cargar conjunto de datos.
datos <- read.csv2("C:/Users/Dell PC/Desktop/inferencia/clase2/Mtcars.csv", stringsAsFactors =  TRUE,
                   row.names = 1)

resumen <- group_by(datos, Cambios) %>% summarise(count = n(), mean(Rendimiento),median(Rendimiento), sd(Rendimiento), 
                                                  IQR(Rendimiento), mean(Rendimiento))

print(resumen)


