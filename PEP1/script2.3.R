library(dplyr)
# cargar conjunto de datos.
datos <- read.csv2("C:/Users/Dell PC/Desktop/inferencia/clase2/Mtcars.csv", stringsAsFactors =  TRUE,
                   row.names = 1)

# Calculo de varias medidas para la variable Potencia
medidas_potencia <- datos %>% summarise(Media = mean(Potencia),
                                        Mediana = median(Potencia),
                                        Varianza = var(Potencia),
                                        IQR = IQR(Potencia))

print(medidas_potencia)
cat("\n")

# Calculo de la media y la desviacion estandar para las variables
# Peso y Cuarto_milla
medidas_varias <- datos %>% summarise(Media_p = mean(Peso),
                                      Media_C = median(Cuarto_milla),
                                      SD_P = sd(Peso),
                                      SD_C = sd(Cuarto_milla))
print(medidas_varias)
cat("\n")