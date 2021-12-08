# Limpiar entorno de trabajo
# rm(list = ls())

# cargar conjunto de datos.
datos <- read.csv2("C:/Users/Dell PC/Desktop/inferencia/clase2/Mtcars.csv", stringsAsFactors =  TRUE,
                   row.names = 1)

# Calcular la media para la variable Rendimiento.
media <- mean(datos[["Rendimiento"]])
cat("Rendimiento medio:",media,"\n\n")

# Calcular la media para la tercera y quinta columnas.
# (variables Desplazamiento y Eje).
cat("medias\n")
print(sapply(datos[c(3, 5)], mean))
cat("\n")

# Calcular la media para las columnas 3 a 6
# (variables Desplazamiento, Potencia, Eje y Peso).

cat("Medias\n")
print(sapply(datos[3:6], mean))
cat("\n")

# Calcular la media para la variable Rendimiento omitiendo valores faltantes
print(mean(datos[["Rendimiento"]], na.rm = TRUE))