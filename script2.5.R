# cargar conjunto de datos.
datos <- read.csv2("C:/Users/Dell PC/Desktop/inferencia/clase2/Mtcars.csv", stringsAsFactors =  TRUE,
                   row.names = 1)

# Crear tabla de contingencia para las variables transmision y cambios
contingencia <- table(datos[["Transmision"]], datos[["Cambios"]])
cat("Tabla de contingencia generada con table():\n")
print(contingencia)
cat("\n")

# Otra forma de crear la misma tabla
contingencia <- xtabs(~ Transmision + Cambios, data = datos)
cat("Tabla de contingencia generada con xtabs():\n")
print(contingencia)
cat("\n")

# Proporciones con totales por columna
proporciones_columna <- prop.table(contingencia, margin=2)
proporciones_columna <- addmargins(proporciones_columna, margin = 1)
cat("Tabla de contingencia con proporciones totales por columna:\n")
print(proporciones_columna)
cat("\n")

# Proporciones con totales
proporciones <- prop.table(contingencia)
proporciones <- addmargins(proporciones)
cat("Tabla de contingencia con proporciones totales:\n")
print(proporciones)
cat("\n")
