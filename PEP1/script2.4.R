library(dplyr)
# cargar conjunto de datos.
datos <- read.csv2("C:/Users/Dell PC/Desktop/inferencia/clase2/Mtcars.csv", stringsAsFactors =  TRUE,
                   row.names = 1)

# Crear tabla de contingencia para la variable gear.
contingencia <- table(datos[["Cambios"]])
cat("Tabla de contingencia generada con table():\n")
print(contingencia)
cat("\n")

# Otra forma de crear la misma tabla
contingencia <- xtabs(~ Cambios, data = datos)
cat("Tabla de contingencia generada con xtabs():\n")
print(contingencia)
cat("\n")

# Calcular totales por fila y mostrarlos por separado
totales <- marginSums(contingencia)
cat("Totales por fila:\n")
print(totales)
cat("\n")

# Calcular totales por fila y agregarlos a la tabla
con_totales <- addmargins(contingencia, 1)
cat("Tabla de contingencia con totales por fila:\n")
print(con_totales)
cat("\n")

# Convertir a tabla de proporciones
proporciones <- prop.table(contingencia)
proporciones <- addmargins(proporciones,1)
cat("Tabla de contingencia con proporciones:\n")
print(proporciones)
cat("\n")

# Convertir a tabla de porcentajes con 2 decimales.
porcentajes <- round(prop.table(contingencia), 4) * 100
porcentajes <- addmargins(porcentajes)
cat("Tabla de contingencia con porcentajes:\n")
print(porcentajes)
cat("\n")


