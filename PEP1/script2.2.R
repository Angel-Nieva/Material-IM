# cargar conjunto de datos.
datos <- read.csv2("C:/Users/Dell PC/Desktop/inferencia/clase2/Mtcars.csv", stringsAsFactors =  TRUE,
                   row.names = 1)

# Calculo de percentiles para la variable Rendimiento
cat("Cuartiles:\n")
print(quantile(datos[["Rendimiento"]]))
cat("\n")

cat("Quintiles:\n")
print(quantile(datos[["Rendimiento"]], seq(0, 1, 0.2)))
cat("\n")

cat("Deciles:\n")
print(quantile(datos[["Rendimiento"]], seq(0, 1, 0.1)))
cat("\n")

cat("Percentiles:\n")
print(quantile(datos[["Rendimiento"]], seq(0, 1, 0.01)))
cat("\n")