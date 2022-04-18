#Se define la carpeta a utilizar, donde deben ir almacenados
#el archivo .csv y este script.
#Modificar la ruta para ser utilizado
setwd("C:/Users/Dell PC/Desktop/IME-2022/Actividades/act2")

#Lectura de la tabla del archivo .csv.
Datos <- read.csv2("EP02 Datos Casen 2017.csv")
#Las variables de esta tabla y que nos importan para 
#resolver la pregunta son las siguientes:
#sexo: sexo de la persona registrada (Hombre o Mujer)
#edad: edad de la persona registrada (a?os)
#ytot: ingreso total de la persona registrada (pesos)

#Se importa la librer?a a utilizar.
if(!require(dplyr)){
  install.packages("dplyr",dependencies = TRUE)
  require(dplyr)
}

if(!require(ggpubr)){
  install.packages("ggpubr",dependencies = TRUE)
  require(ggpubr)
}

#Se obtiene un frame data donde la variable sexo solo posee el valor Mujer.
#El frame data resultate solo posee 3 variables: sexo, edad e ytot.
DatosMujeres <- Datos %>% filter(sexo == "Mujer")

#Del frame data anterior, se selecionan solo las columnas de edad y ingreso total
#El frame data resultate solo posee 2 variables:edad e ytot.
DatosMujeres <- DatosMujeres %>% select(edad,ytot)

#Se realiza una agrupaci?n de los datos por edad, y luego se calcula el promedio
#del ingreso total de cada grupo etario.
#Este frame data tiene 2 variables: edad y total
DatosAgrupados <- group_by(DatosMujeres,edad) %>% summarise("total" = mean(ytot))

#Se utiliza la funci?n ggscatter graficar los datos usando un gr?fico
#de dispersi?n.
g1 <- ggscatter(DatosAgrupados,
                  x = "edad",
                  y = "total",
                  xlab = "Edad mujer",
                  ylab = "Ingreso mujer")

#Se imprime la gr?fica en pantalla, quedado en la vi?eta Plots
print(g1)
#Se imprime por consola una breve respuesta a la pregunta
cat("Del gr?fico se aprecia que existen rangos et?rios donde el ingreso de las mujeres desciende conforme aumenta la edad.\n")