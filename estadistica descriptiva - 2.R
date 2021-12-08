library(dplyr)
library(ggpubr)
# cargar conjunto de datos.

datos <- read.csv("C:/Users/Dell PC/Desktop/inferencia/clase3/Casen 2017.csv", fileEncoding = "UTF-8")

# Identificación de los tipos de variables y sus escalas:

# folio: discreta y escala de razón 
# o: discreta y escala de razón
# id.vivienda: discreta y escala de razón
# hogar: discreta y escala de razón
# region: nominal y escala nominal
# provincia: nominal y escala nominal
# comuna: nominal y escala nominal
# ing.comuna: discreta y escala de razón
# zona: nominal y escala nominal
# sexo: nominal y escala nominal
# edad: discreta y escala de razón
# ecivil: nominal y escala nominal
# ch1: nominal y escala nominal
# ytot: continua y escala de intervalo

#Dar formato categórico a las variables tipo nominal y ordinal 
#datos[["folio"]] <- factor(datos[["folio"]])
#datos[["o"]] <- factor(datos[["o"]])
#datos[["id.vivienda"]] <- factor(datos[["id.vivienda"]])
#datos[["hogar"]] <- factor(datos[["hogar"]])
datos[["region"]] <- factor(datos[["region"]], labels = c("RM"))
datos[["provincia"]] <- factor(datos[["provincia"]])
datos[["comuna"]] <- factor(datos[["comuna"]])
#datos[["ing.comuna"]] <- factor(datos[["ing.comuna"]])
datos[["zona"]] <- factor(datos[["zona"]])
datos[["sexo"]] <- factor(datos[["sexo"]])
#datos[["edad"]] <- factor(datos[["edad"]])
datos[["ecivil"]] <- factor(datos[["ecivil"]])
datos[["ch1"]] <- factor(datos[["ch1"]])
#datos[["ytot"]] <- factor(datos[["ytot"]])


# Pregunta a responder:
# ¿Tiene relación el ingreso de las mujeres de la RM con el riqueza del municipio donde habita?

# Para este ejercicio no es necesario calcular ninguna medida estadística, basta con calcular el grafico
#   de dispersión que nos permite observar si dos variables están relacionadas.


# Filtrar las mujeres en la region metropolitana
mujeresRM <- datos %>% filter(region == "RM" 
                              & sexo == "Mujer")

# Sumar el ingreso de las mujeres de cada comuna
m <- cbind(mujeresRM["ing.comuna"], mujeresRM["ytot"])
m.aux <- c(0,0)
vector.aux = m[1, ]
for(fila in 1:nrow(m)){
  if (m[fila,1] == vector.aux[1,1]){
    vector.aux[1,2] = vector.aux[1,2]+m[fila,2]
  }
  else{
    m.aux <- rbind(m.aux, vector.aux)
    vector.aux = m[fila, ]
  }
}

# Grafico de dispersion positivo 
g <- ggscatter(m.aux,
               x = "ing.comuna",
               y = "ytot",
               title = "Ingreso mujer v/s ranking riqueza municipio",
               xlab = "Ranking municipio",
               ylab = "Ingreso mujer [pesos]") 
print(g)

# Respuesta:
# Si, ya que al aumentar el ranking de ingresos la comuna también se puede apreciar un aumento de 
#   los ingresos totales de las mujeres en dicha comuna

