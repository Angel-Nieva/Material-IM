library(dplyr)
library(ggpubr)

# Se lee un dataframe desde archivo csv .

dir  <- "C:/Users/Dell PC/Desktop/IME-2022/Actividades/act2"
base <- "EP02 Datos Casen 2017.csv"
arch <- file.path(dir, base)

datos <- read.csv2(arch, fileEncoding = "UTF-8")

# Tipos de variables relevantes:
# region: nominal
# sexo: nominal
# ytot: continua

# Se le da formato categórico a las variables tipo nominal:
datos[["region"]] <- factor(datos[["region"]])
datos[["sexo"]] <- factor(datos[["sexo"]])


# Se filtran los hombres de la Región Metropolitana:
hombres_rm <- datos %>% filter(region == "Región Metropolitana de Santiago" & sexo == "Hombre") 

# Cálculo de varias medidas para la variable ingresos (ytot) .
medidas_ingresos <- hombres_rm %>% summarise ( Media = mean ( ytot ),
                                          Mediana = median ( ytot ),
                                          Varianza = var( ytot ),
                                          IQR = IQR( ytot ))
print ( medidas_ingresos )
cat ("\n")

# Media = 754535.5
# Mediana = 400000
# Varianza = 2.134465e+12
# IQR = 475000

# El promedio de ingresos se encuentra entre los 754535.5, además podemos inferir que existe una
# gran brecha entre los ingresos al comparar la media con la varianza.

# Para observar de mejor manera el comportamiento de los datos se procede a utilizar un histograma.
# Histograma para la variable sueldos .

g1 <- gghistogram (hombres_rm ,
                     x = "ytot",
                     bins = 30,
                     add = "mean",
                     xlab = "Ingreso total",
                     ylab = "Frecuencia",
                     color = "blue",
                     fill = "blue")
print (g1)

# ¿Cómo diría que es el ingreso de los hombres de la RM? 
# (simétrico/asimétrico, concentrado/disperso, unimodal/multimodal, etc.)

# Se puede concluir que el ingreso de los hombres en la RM es asimétrico, concentrado y unimodal.

