library ( ggpubr )

dir <- "C:/Users/Dell PC/Desktop/IME-2022/Actividades/act3"

basename <- "EP02 Datos Casen 2017.csv"
file <- file.path(dir, basename)
población <- read.csv2(file = file, fileEncoding = "UTF-8")
tamaño <- nrow(población)
ingreso <- as.numeric(población[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )

# Definir semilla 
set.seed(7)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

# Calculando la Distribución Z
z <- (ingreso.normal - media.ingreso) / sd.ingreso


# Funcion para calcular el chi cuadrado con n grados de libertad
obt_valor_chi <- function(i, Z, n){
  sum(sample(Z, n)^2)
}

# Calculando chi cuadrado con 5 grados de libertad:

chi_5 <- sapply(1:length(z), obt_valor_chi, z, 5)



# Calculando chi cuadrado con 13 grados de libertad:

chi_13 <- sapply(1:length(z), obt_valor_chi, z, 13)


# Calculando la Distribución F:

f5 <- chi_5 / 5
f13 <- chi_13 / 13

f <- f5 / f13

# Graficando las distribuciónes

# Histograma normal
h1 <- data.frame(ingreso.normal)
g <- ggplot(h1, aes(x = ingreso.normal)) + 
  geom_histogram(color = "black", fill = "white", bins=30) +
  theme_pubr() +
  ggtitle("Distribución normal")
print(g)

# Histograma Chi-Cuadrado 5 
h2 <- data.frame(chi_5)
g2 <- ggplot(h2, aes(x = chi_5)) + 
  geom_histogram(color = "black", fill = "white", bins=30) +
  theme_pubr() +
  ggtitle("Distribución Chi-cuadrado 5")
print(g2)

# Histograma Chi-Cuadrado 13
h3 <- data.frame(chi_13)
g3 <- ggplot(h3, aes(x = chi_13)) + 
  geom_histogram(color = "black", fill = "white", bins=30) +
  theme_pubr() +
  ggtitle("Distribución Chi-cuadrado 13")
print(g3)

# Histograma Chi-Cuadrado 13
h4 <- data.frame(chi_13)
g4 <- ggplot(h4, aes(x = chi_13)) + 
  geom_histogram(color = "black", fill = "white", bins=30) +
  theme_pubr() +
  ggtitle("Distribución Chi-cuadrado 13")
print(g4)

# Histograma Distribución F
h5 <- data.frame(f)
g5 <- ggplot(h5, aes(x = f)) + 
  geom_histogram(color = "black", fill = "white", bins=30) +
  theme_pubr() +
  ggtitle("Distribución F")
print(g5)

#--------------------------------------------------------------#

set.seed(7)
n.repeticiones <- 30

ensayo <- function(x)
  ifelse(sample(población[["sexo"]], 1) == "Mujer", 1, 0)

treinta.repeticiones <- sapply(1:n.repeticiones, ensayo)

# Funcion que obtiene k intentos de un ensayo de Bernoulli y obtiene
# la cantidad de exitos obtenidos (sexo = mujer).
exitos <- function(i, k) 
  sum(sample(treinta.repeticiones, k, replace = TRUE))

# Distribucion binominal para 15 intentos.
binomial <- sapply(1:2500, exitos, k = 15)

# Histograma Distribución binomial
h6 <- data.frame(binomial)
g6 <- ggplot(h6, aes(x = binomial)) + 
  geom_histogram(color = "black", fill = "white", bins=15) +
  theme_pubr() +
  ggtitle("Distribución binomial")
print(g6)

