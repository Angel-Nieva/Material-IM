library(ggpubr)

# Generar valores para una distribucion normal con media 0 y desviacion estandar 1

media <- 0
desv_estandar <- 1
x <- seq(-15,35,0.01)
y <- dnorm(x, mean = media, sd = desv_estandar)
normal_1 <- data.frame(x, y)

# Generar valores para una distribucion normal con media 10 y desviacion estandar 6

media <- 10
desv_estandar <- 6
x <- seq(-15,35,0.01)
y <- dnorm(x, mean = media, sd = desv_estandar)
normal_2 <- data.frame(x, y)

# Graficar distribuciones
g <- ggplot(normal_1, aes(x,y)) + geom_line(color = "blue")
g <- g + geom_line(data = normal_2, color = "red")
g <- g + theme_pubr()

print(g)