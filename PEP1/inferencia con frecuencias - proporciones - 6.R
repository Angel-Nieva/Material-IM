library("dplyr")

# ******************************************** ENUNCIADO ******************************************** 
# Los siguientes datos vienen de un estudio hecho por Rothman & Keller (1972) (Journal of chronic
# diseases, 25(12), 711-716) sobre la incidencia de la cantidad de alcohol y de tabaco que se consume en el
# riesgo de padecer cáncer oral. Las tablas muestran el número de personas que consumiendo una cierta
# cantidad de alcohol o de tabaco, mostrada en cada fila, desarrollaron o no desarrollaron (controles) la
# enfermedad durante su vida.
# ****************************************************************************************************

Tabaco = c("0","1-19","20-39","40 o más")
Cáncer_oral = c(26, 66, 248,143)
Controles = c(85,97,197,68)

tabla <- data.frame(Tabaco, Cáncer_oral, Controles)

# Dar formato categorico a la columna Tabaco.
tabla[["Tabaco"]] <- factor(tabla[["Tabaco"]])

##############################
#####     Pregunta 1     #####
##############################

# Estudios previos habían determinado que la incidencia de cáncer oral en la población general que no fuma
# era de 20%. ¿Respaldan estos datos tal estimación?

##############################
#####     Desarrollo     #####
##############################

# Fijar valores conocidos

# Poblacion de no fumadores
no_fumadores <- tabla %>% filter(Tabaco=="0") 
n1 <- no_fumadores[["Cáncer_oral"]] + no_fumadores[["Controles"]]

#existos de la muestra
exitos1 <- no_fumadores[["Cáncer_oral"]] # 26

#probabilidad de exito
p_exito1 <- exitos1/n1
alfa <- 0.01
valor_nulo1 <- 0.2

prueba1 <- prop.test(exitos1, n = n1, p = valor_nulo1, 
                     alternative = "two.sided", conf.level = 1 - alfa)

##############################
#####     Respuesta      #####
##############################
print(prueba1)
cat("1) Los datos no respaldan tal estimacion ya que con una confianza de 95% no se han encontrado equivalentes los valores de la muestra con los de la hipotesis nula")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
##############################
#####     Pregunta 2     #####
##############################

# Según estos datos, ¿da lo mismo no fumar que hacerlo diariamente consumiendo entre 1 y 19 cigarrillos?

##############################
#####     Desarrollo     #####
##############################

#Fijar valores conocidos

# Fumadores de 1 a 19 cigarrilos diarios
cigarrillos_1_19 <- tabla %>% filter(Tabaco=="1-19") 
n2 <- cigarrillos_1_19[["Cáncer_oral"]] + cigarrillos_1_19[["Controles"]]
n <- c(c(n1, n2))

#existos de la muestra
exitos2 <- cigarrillos_1_19[["Cáncer_oral"]]
valor_nulo <- 0.0
exitos <- c(exitos1, exitos2)

prueba2 <- prop.test(exitos, 
                     n = n,
                     alternative = "two.sided", 
                     conf.level = 1 - alfa)

##############################
#####     Respuesta      #####
##############################
print(prueba2)
cat("2) Por lo tanto, segun los datos de la tabla relacionados con el tabaco, no sa lo mismo no fumar a fumar entre 1 a 19 cigarrillos diarios \n")

#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------
##############################
#####     Pregunta 3     #####
##############################

# Suponiendo que la diferencia en la proporción de personas que desarrollan la enfermedad entre quienes
# no fuman y aquellos que fuman de 1 a 19 cigarrillos al día es de 0.2. ¿Cuánta gente deberíamos
# monitorear para obtener un intervalo de confianza del 95% y poder estadístico de 80%? si se intente
# mantener aproximadamente la misma proporción de gente estudiada en cada caso.
# Buena

##############################
#####     Desarrollo     #####
##############################

# Fijar valores conocidos

#probabilidad de exito
p_exito1 <- exitos1/n1
p_exito2 <- exitos2/n2

alfa <- 0.05
poder <- 0.8

n <- power.prop.test(n = NULL,
                     p1 = p_exito1,
                     p2 = p_exito2,
                     sig.level = alfa,
                     power = poder,
                     alternative = "two.sided")$n
print(n)

cat("2) Se necesita aproximadamente una muestra de", n,"\n")

