
#=============================================================================================================
# NOTA:
# Se escriben las palabras sin tildes ni enie para evitar problemas al momento de 
# ejecutar el script.
#=============================================================================================================

################
## PREGUNTA 1 ##
################

# Un experimento psicologico fue realizado para determinar el efecto que puede tener la ansiedad en el
# deseo de una persona de estar sola o acompanada. Se dividio aleatoriamente un grupo de 30
# voluntarias/os en dos grupos de 17 y 13 personas respectivamente. Al primer grupo se le dijo que serian
# sometidos a descargas electricas bastante dolorosas, con el proposito de causar ansiedad. Al segundo
# grupo tambien se le dijo que recibirian descargas electricas, pero que estas serian suaves e indoloras. A
# las personas de ambos grupos se les instruyo esperar 10 minutos antes de comenzar el experimento con
# las descargas y se les dio la opcion de esperar esos 10 minutos en soledad o en compania de otras
# personas. 5 personas quisieron esperar solas en el primer grupo, mientras que 9 optaron por esta
# alternativa en el segundo grupo. ¿Tiene relacion la ansiedad con el deseo de estar sola/o o
# acompanada/o?


# Debido a las caracteristicas de la pregunta se procedera a usar una prueba exacta de Fisher, pues la cantidad
# de muestras es pequena y se estudian dos variables dicotomicas.

# Debido a que es un experimento y a como se señala el enunciado se dividio un grupo de 30 personas
# aleatoriamente, lo cual nos indica que las dos muestras fueron elegidas de forma aleatoria y por 
# lo tanto son independientes entre si y se puede proceder con la prueba.

# Ahora se definiran las hipotesis a contrastar en este caso tanto en lenguaje natural como matematico:

# Natural:

# H0: La ansiedad provocada por el tipo de descarga no tiene relacion con el tipo de compania del individua/o 
#     (estar sola/o o acompanada/o)
# HA: La ansiedad provocada por el tipo de descarga tiene relacion con el tipo de compania del individua/o (estar
#     sola/o o acompanada/o)

#Matematicamente: 

# Donde </=> da a conocer que no existe relacion entre las variables; <=> da a conocer que existe relacion entre 
# las variables

# H0: tipo </=> compania
# HA: tipo <=> compania


#Creacion de tabla.

tipo<- c(rep("descarga_dolorosa",17), rep("descarga_suave",13))
compania <- c(rep("sola",14),rep("acompanada",16))

datos <- data.frame(compania,tipo)
tabla <- xtabs(~.,datos)
print(tabla)

# Nivel de significacion
alfa <- 0.05

# Se aplica la prueba de Fisher 
prueba <- fisher.test(tabla, 1-alfa)
print(prueba)

## RESPUESTA 1 ##

# Dado que el valor p=4.793e-06 es menor al nivel de significancia definido, se rechaza la hipotesis nula
# a favor de la alternativa. Por lo tanto se puede concluir que, con 95% de confianza, la ansiedad provocada
# por el tipo de descarga electrica no tiene relacion con el tipo de compania que desee la persona.
#=============================================================================================================

################
## PREGUNTA 2 ##
################

# Se esta llevando a cabo un estudio para determinar el impacto de un medicamento que ayuda a dejar de
# fumar. Para esto se ha reclutado a 100 pares de gemelas monocigotos (todos mujeres) que viven juntas.
# Seleccionando aleatoriamente, a una de los hermanas se le da el tratamiento, mientras que a la otra se le
# da placebo. Luego de dos semanas tomando el medicamento, se les pide dejar de fumar por tres meses.
# Los pares de hermanas se encuestaron al finalizar este periodo y se le consulto por el exito del tratamiento
# a cada una de las gemelas. La muestra registro 65 pares de gemelas en que ambas pudieron dejar de
# fumar y 40 pares en que ambas no pudieron dejar de fumar. En 27 pares solo la gemela que recibio el
# tratamiento pudo dejar el habito, mientras que en 13 pares pudo dejar de fumar la gemela que recibio
# placebo. ¿Es exitoso el medicamento que se esta probando?


# Para llegar a una solucion se procede a formular la hipotesis nula y alternativa para dicho problema natural 
# y matematicamente

# Natural:

# H0: No hay un cambio significativo del efecto entre el medicamento y el placebo sobre las gemelas monocigotos
# HA: Si hay un cambio significativo del efecto entre el medicamento y el placebo sobre las gemelas monocigotos

#Matematicamente: 

# H0: medicamento == placebo
# HA: medicamento <> placebo

# Para dar respuesta al problema se aplicara la prueba de McNemar, ya que se busca comprobar si es que existe un
# cambio significativo entre las gemelas que recibieron el medicamento y las gemelas que recibieron el placebo
# (dos grupos). Ademas la variable en estudio es dicotomica (el dejar o no de fumar)

# Debido a que es un experimento y a como se senala el enunciado se selecciono aleatoriamente a la hermana que 
# recibiria el medicamento y la hermana que recibiria el placebo. Entonces es razonable indicar que ambas 
# muestras son independientes, y se puede continuar con la aplicacion de la prueba.

# Se construye la tabla de contingencia
pares <- seq(1:145)
medicamento <- c(rep("No_Fuma",65),rep("Fuma",40),rep("No_Fuma",27),rep("Fuma",13))
placebo <- c(rep("No_Fuma",65),rep("Fuma",40),rep("Fuma",27),rep("No_Fuma",13))
tabla <- table(medicamento, placebo)
print(tabla)

# Nivel de significacion
alfa <- 0.05

#Se aplica la prueba
prueba <- mcnemar.test(tabla)
print(prueba)

## RESPUESTA 2 ##

# Dado el valor p= 0.03893 con 1 grado de libertad es mayor al nivel de significacion establecido (0.05) se 
# rechaza la hipotesis nula en favor de la alternativa. Se puede concluir con el 95% de confianza que existe
# un cambio significativo del efecto entre el medicamento y el placebo, por lo que se puede decir que el 
# medicamento es exitoso.
#=============================================================================================================

################
## PREGUNTA 3 ##
################

# Un memorista, que esta trabajando con el grupo de investigacion de Aplicaciones para la Web de nuestro
# Departamento, ha disenado dos algoritmos de búsqueda que intentan considerar el estado de ánimo del
# usuario en los parametros de la busqueda. Por supuesto, necesita evaluar estas propuestas y para eso ha
# realizado un experimento computacional que mide las veces que el usuario necesita hacer solo una
# busqueda para encontrar informacion relevante. La siguiente tabla muestra los resultados de estos
# experimentos, que tambien considera el algoritmo que actualmente utiliza uno de los motores de busqueda
# mas usados. ¿Existe alguna diferencia entre el rendimiento de los algoritmos probados?

# Para llegar a una solucion se procede a formular la hipotesis nula y alternativa para dicho problema natural 
# y matematicamente

# Natural:

# H0: No existe diferencia en el rendimiento de los algoritmos probados (actual, Nuevo 1, Nuevo 2)
# HA: Si existe diferencia en el rendimiento de los algoritmos probados (actual, Nuevo 1, Nuevo 2)

#Matematicamente: 

# H0: rendimiento_1 - rendimiento_2 = 0  ^ 
#     rendimiento_actual - rendimiento_1 = 0 ^ 
#     rendimiento_actual - rendimiento_2 = 0

# HA: rendimiento_1 - rendimiento_2 <> 0  v 
#     rendimiento_actual - rendimiento_1 <> 0 v 
#     rendimiento_actual - rendimiento_2 <> 0

# La prueba elegida para solucionar el problema es la prueba Chi-cuadrado de homogeneidad, ya que principalmente
# se esta trabajando con dos poblaciones, en este caso la necesidad de realizar una busqueda, y la necesidad de
# realizar dos o mas busquedas para encontrar informacion relevante. Por otro lado se busca comprobar que los 
# algoritmos presentan el mismo rendimiento en los diferentes niveles de la variable categorica (Actual, Nuevo_1
# Nuevo_2), lo cual apoya el uso de esta prueba.

# Las condiciones que se deben verificar para hacer uso de la prueba son: 

# 1) Las observaciones deben ser independientes entre si 
# 2) Debe haber a lo menos 5 observaciones esperadas en cada grupo

# Verificacion 1):

# Puesto que se trata de un experimento que es parte de una investigacion se asume que las observaciones fueron
# escogidas de forma aleatoria, y por lo tanto son independientes entre si.


# Verificacion 2):

# Para verificar esta condicion se procede a crear la tabla de contingencia que contenga las frecuencias esperadas
# con el objetivo de verificar que cada una sea mayor a 5. Los calculos se realizan manualmente y se guardan en 
# la siguiente tabla:

# Para obtener los valores de las siguientes listas se hace uso de la formula Eij = (ni * nj) / n
# Donde:
# ni: total de observaciones en la fila i
# nj: total de observaciones en la columna j
# n: tamano de la muestra

uno <- c(3529, 1764.5, 1764.5)
dos_o_mas <- c(1471, 735.5, 735.5)

tabla_esp <- as.table(rbind(uno, dos_o_mas))

dimnames(tabla_esp) <- list(veces = c("Una busqueda", "Dos o mas busquedas"),
                            algoritmo = c("Actual", "Nuevo 1", "Nuevo 2" ))
print(tabla_esp)

# De la tabla se puede visualizar que los valores esperados para cada celda son mayor o igual a 5 

# Es asi como ambas condiciones se verifican y se puede proceder a aplicar la prueba

# Se crea tabla de contingencia

uno <- c(3511, 1749, 1798)
dos_o_mas <- c(1480, 751, 702)

tabla <- as.table(rbind(uno, dos_o_mas))

dimnames(tabla) <- list(veces = c("Una busqueda", "Dos o mas busquedas"),
                            algoritmo = c("Actual", "Nuevo 1", "Nuevo 2" ))
print(tabla)


# Nivel de significacion
alfa <- 0.05

# Prueba chi cuadrado de homogeneidad

prueba <- chisq.test(tabla)

print(prueba)

## RESPUESTA 3 ##

# Luego de realizar la prueba se obtiene el valor de p = 0.2542 con 2 grados de libertad, y este efectivamente
# es mayor al nivel de significancia (0.05). Esto lleva como consecuencia a no rechazar la hipotesis nula, con
# lo cual se puede concluir que con un nivel del 95% de confianza que no existe diferencia en el rendimiento de 
# los algoritmos probados.
#=============================================================================================================


# PREGUNTA 4

#Enuncie un ejemplo novedoso (no discutido en clase ni en las lecturas) relacionado con las preferencias
#de companias de telefonía movil que dan servicio en el pais despues de un ano y medio de teletrabajo, y
#que requiera utilizar una prueba Q de Cochran. Identifique las variables involucradas y las hipotesis a
#contrastar.


# Enunciado ejemplo novedoso:

# Un empleado del SERNAC esta llevando a cabo un estudio acerca de las interrupciones
# de servicios de internet de las tres companias mas grandes de chile. El estudio se 
# basa principalmente en identificar aquellas companias que reciben 50 o mas reclamos
# al mes en un periodo de 1 anio y medio elegidos aleatoriamente segun la categoria 
# del tipo de reclamo. Lo que el empleado busca encontrar es:
# ¿Existe una diferencia significativa en el rendimiento de cada compania?

# La tabla que resume la informacion se construye a continuacion:

meses <- 1:18

clavistel <-c(0,1,1,0,1,0,0,1,1,1,0,1,0,0,0,1,0,0)
nexus <- c(1,0,0,0,1,0,0,1,0,1,0,1,1,1,0,1,0,1)
btr <- c(0,1,1,1,1,1,0,1,0,0,1,0,0,0,0,1,1,1)

# se muestra la tabla con los valores correspondientes:
datos <- data.frame(meses, clavistel, nexus, btr)
datos


# Identificando las respectivas variables:

# variable de respuesta (dicotomica): 
# 1, si la compania presenta 50 o mas reclamos
# 0, si la compania presenta menos de 50 reclamos

# variable independiente (categorica):
# Corresponde a las tres companias mencionadas (clavistel, nexus y btr)


# Identificacion de las hipotesis a contrastar:

# Natural:

# H0: La proporcion de 50 o mas reclamos es igual para todas las companias.
# HA: La proporcion de 50 o mas reclamos es distinta para todas las companias.

#Matematicamente: 

# H0: proporcion_clavistel - proporcion_nexus = 0 ^
#     proporcion_clavistel - proporcion_btr = 0 ^
#     proporcion_nexus - proporcion_btr = 0


# HA: proporcion_clavistel - proporcion_nexus <> 0 v
#     proporcion_clavistel - proporcion_btr <> 0 v
#     proporcion_nexus - proporcion_btr <> 0
    

# Argunmento del porque se utilizaria la prueba Q de Cochran:

# Dado que la variable de respuesta es dicotomica, la variable independiente
# es categorica, las observaciones son independientes (se eligieron aleatoriamente) y
# el tamano de la muestra es suficientemente grande (18 meses*3 companias >= 24 ) es
# adecuado aplicar esta prueba.












