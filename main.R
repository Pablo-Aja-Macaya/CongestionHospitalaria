# ---- Congestión hospitalaria ----

# Se quiere simular en qué punto se llenan las camas
# Tenemos:
  # fecha de entrada y salida de cada paciente
  # número de camas de cada hospital
  # sexo
  # edad

# Para cada simulación j, simular individuos i en base a distribuciones reales:
  # Sexo
  # Edad

# Calcular transiciones entre etapas mediante estimadores de Weibull
  # Estimadores de Weibull: hacer función de densidades de cuánto tiempo pasa entre evento1 y evento2. Después calcular shape y scale con eweibull

# Simular aleatoriamente (?):
  # Etapas y probabilidad de transición a cada una (HW, UCI)
  # Cuándo pasa a una etapa y cuándo sale

# ---- Librerías y variables base ----

library('EnvStats') # weibull_parameters
library('Rlab') # Bernoulli function
library(glue)
library(dplyr)
library(lubridate)
options(lubridate.fasttime = TRUE)


##########################################
# -------- Formateo de datos ----------- #
##########################################

load("datos/SERGASCOVID_05_11.RData")

datos_sergas <- dat
common <- colnames(datos_sergas)[1:8]

# Se añaden columnas que no están en los datos para hacer el paso siguiente más sencillo
datos_sergas$date.admission.hosp_UCI_3 <- NA
datos_sergas$date.discharge_UCI_3 <- NA
datos_sergas$time.ICU.3 <- NA

# Se seleccionan los atributos del paciente y una etapa (ej: atributos normales y datos de su primera estancia)
# Esto crea varias filas de un paciente, en cada fila refiriéndose a una estancia
l <- list()
for (i in 1:3){
  print(i)
  df <- data.frame(datos_sergas[,c(common, glue("date.admission.hosp_HOS_{i}"), glue("date.discharge_HOS_{i}"), glue("date.admission.hosp_UCI_{i}"), glue("date.discharge_UCI_{i}"), "time.HOSP.prev.ICU", "prev.hosp")])
  colnames(df) <- c(common, "date.admission.hosp_HOS", "date.discharge_HOS", "date.admission.hosp_UCI", "date.discharge_UCI", "time.HOSP.prev.ICU", "prev.hosp")
  l[[i]] <- df
}

# Se juntan los dataframes de la lista
full.data <- bind_rows(l)

# Se seleccionan filas que tengan , por lo menos, una fecha de entrada en hospital o una de entrada en uci
datos.seleccionados <- full.data[complete.cases(full.data[ , c("date.admission.hosp_HOS")]) | complete.cases(full.data[ , c("date.admission.hosp_UCI")]),]

# ---- Calcular ---- 
  # tiempo de estancia (total.time)
  # tiempo uci (time.ICU)
  # tiempo hosp (time.HOSP)
  # death y discharge (con fecha de muerte)
  # date.discharge_HOS (última fecha en pacientes que han sobrevivido en el período especificado)
  # prev.hosp (si ha estado en hospital antes de UCI?)
  # time.HOSP.prev.ICU (tiempo que ha estado en el hospital antes de UCI)


# Tiempo en UCI
datos.seleccionados$time.ICU <- apply(datos.seleccionados[,c("date.admission.hosp_UCI", "date.discharge_UCI")],1, 
                    function(l){max(ymd(l), na.rm = TRUE) - min(ymd(l), na.rm = TRUE) + 1})

# Tiempo en HOSP
datos.seleccionados$time.HOSP <- apply(datos.seleccionados[,c("date.admission.hosp_HOS", "date.discharge_HOS")],1, 
                    function(l){max(ymd(l), na.rm = TRUE) - min(ymd(l), na.rm = TRUE) + 1}) 

# En algunas operaciones de fechas puede resultar en infinitos, se limpian y pasan a NAs (antes de hacer operaciones con ellos)
is.na(datos.seleccionados) <- do.call(cbind,lapply(datos.seleccionados, is.infinite))

# Tiempo total
datos.seleccionados$total.time <- apply(datos.seleccionados[,c("time.HOSP", "time.ICU")],1,
                                  function(l){sum(l[c('time.HOSP','time.ICU')], na.rm=TRUE)})

# Si ha muerto dentro del hospital en las fechas establecidas
datos.seleccionados$death.while.inside <- apply(datos.seleccionados[,c('date.death',"date.admission.hosp_HOS", "date.discharge_HOS", "date.admission.hosp_UCI", "date.discharge_UCI")],1, 
                     function(l){
                       max.date = max(ymd(l[c("date.admission.hosp_HOS", "date.discharge_HOS", "date.admission.hosp_UCI", "date.discharge_UCI"), drop=FALSE]), na.rm = TRUE) 
                       min.date = min(ymd(l[c("date.admission.hosp_HOS", "date.discharge_HOS", "date.admission.hosp_UCI", "date.discharge_UCI"), drop=FALSE]), na.rm = TRUE)
                       death <-  ymd(l[c("date.death"), drop=FALSE])
                       # Condición: fecha de muerte existe, y esta está entre la máxima y la mínima
                       !is.na(death) & death <= max.date & death >= min.date
                       })

# Si ha estado en HOSP antes de UCI
datos.seleccionados$prev.hosp <- apply(datos.seleccionados[,c("date.admission.hosp_HOS", "date.admission.hosp_UCI")],1,
                               function(l){
                                 admission.hosp <-  l['date.admission.hosp_HOS']
                                 admission.uci <-  l['date.admission.hosp_UCI']
                                 # Si ha estado en hosp y uci devolver TRUE
                                 if (!is.na(admission.hosp) & !is.na(admission.uci)){
                                   return(TRUE)
                                 } else {
                                   return(FALSE)
                                 }
                               })

# Tiempo en HOSP nates de UCI
datos.seleccionados$time.HOSP.prev.ICU <- apply(datos.seleccionados[,c('prev.hosp',"date.admission.hosp_HOS", "date.discharge_HOS")],1,
                                       function(l){
                                         admission.hosp <-  l['date.admission.hosp_HOS']
                                         discharge.hosp <-  l['date.discharge_HOS']
                                         if (l['prev.hosp']){
                                           return(as.duration(ymd(discharge.hosp)-ymd(admission.hosp)) / ddays(1))
                                         } else {
                                           return(NA)
                                         }
                                       })
  
  


##########################################
# ---- Proporción de edades y sexos ---- #
##########################################

# ---- Individuos en cada rango de edad ----
get.age.intervals <- function(age.vector, interval.size=5){
  age.range <- range(age.vector, na.rm=T)
  breaks = seq(age.range[1], age.range[2], by=interval.size)
  age.range.cut = cut(age.vector, breaks, right=FALSE) 
  age.range.freq = table(age.range.cut) 
  return(age.range.freq)
}
interval.size <- 10

# Ficheros necesarios:
  # - Fichero que indique individuos infectados y hospitalizados, por área sanitaria, con edad y sexo 
  #   Estructura: {area;estado;edad;sexo}. De este se sacan los siguientes datos:
        # Infectados en cada área sanitaria según rango de edades y sexo (CASES)
        # Hospitalizados en cada área sanitaria según rango de edades y sexo (HOSP)
  # - Número de camas en planta y UCI para cada área sanitaria (area;camas.planta;camas.uci)

### DATOS DE PRUEBA ###
complete.cases.df <- data.frame(area=rep(c('CHUAC','CHUS','HULA', 'CHUVI'), len=nrow(datos.seleccionados)),
                                estado = rep(c('INF','HOSP'), len=nrow(datos.seleccionados)),
                                age = datos.seleccionados$age, sex = datos.seleccionados$sex)




# ---- CASES ----
women.age.interval <- get.age.intervals(subset(datos.seleccionados, sex=='Mujer')$age, interval.size)
men.age.interval <- get.age.intervals(subset(datos.seleccionados, sex=='Hombre')$age, interval.size)

total.m <- sum(men.age.interval)
total.w <- sum(women.age.interval)
total <- total.w + total.m

prob.w <- total.w/total # Parameter Bernoulli (0=man, 1=woman)
prob.m <- total.m/total

woman.age.prob <- women.age.interval/total.w 
man.age.prob <- men.age.interval/total.m 

plot(women.age.interval, col='red')
plot(men.age.interval, col='blue')

#### IMPORTANTE: COmpletar ####
# ---- HOSP: ----
# woman.age.hosp <- c(2321, 6248, 6347, 5235, 4306, 2666, 1336, 560, 94, 78)
# total.w.hosp <- sum(woman.age.hosp)
# man.age.hosp <- c(1650, 7142, 9637, 8118, 6345, 3831, 1478, 524, 90, 111)
# total.m.hosp <- sum(man.age.hosp)
# total.hosp <- total.w.hosp + total.m.hosp
# 
# prob.w.hosp <- total.w.hosp/total.hosp
# prob.m.hosp <- total.m.hosp/total.hosp
# woman.age.prob.hosp <- woman.age.hosp/total.w.hosp # sum(woman.age.prob.hosp)=1
# man.age.prob.hosp <- man.age.hosp/total.m.hosp     # sum(man.age.prob.hosp)=1
# 
# # Proporciones muestrales de hospitalizados:
# prob.rc.woman <- woman.age.hosp/woman.age
# prob.rc.man <- man.age.hosp/man.age



##########################################
# ------ Variables de simulación ------- #
##########################################
m <- 1000 # Number of samples m=1000
n.ind <- 1000 # infected individuals N=1000
n.time <- 250 # Number of days (follow-up time)

age <- gender <- inf.time <- prob.rc <- final.state <- matrix(rep(NA, length.out=m*n.ind), nrow = m, ncol = n.ind) 
state <- rep(NA, m*n.ind*n.time)
dim(state) <- c(m, n.ind, n.time)

sample.size <- nrow(datos.seleccionados)

# ---- When a patient is admitted into the hospital ----
# Probability of going directly to ICU
prob.ICU <- nrow(subset(datos.seleccionados, is.na(date.admission.hosp_HOS) & !is.na(date.admission.hosp_UCI)))/sample.size
# Probability of staying in hospital ward first
prob.HW <- nrow(subset(datos.seleccionados, !is.na(date.admission.hosp_HOS)))/sample.size

# ---- Options in HW ----
# Of those admitted in hospital ward, the probability of death without going to ICU is
prob.HW.death <- nrow(subset(datos.seleccionados, !is.na(date.admission.hosp_HOS) & !is.na(date.admission.hosp_UCI) & death.while.inside==TRUE))/sample.size
# Probability that a patient admitted in hospital ward finally has to enter ICU
prob.HW.ICU <- nrow(subset(datos.seleccionados, !is.na(date.admission.hosp_HOS) & !is.na(date.admission.hosp_UCI)))/sample.size
# Probability that a patient admitted to hospital ward becomes discharged without entering ICU is
prob.HW.disc <- nrow(subset(datos.seleccionados, !is.na(date.admission.hosp_HOS) & is.na(date.admission.hosp_UCI) & death.while.inside==FALSE))/sample.size

# ---- Options in ICU ----
# Probability of dying after being admitted in ICU
prob.ICU.death <- nrow(subset(datos.seleccionados, !is.na(date.admission.hosp_UCI) & death.while.inside==TRUE))/sample.size
# Probability of being transferred to hospital ward after being admitted in ICU
prob.ICU.HW <- 0.78


##########################################
# ------------- Weibull ---------------- #
##########################################
get.weibull.parameters <- function(vector, smoothness=10){
  # Funcion de densidades
  density.function <- density(vector, bw=smoothness)
  
  # Parámetros weibull
  weibull_parameters = eweibull(abs(density.function$x), method = "mle")$parameters
  shape <- weibull_parameters[1]
  scale <- weibull_parameters[2]
  
  # Mostrar si alinean bien
  plot(NA, xlim=c(0,max(density.function$x)), ylim=c(0,max(density.function$y)), xlab="", ylab="", main=glue('Scale: {round(scale,2)}, Shape: {round(shape,2)}, Bandwidth: {smoothness} '))
  curve(dweibull(x, shape=shape, scale=scale, log=FALSE), from=0, to=max(density.function$x), add=TRUE, col='blue')
  lines(density.function, col='red')
  
  return(weibull_parameters)
}

## IMPORTANTE: SI SE QUIERE INCLUIR EL SEXO IGUAL CALCULAR LOS PARAMETROS EN BASE A ESO ##

# Parámetros con tiempo que pasan en HOSP antes de ir a UCI
weibull.HW.ICU <- get.weibull.parameters(subset(datos.seleccionados, !is.na(time.HOSP) & !is.na(time.ICU))$time.HOSP, 10)
shape.HW.ICU <- weibull.HW.ICU[1]
scale.HW.ICU <- weibull.HW.ICU[2]

# Parámetros con tiempo que pasan en UCI antes de morir
weibull.ICU.death <- get.weibull.parameters(subset(datos.seleccionados, death.while.inside==TRUE & !is.na(time.ICU))$time.ICU)
shape.ICU.death <- weibull.ICU.death[1]
scale.ICU.death <- weibull.ICU.death[2]

# Parámetros con tiempo que pasan en HW antes del discharge
weibull.HW.discharge<- get.weibull.parameters(subset(datos.seleccionados, death.while.inside==FALSE & !is.na(time.HOSP))$time.HOSP)
shape.HW.discharge <- weibull.HW.discharge[1]
scale.HW.discharge <- weibull.HW.discharge[2]

### IMPORTANTE ####
# Mas parametros:
# scale.HW.disc
# scale.ICU.HW


