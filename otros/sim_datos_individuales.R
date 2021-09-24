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

load("datos/org/SERGASCOVID_05_11.RData")

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
# Ficheros necesarios:
  # - Fichero que indique individuos infectados y hospitalizados, por área sanitaria, con edad y sexo 
  #   Estructura: {area;estado;edad;sexo}. De este se sacan los siguientes datos:
        # Infectados en cada área sanitaria según rango de edades y sexo (CASES)
        # Hospitalizados en cada área sanitaria según rango de edades y sexo (HOSP)
  # - Número de camas en planta y UCI para cada área sanitaria (area;camas.planta;camas.uci)

# ---- Individuos en cada rango de edad ----
get.age.intervals <- function(age.vector, interval.size=INTERVAL.SIZE, range.min=RANGE.MIN, range.max=RANGE.MAX){
  breaks = seq(range.min, range.max, by=interval.size)
  age.range.cut = cut(age.vector, breaks, right=FALSE) 
  age.range.freq = table(age.range.cut) 
  return(age.range.freq)
}
INTERVAL.SIZE <- 10
RANGE.MIN <- 0
RANGE.MAX <- 110

# ---- Meter grupos de edad en el dataframe ----
datos.seleccionados <- datos.seleccionados %>% mutate(age.group = cut(age, breaks = seq(RANGE.MIN, RANGE.MAX, by=INTERVAL.SIZE), right = F))

### --- DATOS DE PRUEBA --- ###
datos.seleccionados$state <- rep(c('CASE','HOS'), len=nrow(datos.seleccionados))

# ---- CASES ----
women.age.interval <- get.age.intervals(subset(datos.seleccionados, sex=='Mujer')$age)
men.age.interval <- get.age.intervals(subset(datos.seleccionados, sex=='Hombre')$age)

total.m <- sum(men.age.interval)
total.w <- sum(women.age.interval)
total <- total.w + total.m

prob.w <- total.w/total # Parameter Bernoulli (0=man, 1=woman)
prob.m <- total.m/total

woman.age.prob <- women.age.interval/total.w 
man.age.prob <- men.age.interval/total.m 

plot(women.age.interval, col='red')
plot(men.age.interval, col='blue')

# ---- HOSP: ----
women.age.hosp.interval <-  get.age.intervals(subset(datos.seleccionados, sex=='Mujer' & state=='HOS')$age)
men.age.hosp.interval <- get.age.intervals(subset(datos.seleccionados, sex=='Hombre' & state=='HOS')$age)

total.w.hosp <- sum(women.age.hosp.interval)
total.m.hosp <- sum(men.age.hosp.interval)
total.hosp <- total.w.hosp + total.m.hosp

prob.w.hosp <- total.w.hosp/total.hosp
prob.m.hosp <- total.m.hosp/total.hosp
woman.age.prob.hosp <- women.age.hosp.interval/total.w.hosp
man.age.prob.hosp <- men.age.hosp.interval/total.m.hosp 

# Proporciones muestrales de hospitalizados:
prob.rc.woman <- women.age.hosp.interval/women.age.interval
prob.rc.man <- men.age.hosp.interval/men.age.interval

plot(women.age.hosp.interval, col='red')
plot(men.age.hosp.interval, col='blue')




##########################################
# ------ Capacidad asistencial --------- #
##########################################
# Datos de prueba
bed.capacity <- data.frame(ccca=c('Galicia'), hospital=c('CHUAC','HULA','CHUS','CHUVI'), tipo.hospital=c('publico'),
                           total.camas=c(100,50,70,60), ocupadas.covid19=c(30,10,15,12), ocupadas.no.covid19=c(10,10,5,3),
                           ingresos.24h.covid19=c(1,1,1,1), altas.24h.covid19=c(1,1,1,1), fecha.envio=c('2020-01-01'))




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
### ALGO MAL AQUI ###
prob.HW <- nrow(subset(datos.seleccionados, !is.na(date.admission.hosp_HOS) ))/sample.size


# ---- Options in HW ----
# Of those admitted in hospital ward, the probability of death without going to ICU is
prob.HW.death <- nrow(subset(datos.seleccionados, !is.na(date.admission.hosp_HOS) & is.na(date.admission.hosp_UCI) & death.while.inside==TRUE))/sample.size
# Probability that a patient admitted in hospital ward finally has to enter ICU
prob.HW.ICU <- nrow(subset(datos.seleccionados, !is.na(date.admission.hosp_HOS) & !is.na(date.admission.hosp_UCI)))/sample.size
# Probability that a patient admitted to hospital ward becomes discharged without entering ICU is
prob.HW.disc <- nrow(subset(datos.seleccionados, !is.na(date.admission.hosp_HOS) & is.na(date.admission.hosp_UCI) & death.while.inside==FALSE))/sample.size



# ---- Options in ICU ----

# Probability of dying after being admitted in ICU
prob.ICU.death <- nrow(subset(datos.seleccionados, !is.na(date.admission.hosp_UCI) & death.while.inside==TRUE))/sample.size

# Probability of being transferred to hospital ward after being admitted in ICU
  # para cada n.case unico que este repetido en datos.seleccionados
    # coger todas las filas de ese n.case
    # ver numero de filas
    # para cada fila "r" ver si la fecha de discharge de UCI menor o igual que la fecha de admission a HOS en la fila r+1 (excepto que ya se este en la ultima fila)
      # que esas fechas sean iguales indica que ha pasaodo de UCI a HOS en el mismo dia (sirve para calcular prob.ICU.HW)
      # llevar cuenta de cuántas veces pasa esto

get.calculo.ICU.HW <- function(df){
  # Obtener matriz con filas de n.case duplicados
  n.case.duplicados <- df[duplicated(df$n.case),]$n.case
  duplicados <- subset(df, n.case%in%n.case.duplicados)
  duplicados <- duplicados[order(duplicados$n.case, duplicados$date.admission.hosp_HOS),]
  
  # Para cada id único, mirar sus filas y ver si la fecha de discharge_UCI es menor o igual que la de admission.hosp en la siguiente estancia
  count.ICU.HW <- c()
  time.ICU.HW <- c()
  for (id in n.case.duplicados){
    sel <- subset(duplicados, n.case==id)
    rows <- nrow(sel)
    for (r in 1:rows){
      if (r!=rows){
        # Indicador de si la fecha de salidad de UCI es anterior a la siguiente entrada en HOSP
        condition <- ymd(sel[r,'date.discharge_UCI']) <= ymd(sel[r+1,'date.admission.hosp_HOS'])
        count.ICU.HW <- c(count.ICU.HW, condition)
        if (!is.na(condition) & condition==TRUE){
          # Tiempo en ICU antes de HW (necesario para Weibull)
          time.ICU.HW <- c(time.ICU.HW, ymd(sel[r+1,'date.admission.hosp_HOS'])-ymd(sel[r,'date.discharge_UCI'])+1)
        }
      }
    }
  }
  prob.ICU.HW <- sum(count.ICU.HW, na.rm=TRUE)/nrow(datos_sergas) ## QUIZAS MAL
  return (list(time.ICU.HW=time.ICU.HW, prob.ICU.HW=prob.ICU.HW))
}

tmp <- get.calculo.ICU.HW(datos.seleccionados)
prob.ICU.HW <- tmp$prob.ICU.HW

##########################################
# ------------- Weibull ---------------- #
##########################################
get.weibull.parameters <- function(vect, smoothness=1.5, extra.tittle=''){
  # Funcion de densidades
  density.function <- density(na.omit(vect), bw=smoothness)
  
  # Parámetros weibull
  weibull_parameters = eweibull(abs(density.function$x), method = "mle")$parameters
  shape <- weibull_parameters[1]
  scale <- weibull_parameters[2]
  
  # Mostrar si alinean bien
  plot(NA, xlim=c(0,max(density.function$x)), ylim=c(0,max(density.function$y)), xlab="", ylab="", main=glue('Scale: {round(scale,2)}, Shape: {round(shape,2)}, Bandwidth: {smoothness}\n{extra.tittle} '))
  curve(dweibull(x, shape=shape, scale=scale, log=FALSE), from=0, to=max(density.function$x), add=TRUE, col='blue')
  lines(density.function, col='red')
  legend("topright", legend = c("Weibull estimada", "Distribución real"), col = c('blue','red'),pch = c(1,1) )
  
  return(weibull_parameters)
}
get.conditional.weibull.parameters <- function(df, age.levels, s, target.column){
  result.list = list()
  par(mfrow=c(2,2))
  for (l in age.levels){
    target <- subset(df, age.group==l & sex==s)[[target.column]]
    if (length(target) != 0){
      p <- get.weibull.parameters(target, extra.tittle=glue('Sexo: {s}, Edad: {l}, Col: {target.column}'))
      result.list[[l]] <- p
    } else {
      result.list[[l]] <- NA
    }
  }  
  par(mfrow=c(1,1))
  return (result.list)
}
### IMPORTANTE ####
# FALTAN parametros en condicionales:
# scale.ICU.HW



# ---- Parámetros Weibull no condicionales ----

# Parámetros con tiempo que pasan en HOSP antes de ir a UCI
weibull.HW.ICU <- get.weibull.parameters(subset(datos.seleccionados, !is.na(time.HOSP) & !is.na(time.ICU))$time.HOSP)
shape.HW.ICU <- weibull.HW.ICU[1]
scale.HW.ICU <- weibull.HW.ICU[2]

# Parámetros con tiempo que pasan en UCI antes de morir
weibull.ICU.death <- get.weibull.parameters(subset(datos.seleccionados, death.while.inside==TRUE & !is.na(time.ICU))$time.ICU)
shape.ICU.death <- weibull.ICU.death[1]
scale.ICU.death <- weibull.ICU.death[2]

# Parámetros con tiempo que pasan en HW antes del discharge
weibull.HW.disc <- get.weibull.parameters(subset(datos.seleccionados, death.while.inside==FALSE & !is.na(time.HOSP))$time.HOSP)
shape.HW.disc <- weibull.HW.disc[1]
scale.HW.disc <- weibull.HW.disc[2]

# Parámetros con tiempo que pasan entre UCI y HOS
weibull.ICU.HW <- get.weibull.parameters(get.calculo.ICU.HW(datos.seleccionados)$time.ICU.HW)
shape.ICU.HW <- weibull.ICU.HW[1]
scale.ICU.HW <- weibull.ICU.HW[2]

# ---- Parámetros Weibull condicionales ----

# Parámetros con tiempo que pasan en HOSP antes de ir a UCI
tmp <- subset(datos.seleccionados, !is.na(time.HOSP) & !is.na(time.ICU))
weibull.HW.ICU.women <- get.conditional.weibull.parameters(tmp, levels(datos.seleccionados$age.group), 'Mujer', 'time.HOSP')
weibull.HW.ICU.men <- get.conditional.weibull.parameters(tmp, levels(datos.seleccionados$age.group), 'Hombre', 'time.HOSP')

# Parámetros con tiempo que pasan en UCI antes de morir
tmp <- subset(datos.seleccionados, death.while.inside==TRUE & !is.na(time.ICU))
weibull.ICU.death.women <- get.conditional.weibull.parameters(tmp, levels(datos.seleccionados$age.group), 'Mujer', 'time.ICU')
weibull.ICU.death.men <- get.conditional.weibull.parameters(tmp, levels(datos.seleccionados$age.group), 'Hombre', 'time.ICU')

# Parámetros con tiempo que pasan en HW antes del discharge
tmp <- subset(datos.seleccionados, death.while.inside==FALSE & !is.na(time.HOSP))
weibull.HW.disc.women <- get.conditional.weibull.parameters(tmp, levels(datos.seleccionados$age.group), 'Mujer', 'time.HOSP')
weibull.HW.disc.men <- get.conditional.weibull.parameters(tmp, levels(datos.seleccionados$age.group), 'Hombre', 'time.HOSP')

# Parámetros con tiempo que pasan entre UCI y HOS
### FALTA ###



weibull.unkwown.population <- function(vec, parameter, tittle=''){
  y <- c()
  for (n in names(vec)){
    if (!is.na(vec[[n]][parameter])){
      y <- c(y, vec[[n]][parameter])
    }
  }
  
  tmp <- data.frame(x=1:length(y),y=y)
  par(mfrow=c(1,2))
  plot(tmp, main=tittle)
  abline(lm(y ~ x, data = tmp))
  library(drc)
  fm <- drm(y ~ x, data = tmp, fct = G.3())
  plot(fm, main=tittle)
  par(mfrow=c(1,1))
}

p <- 2
weibull.unkwown.population(weibull.HW.ICU.women, p, 'scale.HW.ICU.women')
weibull.unkwown.population(weibull.ICU.death.women, p, 'scale.ICU.death.women')
weibull.unkwown.population(weibull.HW.disc.women, p, 'scale.HW.disc.women')

weibull.unkwown.population(weibull.HW.ICU.men, p, 'scale.HW.ICU.men')
weibull.unkwown.population(weibull.ICU.death.men, p, 'scale.ICU.death.men')
weibull.unkwown.population(weibull.HW.disc.men, p, 'scale.HW.disc.men')

p <- 1
weibull.unkwown.population(weibull.HW.ICU.women, p, 'shape.HW.ICU.women')
weibull.unkwown.population(weibull.ICU.death.women, p, 'shape.ICU.death.women')
weibull.unkwown.population(weibull.HW.disc.women, p, 'shape.HW.disc.women')

weibull.unkwown.population(weibull.HW.ICU.men, p, 'shape.HW.ICU.men')
weibull.unkwown.population(weibull.ICU.death.men, p, 'shape.ICU.death.men')
weibull.unkwown.population(weibull.HW.disc.men, p, 'shape.HW.disc.men')



