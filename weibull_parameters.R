# ---- Parámetros Weibull ----


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



##########################################
# ------------- Weibull ---------------- #
##########################################
get.calculo.ICU.HW <- function(df){
  # Obtener matriz con filas de n.case duplicados (varias estancias)
  n.case.duplicados <- df[duplicated(df$n.case),]$n.case
  duplicados <- subset(df, n.case%in%n.case.duplicados)
  duplicados <- duplicados[order(duplicados$n.case, duplicados$date.admission.hosp_HOS),]
  
  # Para cada id único, mirar sus filas y ver si la fecha de discharge_UCI es menor o igual que la de admission.hosp en la siguiente estancia
  time.ICU.HW <- c()
  for (id in n.case.duplicados){
    sel <- subset(duplicados, n.case==id)
    rows <- nrow(sel)
    for (r in 1:rows){
      if (r!=rows){
        # Indicador de si la fecha de salidad de UCI es anterior a la siguiente entrada en HOSP
        ultima.UCI <- ymd(sel[r,'date.discharge_UCI'])
        siguiente.HOS <- ymd(sel[r+1,'date.admission.hosp_HOS'])
        condition <-  ultima.UCI <= siguiente.HOS
        if (!is.na(condition) & condition==TRUE){
          # Tiempo en ICU antes de HW (necesario para Weibull)
          # (fecha de admisión en hosp de la siguiente estancia menos la fecha de discharge de UCI de la estancia actual)
          time.ICU.HW <- c(time.ICU.HW, siguiente.HOS - ultima.UCI+1)
        }
      }
    }
  }
  return (list(time.ICU.HW=time.ICU.HW))
}

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
      result.list[[l]] <- c(NA,NA)
    }
  }  
  par(mfrow=c(1,1))
  return (result.list)
}

get.conditional.ICU.HW <- function(df, age.levels, s){
  result.list = list()
  par(mfrow=c(2,2))
  for (l in age.levels){
    target <- subset(df, age.group==l & sex==s)
    if (length(target) != 0){
      p <- get.calculo.ICU.HW(target)$time.ICU.HW
      if (is.vector(p)){
        p <- get.weibull.parameters(p)
        result.list[[l]] <- p
      } else {
        result.list[[l]] <- c(NA,NA)
      }
      
    } else {
      result.list[[l]] <- c(NA,NA)
    }
  }  
  par(mfrow=c(1,1))
  return (result.list)  
}

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


data.frame(HW.ICU=c(shape.HW.ICU,scale.HW.ICU), ICU.death=c(shape.ICU.death,scale.ICU.death),
           HW.disc=c(shape.HW.disc,scale.HW.disc), ICU.HW=c(shape.ICU.HW,scale.ICU.HW))

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
tmp <- subset(datos.seleccionados) # REVISAR
weibull.ICU.HW.women <- get.conditional.ICU.HW(tmp, levels(datos.seleccionados$age.group), 'Mujer')
weibull.ICU.HW.men <- get.conditional.ICU.HW(tmp, levels(datos.seleccionados$age.group), 'Hombre')


#################################################################################

get.mean.age <- function(interval.vector){
  tmp <- gsub("\\[|)", "", interval.vector)
  rango.edad <- strsplit(tmp, ',')
  age.interval <- unlist(lapply(rango.edad, function(x){mean(as.numeric(x), na.rm=T)}))
  age.interval <- age.interval[!is.na(age.interval)]
  return(age.interval)
}

get.full.weibull.pam <- function(res.list, sex){
  # Esta función transforma los resultados de Weibull en un dataframe y rellena los nulos
  # (grupos de edad-sexo que no aparecen en los datos) con el mínimo o máximo de la variable
  
  fill.min.max <- function(df, c){
    # Sustituir los nulos por el mínimo o máximo según si sus posiciones estén 
    # debajo del mínimo o encima del máximo en el vector
    # c = parámetro de weibull objetivo
    
    # Posiciones
    min.index <- which.min(df[,c]) # índices del valor mínimo
    max.index <- which.max(df[,c]) # índice del valor máximo
    na.index <- which(is.na(df[,c])) # índices de los nulos
    
    # Sustitución
    df[,c][na.index[na.index<min.index]] <- df[,c][min.index] # nulo antes del mínimo
    df[,c][na.index[na.index>max.index]] <- df[,c][max.index] # nulo después del mínimo
    df[,c][na.index[na.index>min.index & na.index<max.index]] <- mean(df[,c][c(min.index, max.index)]) # nulo entre mínimo y máximo
    
    return(df)
  }
  
  # Transformar datos a :
  #       shape     scale sexo     grupo edad
  # 1        NA        NA    0    [0,10)    5
  # 2  1.271158  6.812643    0   [10,20)   15
  res <- as.data.frame(res.list)
  names(res) <- names(res.list)
  res <- as.data.frame(t(rbind(res, sexo=NA)))
  res$grupo <- rownames(res)
  rownames(res) <- NULL
  res$edad <- get.mean.age(res$grupo)
  
  # Rellenar nulos con máximos o mínimos según la posición de los nulos (edad del grupo)
  df <- fill.min.max(res, 'shape')
  df <- fill.min.max(df, 'scale')
  
  # Rellenar el parámetro de sexo
  df$sexo <- sex
  
  return(df)
}

weibull.HW.ICU.women <- get.full.weibull.pam(weibull.HW.ICU.women, 'M')
weibull.HW.ICU.men <- get.full.weibull.pam(weibull.HW.ICU.men, 'H')

weibull.ICU.death.women <- get.full.weibull.pam(weibull.ICU.death.women, 'M')
weibull.ICU.death.men <- get.full.weibull.pam(weibull.ICU.death.men, 'H')

weibull.HW.disc.women <- get.full.weibull.pam(weibull.HW.disc.women, 'M')
weibull.HW.disc.men <- get.full.weibull.pam(weibull.HW.disc.men, 'H')

weibull.ICU.HW.women <- get.full.weibull.pam(weibull.ICU.HW.women, 'M')
weibull.ICU.HW.men <- get.full.weibull.pam(weibull.ICU.HW.men, 'H')


par(mfrow=c(1,2))
tmp1 <- weibull.ICU.HW.women
tmp2 <- weibull.ICU.HW.men
plot(tmp1$edad, tmp1$scale, col='red')
points(tmp2$edad, tmp2$scale, col='blue')
plot(tmp1$edad, tmp1$shape, col='red')
points(tmp2$edad, tmp2$shape, col='blue')

tmp <- rbind(weibull.HW.disc.men, weibull.HW.disc.women)
model <- lm(scale ~ sexo + edad, tmp)
predict(model, newdata=data.frame(sexo='H', edad=60))

