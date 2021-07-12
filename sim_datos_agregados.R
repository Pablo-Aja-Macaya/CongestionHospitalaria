# Pasos:
  # - Procesar datos y calcular proporción por edades y sexos
  # - Calcular probabilidades
  # - Weibull (no se puede con datos reales entiendo)
  # - Simulacion

library(Rlab)
library(data.table)
library(scales)
library(foreach)
library(doParallel)

##########################################
# ---- Proporción de edades y sexos ---- #
##########################################

# Variables a elegir
area.sanitaria <- 'all' # si se pone 'all' se eligen todas

filter.cases <- function(df,area){
  # Filtra el dataframe de casos según área sanitaria
  # Si se pone 'all' se cogen todas las áreas
  if (area=='all'){
    casos <- df
  } else {
    casos <- subset(df, area_sanitaria==area)
  }
  return(casos)
}

get.group.total <- function(df, s){
  # Coge un dataframe y el sexo objetivo y saca el total de individuos para cada
  # rango de edad
  grupos <- unique(subset(df, sexo==s)$grupo_edad)
  grupos <- grupos[order(grupos)] # ordenar
  l <- list()
  for (g in grupos){
    tot <- sum(subset(df, sexo==s & grupo_edad==g)$cantidad)
    l[[g]] <- tot
  }
  return(unlist(l))
}

# Casos base
casos.org <- data.frame(fread('datos/solicitud_BD_1.csv'))

# Casos elegidos
casos <- filter.cases(casos.org, area.sanitaria)

# Hospitalizados en estos casos
hospitalizados <- subset(casos, estado=='HOS')

# ---- Proporciones de casos ---- #
women.age.interval <- get.group.total(casos, 'M')
men.age.interval <- get.group.total(casos, 'H')

# Calculo de la media de cada intervalo de edad (se usa en la simulación) Ej: Transforma "[20,30)" a 25
# PELIGRO: si en los datos de input no se representan todos los grupos en ambos sexos 
# la simulación podría estar mal equilibrada (ej: si sólo hay hombres de +70 y mujeres de -50 
# la simulación no creará individuos hombres de 50 años)
get.mean.age <- function(interval.vector){
  tmp <- gsub("\\[|)", "", names(interval.vector))
  rango.edad <- strsplit(tmp, ',')
  age.interval <- unlist(lapply(strsplit(tmp, ','), function(x){mean(as.numeric(x))}))
  return(age.interval)
}
women.mean.age.interval <- get.mean.age(women.age.interval)
men.mean.age.interval <- get.mean.age(men.age.interval)


# Proporciones
total.m <- sum(men.age.interval)
total.w <- sum(women.age.interval)
total <- total.w + total.m

prob.w <- total.w/total # Parameter Bernoulli (0=man, 1=woman)
prob.m <- total.m/total

woman.age.prob <- women.age.interval/total.w 
man.age.prob <- men.age.interval/total.m 

plot(as.factor(names(women.age.interval)), women.age.interval, main='women.age.interval')
plot(as.factor(names(men.age.interval)), men.age.interval, main='men.age.interval')

# ---- Proporciones de hospitalizados ---- #
women.age.hosp.interval <-  get.group.total(hospitalizados, 'M')
men.age.hosp.interval <- get.group.total(hospitalizados, 'H')

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

plot(as.factor(names(women.age.hosp.interval)), women.age.hosp.interval, main='women.age.hosp.interval')
plot(as.factor(names(men.age.hosp.interval)), men.age.hosp.interval, main='men.age.hosp.interval')

# Proporciones muestrales de hospitalizados:
prob.rc.real <- ( total.hosp ) / ( total )

##########################################
# ------ Capacidad asistencial --------- #
##########################################
capacidad <- data.frame(fread("datos/solicitud_BD.2.csv"))
names(capacidad) <- tolower(names(capacidad))

get.capacity <- function(df, hosp, unidad){
  if (hosp=='all'){
    selected <- subset(df, unidad==unidad)    
  } else {
    selected <- subset(df, hospital==hosp & unidad==unidad)    
  }
  stats <- list()
  stats$average_total <- mean(selected$total_camas)
  stats$average_ocupadas_covid <- mean(selected$ocupadas_covid19)
  stats$average_ocupadas_no_covid <- mean(selected$ocupadas_no_covid19)
  stats$average_ingresos_24_covid <- mean(selected$ingresos_24h_covid19)
  stats$average_altas_24_covid <- mean(selected$altas_24h_covid19)
  
  return(stats)
}

capacidad.asistencial <- get.capacity(capacidad, area.sanitaria, 'UCI')



##########################################
# ---- Probabilidades en simulación ---- #
##########################################

# ---- When a patient is admitted into the hospital ---- #
# Probability of going directly to ICU
prob.ICU <- sum(subset(hospitalizados, ingreso_hospitalario==0 & ingreso_uci==1)$cantidad, na.rm=T) / sum(hospitalizados$cantidad, na.rm=T)
# Probability of staying in hospital ward first
prob.HW <- sum(subset(hospitalizados, ingreso_hospitalario==1)$cantidad, na.rm=T) / sum(hospitalizados$cantidad, na.rm=T)


# ---- Options in HW ---- #
# Of those admitted in hospital ward, the probability of death without going to ICU is
prob.HW.death <- mean(subset(hospitalizados, ingreso_hospitalario==1 & ingreso_uci==0)$proporcion_muertos, na.rm=T)
# Probability that a patient admitted in hospital ward finally has to enter ICU
prob.HW.ICU <- sum(subset(hospitalizados, ingreso_hospitalario==1 & ingreso_uci==1)$cantidad, na.rm=T) / sum(hospitalizados$cantidad, na.rm=T)
# Probability that a patient admitted to hospital ward becomes discharged without entering ICU is
prob.HW.disc <- sum(subset(hospitalizados, ingreso_hospitalario==1 & ingreso_uci==0)$cantidad) / sum(hospitalizados$cantidad, na.rm=T)


# ---- Options in ICU ---- #

# Probability of dying after being admitted in ICU
prob.ICU.death <- mean(subset(hospitalizados, ingreso_uci==1)$proporcion_muertos)

# Probability of being transferred to hospital ward after being admitted in ICU
### PROBLEMA: no se puede a no ser que se agrupe por un atributo extra, 
# que sería "dirección" (si la fecha de discharge de UCI es menor que la de admission 
# en HOSP entonces direccion=UCI.to.HOS)
prob.ICU.HW <- 0.78
  

##########################################
# ------------- Weibull ---------------- #
##########################################

# Queda decidir de dónde se sacan los datos para esto 
# (en sim_datos_individuales.R hay una implementación para datos antiguos individualizados)
# Por ahora se usa la fórmula calculada a mano

##############################################
# -- Variables comunes entre simulaciones -- #
##############################################
num.cores <- 6
registerDoParallel(num.cores) 

m <- 1000 # Number of samples m=1000
n.ind <- 1000 # infected individuals N=1000
n.time <- 250 # Number of days (follow-up time)

par.m.size <- m/10
par.m.loops <- par.m.size/10
##############################################
# -- Simulación paralelizada condicional  -- #
##############################################
# Cada bucle paralelo crea una lista de resultados (n.HOS, n.ICU...)
# Al final se tiene una lista de longitud=par.m.loops, cada una con una lista de resultados
set.seed(123)
res <- foreach (par.m=1:par.m.loops) %dopar% {
  age <- gender <- inf.time <- prob.rc <- final.state <- matrix(rep(NA, length.out=par.m.size*n.ind), nrow = par.m.size, ncol = n.ind) 
  state <- rep(NA, par.m.size*n.ind*n.time)
  dim(state) <- c(par.m.size, n.ind, n.time)

  n.HOS <- n.ICU <- n.Dead <- n.Discharge <- n.H.Dead <- n.ICU.Dead  <- matrix(rep(NA, length.out= par.m.size*n.time), nrow = par.m.size, ncol = n.time) 
  for (j in 1:par.m.size) {
    # cat("j=",j,"\n")
    # if(j%%10==0) cat("j=",j,"\n")
    
    # Definition of age, gender and infection times
    # of patients in sample j
    
    gender[j,] <- rbern(n.ind, prob=prob.w)    # Gender from real data distribution
    
    # Age and hospital admission real data distribution
    # Con la distribución de edades reales se selecciona una etapa (ej: 54.5) y su 
    # probabilidad de hospitalización (ej: 0.28) para cada individuo i en una simulación j
    for (i in 1:n.ind){
      if (gender[j,i] == 1) {
        n.interval <- sample(1:length(woman.age.prob),size=1,prob=woman.age.prob)
        age[j,i] <- women.mean.age.interval[n.interval]
        prob.rc[j,i] <- prob.rc.woman[n.interval]
      } else {
        n.interval <- sample(1:length(man.age.prob),size=1,prob=man.age.prob)
        age[j,i] <- men.mean.age.interval[n.interval]
        prob.rc[j,i] <- prob.rc.man[n.interval]
      }
    }  
    
    # WARNING: No sé qué es esto
    inf.time[j,] <- rnorm(n=n.ind, mean=60, sd=10)
    
    #----------------------------------------------------
    # We define the times with infection as the state "I"
    #----------------------------------------------------
    for (i in 1:n.ind){
      # Ceiling redondea hacia arriba (1.1->2)
      # WARNING: No entiendo
      state[j, i, 1:(ceiling(inf.time[j,i])-1)] = 0
      state[j, i, ceiling(inf.time[j,i])] = "I"
    }
    
    #----------------------------------------------------
    # We simulate the times of the patients in hospital (i in in.H)
    #----------------------------------------------------
    u <- runif(n.ind) # esto genera random deviates of the uniform distribution 
    ind.H <- which(u<=prob.rc[j,]) # qué individuos tienen la valor alatorio u <= probabilidad de ser hospitalizados
    
    # Para cada individuo seleccionado (que tiene la enfermedad)
    for (i in ind.H){
      # Time since infection until hospital admission
      t.inf.until.hosp <- rnorm(n=1, mean=12-0.05*age[j,i], sd=1)
      state[j,i,(ceiling(inf.time[j,i])+1):ceiling((inf.time[j,i] + t.inf.until.hosp-1))] = "I"
      
      # The parameters of the Weibulls are computed only once
      scale.ICU.death <- 15.5 * ( (100 - abs(age[j,i]- 60) - 10*gender[j,i]) / 62)    # New parameters
      scale.ICU.HW <- 16.3 * ( (100 - abs(age[j,i]- 60) - 10*gender[j,i])/62)       # New parameters
      scale.HW.disc <- 8.4 * ((60 + age[j,i]-10*gender[j,i])/100)     # New parameters
      scale.HW.ICU <- 4.2
      
      #--------------------------------
      # The first states are simulated
      #--------------------------------
      v1 <- runif(1) # probabilidad aleatoria de que empiece en HW (v1<=prob.HW) o en ICU (v1>=prob.HW)
      #---------------------------------------------
      if (v1 <= prob.HW) {# Patient in hospital ward
        v2 <- runif(1)
        if (v2 <= prob.HW.death) {# Patient dies in HW
          time.HW.death <- rexp(1, 0.1)# Of those admitted in hospital ward, the time to death 
          state[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp) : ceiling(inf.time[j,i] + t.inf.until.hosp + time.HW.death)-1] = "H"
          state[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp + time.HW.death)] = "H.Dead"
          final.state[j,i] = "Dead"
        } else if ( (v2 > prob.HW.death) && (v2 <= prob.HW.death+prob.HW.ICU) ) {# Patient goes to ICU
          time.HW.ICU <- rweibull(1, shape=1.6, scale=scale.HW.ICU)# Time since hospital ward admission to ICU
          state[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp) : ceiling(inf.time[j,i] + t.inf.until.hosp + time.HW.ICU)] = "H"
          final.state[j,i] = "ICU"
        } else {# Patient discharged
          time.HW.disc <- rweibull(1, shape=2.6, scale=scale.HW.disc )# Time since hospital ward admission to discharge is
          state[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp) : ceiling(inf.time[j,i] + t.inf.until.hosp + time.HW.disc)-1] = "H"
          state[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp + time.HW.disc)] = "H.Discharge"
          final.state[j,i] = "Discharge"}
      } else {
        #---------------------------------------------
        # Patient in ICU
        v3 <- runif(1)
        if (v3 <= prob.ICU.death) {# Patient dies in ICU
          time.ICU.death <- rweibull(1, shape=1.4, scale=scale.ICU.death)# Time from admission in ICU to death
          state[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp) : ceiling(inf.time[j,i] + t.inf.until.hosp + time.ICU.death)-1] = "ICU"
          state[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp + time.ICU.death)] = "ICU.Dead"
          final.state[j,i] = "Dead"
        } else {# Patient goes to hospital ward
          time.ICU.HW <- rweibull(1, shape=1.8, scale=scale.ICU.HW ) # Time since admission in ICU till return to ward
          state[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp) : ceiling(inf.time[j,i] + t.inf.until.hosp + time.ICU.HW)] = "ICU"
          final.state[j,i] = "HOS"
        }}
      
      #--------------------------------
      # The following states are simulated
      # for those in HOS or ICU
      #--------------------------------
      while((final.state[j,i]=="HOS") | (final.state[j,i]=="ICU")){
        # La matriz state[j,i,] muestra la evolución del paciente desde "0" (sano), a I, H, ICU, H.Discharge etc.
        # También pueden tener NAs (time without state) en el que todavía no sabemos qué le pasó,
        # Sabemos que está en HOS o ICU pero ahora se simula lo que le pasa
        i.final <- min(which(is.na(state[j,i,]))) # Lowest time without state
        
        v1 <- runif(1)
        #---------------------------------------------
        # Ahora se simula
        if(final.state[j,i]=="HOS"){# Patient in hospital ward
          v2 <- runif(1)
          if (v2 <= prob.HW.death) {# Patient dies in HW
            time.HW.death <- rexp(1, 0.1)# Of those admitted in hospital ward, the time to death 
            state[j, i, i.final: ceiling(i.final+time.HW.death)-1] = "H"
            state[j, i, ceiling(i.final+time.HW.death)] = "H.Dead"
            final.state[j,i] = "Dead"
          } else if ( (v2 > prob.HW.death) && (v2 <= prob.HW.death+prob.HW.ICU) ) {# Patient goes to ICU
            time.HW.ICU <- rweibull(1, shape=1.6, scale=scale.HW.ICU)# Time since hospital ward admission to ICU
            state[j,i,i.final: ceiling(i.final+time.HW.ICU)] = "H"
            final.state[j,i] = "ICU"
          } else {# Patient discharged
            time.HW.disc <- rweibull(1, shape=2.6, scale=scale.HW.disc )# Time since hospital ward admission to discharge is
            state[j,i,i.final: ceiling(i.final+time.HW.disc)-1] = "H"
            state[j,i,ceiling(i.final+time.HW.disc)] = "H.Discharge"
            final.state[j,i] = "Discharge"}
        } else {
          #---------------------------------------------
          # Patient in ICU
          v3 <- runif(1)
          if (v3 <= prob.ICU.death) {# Patient dies in ICU
            time.ICU.death <- rweibull(1, shape=1.4, scale=scale.ICU.death)# Time from admission in ICU to death
            state[j,i,i.final : ceiling(i.final+time.ICU.death)-1] = "ICU"
            state[j,i,ceiling(i.final+time.ICU.death)] = "ICU.Dead"
            final.state[j,i] = "Dead"
          } else {# Patient goes to hospital ward
            time.ICU.HW <- rweibull(1, shape=1.8, scale=scale.ICU.HW ) # Time since admission in ICU till return to ward
            state[j,i,i.final : ceiling(i.final+time.ICU.HW) ] = "ICU"
            final.state[j,i] = "HOS"
          }}
      } # end while
    } # end i in n.ind
    
    #=================================================================
    # Number of patients in each state (HOS, ICU, Dead, Discharge)
    #=================================================================
    
    for (k in 1:n.time){
      n.HOS[j,k] <- length(which(state[j, ,k]=="H"))
      n.ICU[j,k] <- length(which(state[j, ,k]=="ICU"))
      n.H.Dead[j,k] <- length(which(state[j, ,k]=="H.Dead"))
      n.Discharge[j,k] <- length(which(state[j, ,k]=="H.Discharge"))
      n.ICU.Dead[j,k] <- length(which(state[j, ,k]=="ICU.Dead"))
      n.Dead[j,k] <- n.H.Dead[j,k] + n.ICU.Dead[j,k]
    }
  } # end j in m
  # Esta línea saca fuera del bucle paralelo la información
  list(n.HOS=n.HOS,n.ICU=n.ICU,n.H.Dead=n.H.Dead,n.Discharge=n.Discharge,n.ICU.Dead=n.ICU.Dead,n.Dead=n.Dead)
}
stopImplicitCluster()

# Ahora se juntan estos resultados
get.sim.results <- function(res, name){
  return(do.call(rbind, lapply(res, function(x){x[[name]]})))
}

n.HOS <- get.sim.results(res, 'n.HOS')
n.ICU <- get.sim.results(res, 'n.ICU')
n.H.Dead <- get.sim.results(res, 'n.H.Dead')
n.Discharge <- get.sim.results(res, 'n.Discharge')
n.ICU.Dead <- get.sim.results(res, 'n.ICU.Dead')
n.Dead <- get.sim.results(res, 'n.Dead')

###################################################
# ----- Gráficas de resultados condicionales ---- #
###################################################
nHOS <- nICU <- nDead <- nDischarge <- nH.Dead <- nICU.Dead  <- rep(0, length.out= n.time) 
for (k in 1:n.time){
  nHOS[k] <- sum(n.HOS[,k], na.rm=T)/m
  nICU[k] <- sum(n.ICU[,k], na.rm=T)/m
  nDead[k] <- sum(n.Dead[,k], na.rm=T)/m
  nDischarge[k] <- sum(n.Discharge[,k], na.rm=T)/m
  nH.Dead[k] <- sum(n.H.Dead[,k], na.rm=T)/m
  nICU.Dead[k] <- sum(n.ICU.Dead[,k], na.rm=T)/m
}

t <- seq(1, n.time)
par(mfrow=c(2,2))
plot(t, nHOS, type="l",lty=1, lwd=2, col=1)
plot(t, nICU, type="l",lty=1, lwd=2, col=1)
plot(t, nDead, type="l",lty=1, lwd=2, col=1)
plot(t, nH.Dead, type="l",lty=1, lwd=2, col=1)

par(mfrow=c(1,2))
plot(t, nDischarge, type="l", lty=1, lwd=2, col=1)
barplot(nDischarge)
par(mfrow=c(1,1))

# Ver si en algún momento se superó el número de camas
cap <- capacidad.asistencial$average_total
plot(NA, xlim=c(0,n.time), ylim=c(0,cap+100), xlab="", ylab="")
abline(h=cap, col='red')
for(j in 1:m){
  # Para cada simulación ver en cuántos días hay más gente en UCI y Hospital que camas
  lines(n.ICU[j,]+n.HOS[j,], col=alpha('black', 0.2))
}
lines(nHOS, type="l",lty=1, lwd=2, col='green')
lines(nICU, type="l",lty=1, lwd=2, col='red')
lines(nHOS+nICU, type="l",lty=1, lwd=2, col='blue')

# Número de días que se sobrepasa
sum(nHOS+nICU>=cap)


#################################################
# -- Simulación paralelizada no condicional --- #
#################################################
# The parameters of the Weibulls 
scale.ICU.death <- 15.5 
scale.ICU.HW <- 16.3 
scale.HW.disc <- 8.4
scale.HW.ICU <- 4.2

# Cada bucle paralelo crea una lista de resultados (n.HOS, n.ICU...)
# Al final se tiene una lista de longitud=par.m.loops, cada una con una lista de resultados
set.seed(123)
res <- foreach (par.m=1:par.m.loops) %dopar% {
  age.inc <- gender.inc <- inf.time <- prob.rc <- final.state.inc <- matrix(rep(NA, length.out=par.m.size*n.ind), nrow = par.m.size, ncol = n.ind) 
  n.HOS.inc <- n.ICU.inc <- n.Dead.inc <- n.Discharge.inc <- n.H.Dead.inc <- n.ICU.inc.Dead.inc  <- matrix(rep(NA, length.out= par.m.size*n.time), nrow = par.m.size, ncol = n.time)
  state.inc <- rep(NA, par.m.size*n.ind*n.time)
  dim(state.inc) <- c(par.m.size, n.ind, n.time)
  
  n.HOS.inc <- n.ICU.inc <- n.Dead.inc <- n.Discharge.inc <- n.H.Dead.inc <- n.ICU.inc.Dead.inc  <- matrix(rep(NA, length.out= par.m.size*n.time), nrow = par.m.size, ncol = n.time) 
  for (j in 1:par.m.size){
    # cat("j=",j,"\n")
    if(j%%10==0) cat("j=",j,"\n")
    
    # Definition of age, gender and infection times
    # of patients in sample j
    
    gender.inc[j,] <- rbern(n.ind, prob=prob.w)    # Gender from real data distribution
    
    # Age and hospital admission real data distribution
    for (i in 1:n.ind){
      
      if (gender.inc[j,i] == 1) {
        n.interval <- sample(1:length(woman.age.prob),size=1,prob=woman.age.prob)
        age.inc[j,i] <- women.mean.age.interval[n.interval]
      } else {
        n.interval <- sample(1:length(man.age.prob),size=1,prob=man.age.prob)
        age.inc[j,i] <- men.mean.age.interval[n.interval]
      }
      
    }  
    prob.rc[j,] <- prob.rc.real
    inf.time[j,] <-rnorm(n=n.ind, mean=60, sd=10)
    # prob.rc[j,] <- ( (0.9-0.1*gender.inc[j,]) * exp(0.1*(age.inc[j,]-40)-gender.inc[j,]) ) / ( 1 + exp(0.1 * (age.inc[j,]-40) -gender.inc[j,]) )
    # prob.rc[j,] <-  0.85* exp(-0.5)  / ( 1 + exp(-0.5) )
    
    #----------------------------------------------------
    # We define the times with infection as the state.inc "I"
    #----------------------------------------------------
    for (i in 1:n.ind){
      state.inc[j, i, 1:(ceiling(inf.time[j,i])-1)] = 0
      state.inc[j, i, ceiling(inf.time[j,i])] = "I"
    }
    #----------------------------------------------------
    # We simulate the times of the patients in hospital (i in in.H)
    #----------------------------------------------------
    
    u <- runif(n.ind)
    ind.H <- which(u<=prob.rc[j,])
    
    for (i in ind.H){
      # Time since infection until hospital admission
      t.inf.until.hosp <- rnorm(n=1, mean=12-0.05*age.inc[j,i], sd=1)
      state.inc[j,i,(ceiling(inf.time[j,i])+1):ceiling((inf.time[j,i] + t.inf.until.hosp-1))] = "I"
      
      #--------------------------------
      # The first state.incs are simulated
      #--------------------------------
      
      v1 <- runif(1)
      #---------------------------------------------
      if (v1 <= prob.HW) {# Patient in hospital ward
        v2 <- runif(1)
        if (v2 <= prob.HW.death) {# Patient dies in HW
          time.HW.death <- rexp(1, 0.1)# Of those admitted in hospital ward, the time to death 
          state.inc[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp) : ceiling(inf.time[j,i] + t.inf.until.hosp + time.HW.death)-1] = "H"
          state.inc[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp + time.HW.death)] = "H.Dead"
          final.state.inc[j,i] = "Dead"
        } else if ( (v2 > prob.HW.death) && (v2 <= prob.HW.death+prob.HW.ICU) ) {# Patient goes to ICU
          time.HW.ICU <- rweibull(1, shape=1.6, scale=scale.HW.ICU)# Time since hospital ward admission to ICU
          state.inc[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp) : ceiling(inf.time[j,i] + t.inf.until.hosp + time.HW.ICU)] = "H"
          final.state.inc[j,i] = "ICU"
        } else {# Patient discharged
          time.HW.disc <- rweibull(1, shape=2.6, scale=scale.HW.disc )# Time since hospital ward admission to discharge is
          state.inc[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp) : ceiling(inf.time[j,i] + t.inf.until.hosp + time.HW.disc)-1] = "H"
          state.inc[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp + time.HW.disc)] = "H.Discharge"
          final.state.inc[j,i] = "Discharge"}
      } else {
        #---------------------------------------------
        # Patient in ICU
        v3 <- runif(1)
        if (v3 <= prob.ICU.death) {# Patient dies in ICU
          time.ICU.death <- rweibull(1, shape=1.4, scale=scale.ICU.death)# Time from admission in ICU to death
          state.inc[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp) : ceiling(inf.time[j,i] + t.inf.until.hosp + time.ICU.death)-1] = "ICU"
          state.inc[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp + time.ICU.death)] = "ICU.Dead"
          final.state.inc[j,i] = "Dead"
        } else {# Patient goes to hospital ward
          time.ICU.HW <- rweibull(1, shape=1.8, scale=scale.ICU.HW ) # Time since admission in ICU till return to ward
          state.inc[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp) : ceiling(inf.time[j,i] + t.inf.until.hosp + time.ICU.HW)] = "ICU"
          final.state.inc[j,i] = "HOS"
        }}
      
      #--------------------------------
      # The following state.incs are simulated
      # for those in HOS or ICU
      #--------------------------------
      while((final.state.inc[j,i]=="HOS") | (final.state.inc[j,i]=="ICU")){
        
        i.final <- min(which(is.na(state.inc[j,i,]))) # Lowest time without state.inc
        
        v1 <- runif(1)
        #---------------------------------------------
        if(final.state.inc[j,i]=="HOS"){# Patient in hospital ward
          v2 <- runif(1)
          if (v2 <= prob.HW.death) {# Patient dies in HW
            time.HW.death <- rexp(1, 0.1)# Of those admitted in hospital ward, the time to death 
            state.inc[j, i, i.final: ceiling(i.final+time.HW.death)-1] = "H"
            state.inc[j, i, ceiling(i.final+time.HW.death)] = "H.Dead"
            final.state.inc[j,i] = "Dead"
          } else if ( (v2 > prob.HW.death) && (v2 <= prob.HW.death+prob.HW.ICU) ) {# Patient goes to ICU
            time.HW.ICU <- rweibull(1, shape=1.6, scale=scale.HW.ICU)# Time since hospital ward admission to ICU
            state.inc[j,i,i.final: ceiling(i.final+time.HW.ICU)] = "H"
            final.state.inc[j,i] = "ICU"
          } else {# Patient discharged
            time.HW.disc <- rweibull(1, shape=2.6, scale=scale.HW.disc)# Time since hospital ward admission to discharge is
            state.inc[j,i,i.final: ceiling(i.final+time.HW.disc)-1] = "H"
            state.inc[j,i,ceiling(i.final+time.HW.disc)] = "H.Discharge"
            final.state.inc[j,i] = "Discharge"}
        } else {
          #---------------------------------------------
          # Patient in ICU
          v3 <- runif(1)
          if (v3 <= prob.ICU.death) {# Patient dies in ICU
            time.ICU.death <- rweibull(1, shape=1.4, scale=scale.ICU.death)# Time from admission in ICU to death
            state.inc[j,i,i.final : ceiling(i.final+time.ICU.death)-1] = "ICU"
            state.inc[j,i,ceiling(i.final+time.ICU.death)] = "ICU.Dead"
            final.state.inc[j,i] = "Dead"
          } else {# Patient goes to hospital ward
            time.ICU.HW <- rweibull(1, shape=1.8, scale=scale.ICU.HW ) # Time since admission in ICU till return to ward
            state.inc[j,i,i.final : ceiling(i.final+time.ICU.HW)] = "ICU"
            final.state.inc[j,i] = "HOS"
          }}
        
      } # end while
    } # end i in n.ind
    
    #=================================================================
    # Number of patients in each state.inc (HOS, ICU, Dead, Discharge)
    #=================================================================
    
    for (k in 1:n.time){
      n.HOS.inc[j,k] <- length(which(state.inc[j, ,k]=="H"))
      n.ICU.inc[j,k] <- length(which(state.inc[j, ,k]=="ICU"))
      n.H.Dead.inc[j,k] <- length(which(state.inc[j, ,k]=="H.Dead"))
      n.Discharge.inc[j,k] <- length(which(state.inc[j, ,k]=="H.Discharge"))
      n.ICU.inc.Dead.inc[j,k] <- length(which(state.inc[j, ,k]=="ICU.Dead"))
      n.Dead.inc[j,k] <- n.H.Dead.inc[j,k] + n.ICU.inc.Dead.inc[j,k]
    }
  } # end j in m
  # Esta línea saca fuera del bucle paralelo la información
  list(n.HOS.inc=n.HOS.inc,n.ICU.inc=n.ICU.inc,n.H.Dead.inc=n.H.Dead.inc,n.Discharge.inc=n.Discharge.inc,n.ICU.inc.Dead.inc=n.ICU.inc.Dead.inc,n.Dead.inc=n.Dead.inc)
}
stopImplicitCluster()

# Ahora se juntan estos resultados
get.sim.results <- function(res, name){
  return(do.call(rbind, lapply(res, function(x){x[[name]]})))
}

n.HOS.inc <- get.sim.results(res, 'n.HOS.inc')
n.ICU.inc <- get.sim.results(res, 'n.ICU.inc')
n.Dead.inc <- get.sim.results(res, 'n.Dead.inc')
n.Discharge.inc <- get.sim.results(res, 'n.Discharge.inc')
n.H.Dead.inc <- get.sim.results(res, 'n.H.Dead.inc')
n.ICU.inc.Dead.inc <- get.sim.results(res, 'n.ICU.inc.Dead.inc')


#################################################
# -- Gráficas de resultados no condicionales -- #
#################################################

nHOS.inc <- nICU.inc <- nDead.inc <- nDischarge.inc <- nH.Dead.inc <- nICU.inc.Dead  <- rep(0, length.out= n.time) 
for (k in 1:n.time){
  nHOS.inc[k] <- sum(n.HOS.inc[,k], na.rm=T)/m
  nICU.inc[k] <- sum(n.ICU.inc[,k], na.rm=T)/m
  nDead.inc[k] <- sum(n.Dead.inc[,k], na.rm=T)/m
  nDischarge.inc[k] <- sum(n.Discharge.inc[,k], na.rm=T)/m
  nH.Dead.inc[k] <- sum(n.H.Dead.inc[,k], na.rm=T)/m
  nICU.inc.Dead[k] <-sum(n.ICU.inc.Dead.inc[,k], na.rm=T)/m
}

t <- seq(1, n.time)
par(mfrow=c(2,2))
plot(t, nHOS.inc, type="l",lty=1, lwd=2, col=1)
plot(t, nICU.inc, type="l",lty=1, lwd=2, col=1)
plot(t, nDead.inc, type="l",lty=1, lwd=2, col=1)
plot(t, nH.Dead.inc, type="l",lty=1, lwd=2, col=1)

par(mfrow=c(1,2))
plot(t, nDischarge.inc, type="l", lty=1, lwd=2, col=1)
barplot(nDischarge.inc)
par(mfrow=c(1,1))

# Ver si en algún momento se superó el número de camas
cap <- capacidad.asistencial$average_total
plot(NA, xlim=c(0,n.time), ylim=c(0,cap+100), xlab="", ylab="")
abline(h=cap, col='red')
for(j in 1:m){
  # Para cada simulación ver en cuántos días hay más gente en UCI y Hospital que camas
  lines(n.ICU.inc[j,]+n.HOS.inc[j,], col=alpha('black', 0.2))
}
lines(nHOS.inc, type="l",lty=1, lwd=2, col='green')
lines(nICU.inc, type="l",lty=1, lwd=2, col='red')
lines(nHOS.inc+nICU.inc, type="l",lty=1, lwd=2, col='blue')

# Número de días que se sobrepasa
sum(nHOS.inc+nICU.inc>=cap)




