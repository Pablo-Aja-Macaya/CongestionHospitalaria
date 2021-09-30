###########################################################
# Script principal: Simulación de congestión hospitalaria #
###########################################################

# Este script es el código principal del repositorio, e incluye el procesamiento
# de datos de casos/hospitalizados, datos de capacidad (analisis_capacidad.R),
# cálculo de probabilidades de transición, cálculo de variables Weibull,
# las simulaciones paralelizadas y su visualizacióon

# A tener en cuenta: la variable num.cores especifica los hilos usados en la simulación
# Dependiendo del número de simulaciones un num.cores alto puede causar problemas de RAM
# Ejemplo: Un ordenador con 8 hilos y 16 GB de RAM usará, como máximo, alrededor de 8-9 GB de RAM si:
# - num.cores = 6
# - m = 1000
# - n.ind = 1000
# - n.time = 250

# Más información disponible en el GitHub del repositorio: https://github.com/Pablo-Aja-Macaya/CongestionHospitalaria

###########################################################

# Librerías
library(Rlab)
library(data.table)
library(scales)
library(foreach)
library(doParallel)
library(DT)
library(readr)
library(dplyr)
library(glue)
library(zoo)
library(plotly)

# ---- Variables  ---- 

# Variables de simulación
num.cores <- 6 # número de hilos usados en simulación
registerDoParallel(num.cores) 

m <- 500 # simulaciones
n.ind <- 1000 # individuos infectados
n.time <- 250 # días (follow-up time)

par.m.loops <- 10 # cuántas tandas
par.m.size <- m/par.m.loops # cuántas simulaciones por núcleo


#  Variables de datos
outlier.filter.type <- 'sliding_median' # tipo de filtro de outliers
window.size <- 5 # para el filtro de outliers si se elige desplazamiento de ventana
modo.weibull <- 'manual' # 'automatico', 'manual' (manual/formula)
inf.time.avg <- 100 # dia medio donde ocurre la infección
inf.time.sd <- 20 # desviación estándard del día donde ocurre la infección
area.sanitaria <- "Coruña - Cee" # si se pone 'all' se eligen todas
hosp.ref <- 'all' # qué hospitales se seleccionan (1: referencias, 0: no referencias, 'all': todos)
areas.hospitales <- data.frame(read_csv("datos/areas_hospitales_correspondencia.csv")) # correspondencia entre hospital y área
casos.org <- data.frame(read_csv("datos/sivies_agreg_area_sanitaria.csv")) # casos base

# Para crear buenas tablas
create.table <- function(df, capt){
  DT::datatable(df, extensions = c('FixedColumns'),
                options = list(scrollX = TRUE, paging=TRUE), 
                caption=htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: center; color: black;',
                  htmltools::em(capt)
                ))
}



# ---- Proporción de edades y sexos ---- 
# Datos completos
create.table(casos.org, 'Casos y hospitalizados')

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
  # Coge un dataframe y el sexo objetivo y saca el total de individuos para cada rango de edad
  grupos <- unique(subset(df, sexo==s & grupo_edad!='')$grupo_edad)
  grupos <- grupos[order(grupos)] # ordenar
  l <- list()
  for (g in grupos){
    tot <- sum(subset(df, sexo==s & grupo_edad==g)$cantidad)
    l[[g]] <- tot
  }
  return(unlist(l))
}

get.mean.age <- function(interval.vector){
  # Coge un vector como c([10,20), [20,30)) y lo transforma en c(15, 25)
  tmp <- gsub("\\[|)", "", names(interval.vector))
  rango.edad <- strsplit(tmp, ',')
  age.interval <- unlist(lapply(strsplit(tmp, '-'), function(x){mean(as.numeric(x), na.rm=T)}))
  age.interval <- age.interval[!is.na(age.interval)]
  return(age.interval)
}

# Quitar grupos de edad nulos
casos.org <- subset(casos.org, grupo_edad!='NULL')

# Casos elegidos
casos <- filter.cases(casos.org, area.sanitaria)
create.table(casos, 'Casos elegidos')

# Hospitalizados en estos casos
hospitalizados <- subset(casos, estado=='HOS')
create.table(hospitalizados, 'Hospitalizados')

# -- Proporciones de casos -- #
women.age.interval <- get.group.total(casos, 'M')
men.age.interval <- get.group.total(casos, 'H')

# Calculo de la media de cada intervalo de edad (se usa en la simulación) Ej: Transforma "[20,30)" a 25
# PELIGRO: si en los datos de input no se representan todos los grupos en ambos sexos 
# la simulación podría estar mal equilibrada (ej: si sólo hay hombres de +70 y mujeres de -50 
# la simulación no creará individuos hombres de 50 años)
women.mean.age.interval <- get.mean.age(women.age.interval)
men.mean.age.interval <- get.mean.age(men.age.interval)

# Individuos totales por sexo
total.m <- sum(men.age.interval)
total.w <- sum(women.age.interval)
total <- total.w + total.m

# Probabilidad de pertencer a cada sexo
prob.w <- total.w/total
prob.m <- total.m/total

# Probabilidad de pertenecer a cada rango de edad
woman.age.prob <- women.age.interval/total.w 
man.age.prob <- men.age.interval/total.m 

# -- Proporciones de hospitalizados -- #
# Hospitalizados por rango de edad y sexo
women.age.hosp.interval <-  get.group.total(hospitalizados, 'M')
men.age.hosp.interval <- get.group.total(hospitalizados, 'H')

# Total de hospitalizados por sexo
total.w.hosp <- sum(women.age.hosp.interval)
total.m.hosp <- sum(men.age.hosp.interval)
total.hosp <- total.w.hosp + total.m.hosp

# Probabilidad de pertenecer a cada sexo en hospitalizados
prob.w.hosp <- total.w.hosp/total.hosp
prob.m.hosp <- total.m.hosp/total.hosp

# Probabilidad de ser hospitalizado por sexo
woman.age.prob.hosp <- women.age.hosp.interval/total.w.hosp
man.age.prob.hosp <- men.age.hosp.interval/total.m.hosp 

# Proporciones muestrales de hospitalizados por sexo
prob.rc.woman <- women.age.hosp.interval/women.age.interval
prob.rc.man <- men.age.hosp.interval/men.age.interval

# Proporciones muestrales de hospitalizados:
prob.rc.real <- ( total.hosp ) / ( total )


# ------ Capacidad asistencial --------- #
source('analisis_capacidad.R')


# ---- Probabilidades en simulacion ---- 

# -- Paciente es admitido en hospital -- #
# Probabilidad de ir directamente a UCI
prob.ICU <- sum(subset(hospitalizados, ingreso_uci=='Si' & primera_entrada=='UCI')$cantidad, na.rm=T) / sum(subset(hospitalizados, primera_entrada!='NULL' | primera_entrada!='')$cantidad, na.rm=T)
# Probabilidad de quedarse en hospital ward (HW) primero
prob.HW <- sum(subset(hospitalizados, ingreso_hospitalario=='Si' & primera_entrada=='HOSP')$cantidad, na.rm=T) / sum(subset(hospitalizados, primera_entrada!='NULL' | primera_entrada!='')$cantidad, na.rm=T)


# -- Opciones en HW -- #
# Probabilidad de morir antes de pasar a UCI
prob.HW.death <- mean(subset(hospitalizados, ingreso_hospitalario=='Si' & ingreso_uci=='No')$proporcion_muertos, na.rm=T)

# Probabilidad de entrar en UCI
prob.HW.ICU <- sum(subset(hospitalizados, ingreso_hospitalario=='Si' & ingreso_uci=='Si')$cantidad, na.rm=T) / sum(hospitalizados$cantidad, na.rm=T)

# Probabilidad de irse de HW sin entrar en UCI (Esto no es del todo preciso, incluye a los que se mueren en HW)
prob.HW.disc <- sum(subset(hospitalizados, ingreso_hospitalario=='Si' & ingreso_uci=='No')$cantidad, na.rm=T) / sum(hospitalizados$cantidad, na.rm=T)


# -- Opciones en UCI -- #
# Probabilidad de morir tras ser admitido en UCI
prob.ICU.death <- mean(subset(hospitalizados, ingreso_uci=='Si')$proporcion_muertos, na.rm=T)

# Probabilidad de ser transferido a HW después de UCI
### PROBLEMA: no se puede ser muy preciso con este campo por falta de datos
prob.ICU.HW <- sum(subset(hospitalizados, ingreso_hospitalario=='Si' & ingreso_uci=='Si')$cantidad, na.rm=T)/ sum(subset(hospitalizados)$cantidad, na.rm=T)

# Sacar por pantalla las probabilidades
list(prob.ICU=prob.ICU, prob.HW=prob.HW, prob.HW.death=prob.HW.death, prob.HW.ICU=prob.HW.ICU, prob.HW.disc=prob.HW.disc, prob.ICU.death=prob.ICU.death, prob.ICU.HW=prob.ICU.HW)


# ------------- Weibull ---------------- 
# Dos maneras de calcular Weibull:
# - Con fórmulas / manualmente
# - Automáticamente con "eweibull" a partir de datos reales

# -- Con eweibull --
load("datos/full_weibull.Rdata")

# Ejemplos de datos incondicionales
weibull.HW.disc.inc

# Ejemplos de datos condicionales
weibull.HW.disc.cond

get.pams <- function(df, age, sex){
  # Encontrar fila más cercana a una edad por sexo
  # (Sólo para datos condicionales)
  matrix.by.sex <- subset(df, sexo==sex)
  sel.row <- matrix.by.sex[which.min(abs(matrix.by.sex$edad - age)),]
  return(sel.row)
}

# ---- Simulación paralelizada condicional ----

# Cada bucle paralelo crea una lista de resultados (n.HOS, n.ICU...)
# Al final se tiene una lista de longitud=par.m.loops, cada una con una lista de resultados
set.seed(123)

system.time({
res <- foreach (par.m=1:par.m.loops, .errorhandling="pass") %dopar% {
  # Se inicializan matrices vacías necesarias para la simulación
  # Dependiendo de cada una pueden tener dos dimensiones (simulacion*individuo)
  # o tres (simulacion*individuo*dia)
  age <- gender <- inf.time <- prob.rc <- final.state <- matrix(rep(NA, length.out=par.m.size*n.ind), nrow = par.m.size, ncol = n.ind) 
  n.HOS <- n.ICU <- n.Dead <- n.Discharge <- n.H.Dead <- n.ICU.Dead  <- matrix(rep(NA, length.out= par.m.size*n.time), nrow = par.m.size, ncol = n.time) 
  
  # Inicialización de matriz state, la cual contendrá, para cada simulación, el estado
  # de cada individuo por día
  state <- rep(NA, par.m.size*n.ind*n.time)
  dim(state) <- c(par.m.size, n.ind, n.time)
  
  for (j in 1:par.m.size) { # j = simulación
    #-------------------------------------------------------------
    # -- Definición del sexo de cada individuo usando Bernoulli --
    #-------------------------------------------------------------
    # Mujeres serán 1 y hombres 0
    bern.dist <- rbern(n.ind, prob=prob.w)
    gender[j,] <- bern.dist
    
    #-------------------------------------------------
    # -- Edad y posibilidad de ingresar en hospital --
    #-------------------------------------------------
    # Con la distribución de edades reales se selecciona una etapa (ej: 54.5) y su 
    # probabilidad de hospitalización (ej: 0.28) para cada individuo i en una simulación j
    # Mujeres (1)
    condicion <- which(bern.dist==1)
    n.interval <- sample(1:length(woman.age.prob),size=length(condicion),prob=woman.age.prob, replace=T)
    age[j,][condicion] <- women.mean.age.interval[n.interval]
    prob.rc[j,][condicion] <- prob.rc.woman[n.interval]
    
    # Hombres (0)
    condicion <- which(bern.dist==0)
    n.interval <- sample(1:length(man.age.prob),size=length(condicion),prob=man.age.prob, replace=T)
    age[j,][condicion] <- men.mean.age.interval[n.interval]
    prob.rc[j,][condicion] <- prob.rc.man[n.interval]
    
    #-----------------------------------------------------
    # -- Día en el que se infecta (distribución normal) --
    # ----------------------------------------------------
    inf.time[j,] <- rnorm(n=n.ind, mean=inf.time.avg, sd=inf.time.sd)
    
    # Definición del día de infección en state
    # Los días previos a la infección se guardan como "0", y el día de infección como "I"
    for (i in 1:n.ind){
      # Ceiling redondea hacia arriba (1.1->2)
      state[j, i, 1:(ceiling(inf.time[j,i])-1)] = 0
      state[j, i, ceiling(inf.time[j,i])] = "I"
    }
    
    #--------------------------------------------------------
    # -- Se definen individuos que ingresan en el hospital --
    #--------------------------------------------------------
    u <- runif(n.ind) # esto genera random deviates of the uniform distribution 
    ind.H <- which(u<=prob.rc[j,]) # qué individuos tienen la valor alatorio u <= probabilidad de ser hospitalizados
    
    # -- Para cada individuo seleccionado (que tiene la enfermedad y es hospitalizado) --
    for (i in ind.H){
      # -- Tiempo desde infección hasta el día de hospitalización --
      # Variables repetidas
      ind.age <- age[j,i]
      ind.gender <- gender[j,i]
      ind.inf.time <- inf.time[j,i]
      
      # Cada día en state desde el día de infección hasta el día de hospitalización se define como "I"
      t.inf.until.hosp <- rnorm(n=1, mean=12-0.05*ind.age, sd=1)
      state[j,i,(ceiling(ind.inf.time)+1):ceiling((ind.inf.time + t.inf.until.hosp-1))] = "I"
      
      # -- Parámetros Weibull según edad y sexo --
      if(modo.weibull=='manual'){
        # Modo: fórmula manual
        # Scale
        scale.ICU.death <- 15.5 * ( (100 - abs(ind.age- 60) - 10*ind.gender) / 62 )
        scale.ICU.HW <- 16.3 * ( (100 - abs(ind.age- 60) - 10*ind.gender) / 62 )
        scale.HW.disc <- 8.4 * ( (60 + ind.age-10*ind.gender)/100 )
        scale.HW.ICU <- 4.2
        # Shape
        shape.ICU.death <- 1.4
        shape.ICU.HW <- 1.8
        shape.HW.disc <- 2.6
        shape.HW.ICU <- 1.6        
      } else if (modo.weibull=='automatico') {
        # Modo: calculados por "eweibull" (pasa de 18 segundos a 36)
        # Filas objetivo
        weibull.ICU.death <- get.pams(weibull.ICU.death.cond, ind.age, ind.gender)
        weibull.ICU.HW <- get.pams(weibull.ICU.HW.cond, ind.age, ind.gender)
        weibull.HW.disc <- get.pams(weibull.HW.disc.cond, ind.age, ind.gender)
        weibull.HW.ICU <- get.pams(weibull.HW.ICU.cond, ind.age, ind.gender)
        # Scale
        scale.ICU.death <- weibull.ICU.death$scale
        scale.ICU.HW <- weibull.ICU.HW$scale
        scale.HW.disc <- weibull.HW.disc$scale
        scale.HW.ICU <- weibull.HW.ICU$scale
        # Shape
        shape.ICU.death <- weibull.ICU.death$shape
        shape.ICU.HW <- weibull.ICU.HW$shape
        shape.HW.disc <- weibull.HW.disc$shape
        shape.HW.ICU <- weibull.HW.ICU$shape        
      }

      
      #-------------------------------------------------
      # -- Simulación del primer estado del individuo --
      #-------------------------------------------------
      v1 <- runif(1) # probabilidad aleatoria de que empiece en HW (v1<=prob.HW) o en ICU (v1>=prob.HW)
      
      if (v1 <= prob.HW) {
        # Paciente entra en hospital ward (HW)
        v2 <- runif(1)
        if (v2 <= prob.HW.death) {# Patient dies in HW
          time.HW.death <- rexp(1, 0.1)# Of those admitted in hospital ward, the time to death 
          state[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.HW.death)-1] = "H"
          state[j,i,ceiling(ind.inf.time + t.inf.until.hosp + time.HW.death)] = "H.Dead"
          final.state[j,i] = "Dead"
        } else if ( (v2 > prob.HW.death) && (v2 <= prob.HW.death+prob.HW.ICU) ) {# Patient goes to ICU
          time.HW.ICU <- rweibull(1, shape=shape.HW.ICU, scale=scale.HW.ICU)# Time since hospital ward admission to ICU
          state[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.HW.ICU)] = "H"
          final.state[j,i] = "ICU"
        } else {# Patient discharged
          time.HW.disc <- rweibull(1, shape=shape.HW.disc, scale=scale.HW.disc )# Time since hospital ward admission to discharge is
          state[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.HW.disc)-1] = "H"
          state[j,i,ceiling(ind.inf.time + t.inf.until.hosp + time.HW.disc)] = "H.Discharge"
          final.state[j,i] = "Discharge"}
      } else {
        # Paciente entra en UCI
        v3 <- runif(1)
        if (v3 <= prob.ICU.death) {# Patient dies in ICU
          time.ICU.death <- rweibull(1, shape=shape.ICU.death, scale=scale.ICU.death)# Time from admission in ICU to death
          state[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.ICU.death)-1] = "ICU"
          state[j,i,ceiling(ind.inf.time + t.inf.until.hosp + time.ICU.death)] = "ICU.Dead"
          final.state[j,i] = "Dead"
        } else {# Patient goes to hospital ward
          time.ICU.HW <- rweibull(1, shape=shape.ICU.HW, scale=scale.ICU.HW ) # Time since admission in ICU till return to ward
          state[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.ICU.HW)] = "ICU"
          final.state[j,i] = "HOS"
        }}
      
      #-------------------------------------------------------------------------------
      # -- Simulación del resto de estados para pacientes que queden en el hospital --
      #-------------------------------------------------------------------------------
      # Los pacientes que llegan a este punto no han muerto ni han salido del hospital
      # Son los que su estado final todavía está por decidir
      # Se repite el bucle while hasta que los individuos mueran, salgan del hospital o se acabe la simulación
      while((final.state[j,i]=="HOS") | (final.state[j,i]=="ICU")){
        # Calcular el índice del siguiente día, en el que no se sabe el estado (es NA)
        i.final <- min(which(is.na(state[j,i,])))
        
        # Comprobar si se sobrepasa del tiempo de simulación
        if (is.infinite(i.final)){
          break
        }
        
        # -- Simulación --
        if(final.state[j,i]=="HOS"){
          # Si el estado final actual es hospital
          v2 <- runif(1)
          if (v2 <= prob.HW.death) {# Patient dies in HW
            time.HW.death <- rexp(1, 0.1)# Of those admitted in hospital ward, the time to death 
            state[j, i, i.final: pmin(n.time, ceiling(i.final+time.HW.death)-1)] = "H"
            state[j, i, pmin(n.time, ceiling(i.final+time.HW.death))] = "H.Dead"
            final.state[j,i] = "Dead"
          } else if ( (v2 > prob.HW.death) && (v2 <= prob.HW.death+prob.HW.ICU) ) {# Patient goes to ICU
            time.HW.ICU <- rweibull(1, shape=shape.HW.ICU, scale=scale.HW.ICU)# Time since hospital ward admission to ICU
            state[j,i,i.final: pmin(n.time, ceiling(i.final+time.HW.ICU))] = "H"
            final.state[j,i] = "ICU"
          } else {# Patient discharged
            time.HW.disc <- rweibull(1, shape=shape.HW.disc, scale=scale.HW.disc )# Time since hospital ward admission to discharge is
            state[j,i,i.final: pmin(n.time, ceiling(i.final+time.HW.disc)-1)] = "H"
            state[j,i,pmin(n.time, ceiling(i.final+time.HW.disc))] = "H.Discharge"
            final.state[j,i] = "Discharge"}
        } else {
          # Si el estado final actual es UCI
          v3 <- runif(1)
          if (v3 <= prob.ICU.death) {# Patient dies in ICU
            time.ICU.death <- rweibull(1, shape=shape.ICU.death, scale=scale.ICU.death)# Time from admission in ICU to death
            state[j,i,i.final : pmin(n.time, ceiling(i.final+time.ICU.death)-1)] = "ICU"
            state[j,i,pmin(n.time, ceiling(i.final+time.ICU.death))] = "ICU.Dead"
            final.state[j,i] = "Dead"
          } else {# Patient goes to hospital ward
            time.ICU.HW <- rweibull(1, shape=shape.ICU.HW, scale=scale.ICU.HW ) # Time since admission in ICU till return to ward
            state[j,i,i.final :pmin(n.time, ceiling(i.final+time.ICU.HW))] = "ICU"
            final.state[j,i] = "HOS"
          }}
      } # end while
    } # end i in n.ind
    
    #---------------------------------------------------------------------------------
    # Almacenar número de pacientes por simulación en cada estado por día (HOS, ICU, Dead, Discharge)
    #---------------------------------------------------------------------------------
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
})

# ----- Resultados condicionales ---- 
# El sistema produce resultados por hilo y es necesario juntarlos
# Se obtienen matrices de dimensiones simulaciones*pacientes*dias
get.sim.results <- function(res, name){
  return(do.call(rbind, lapply(res, function(x){x[[name]]})))
}
n.HOS <- get.sim.results(res, 'n.HOS')
n.ICU <- get.sim.results(res, 'n.ICU')
n.H.Dead <- get.sim.results(res, 'n.H.Dead')
n.Discharge <- get.sim.results(res, 'n.Discharge')
n.ICU.Dead <- get.sim.results(res, 'n.ICU.Dead')
n.Dead <- get.sim.results(res, 'n.Dead')

# Se calcula el número de individuos por día en cada categoría (pacientes*dias)
nHOS <- nICU <- nDead <- nDischarge <- nH.Dead <- nICU.Dead  <- rep(0, length.out= n.time) 
for (k in 1:n.time){
  nHOS[k] <- sum(n.HOS[,k], na.rm=T)/m
  nICU[k] <- sum(n.ICU[,k], na.rm=T)/m
  nDead[k] <- sum(n.Dead[,k], na.rm=T)/m
  nDischarge[k] <- sum(n.Discharge[,k], na.rm=T)/m
  nH.Dead[k] <- sum(n.H.Dead[,k], na.rm=T)/m
  nICU.Dead[k] <- sum(n.ICU.Dead[,k], na.rm=T)/m
}


# ---- Simulación paralelizada no condicional ----
# Cálculo previo de Weibull 
if(modo.weibull=='manual'){ # TODO: poner condicion
  # Modo: manual
  # Scale
  scale.ICU.death <- 15.5 
  scale.ICU.HW <- 16.3 
  scale.HW.disc <- 8.4
  scale.HW.ICU <- 4.2
  # Shape
  shape.ICU.death <- 1.4
  shape.ICU.HW <- 1.8
  shape.HW.disc <- 2.6
  shape.HW.ICU <- 1.6      
} else if (modo.weibull=='automatico') {
  # Modo: calculados por "eweibull"
  # Scale
  scale.ICU.death <- weibull.ICU.death.inc["scale"]
  scale.ICU.HW <- weibull.ICU.HW.inc["scale"]
  scale.HW.disc <- weibull.HW.disc.inc["scale"]
  scale.HW.ICU <- weibull.HW.ICU.inc["scale"]    
  # Shape
  shape.ICU.death <- weibull.ICU.death.inc["shape"]
  shape.ICU.HW <- weibull.ICU.HW.inc["shape"]
  shape.HW.disc <- weibull.HW.disc.inc["shape"]
  shape.HW.ICU <- weibull.HW.ICU.inc["shape"]       
}


# Cada bucle paralelo crea una lista de resultados (n.HOS, n.ICU...)
# Al final se tiene una lista de longitud=par.m.loops, cada una con una lista de resultados
set.seed(123)
res <- foreach (par.m=1:par.m.loops, .errorhandling="pas") %dopar% {
  # Se inicializan matrices vacías necesarias para la simulación
  # Dependiendo de cada una pueden tener dos dimensiones (simulacion*individuo)
  # o tres (simulacion*individuo*dia)
  age.inc <- gender.inc <- inf.time <- prob.rc <- final.state.inc <- matrix(rep(NA, length.out=par.m.size*n.ind), nrow = par.m.size, ncol = n.ind) 
  n.HOS.inc <- n.ICU.inc <- n.Dead.inc <- n.Discharge.inc <- n.H.Dead.inc <- n.ICU.inc.Dead.inc  <- matrix(rep(NA, length.out= par.m.size*n.time), nrow = par.m.size, ncol = n.time)
  
  # Inicialización de matriz state, la cual contendrá, para cada simulación, el estado
  # de cada individuo por día
  state.inc <- rep(NA, par.m.size*n.ind*n.time)
  dim(state.inc) <- c(par.m.size, n.ind, n.time)
  
  for (j in 1:par.m.size){
    #-------------------------------------------------------------
    # -- Definición del sexo de cada individuo usando Bernoulli --
    #-------------------------------------------------------------
    # Mujeres serán 1 y hombres 0
    gender.inc[j,] <- rbern(n.ind, prob=prob.w) # Gender from real data distribution
    
    #-------------------------------------------------
    # -- Edad y posibilidad de ingresar en hospital --
    #-------------------------------------------------
    for (i in 1:n.ind){
      if (gender.inc[j,i] == 1) { # mujeres
        n.interval <- sample(1:length(woman.age.prob),size=1,prob=woman.age.prob)
        age.inc[j,i] <- women.mean.age.interval[n.interval]
      } else { # hombres
        n.interval <- sample(1:length(man.age.prob),size=1,prob=man.age.prob)
        age.inc[j,i] <- men.mean.age.interval[n.interval]
      }
      
    }  
    prob.rc[j,] <- prob.rc.real
    
    #-----------------------------------------------------
    # -- Día en el que se infecta (distribución normal) --
    # ----------------------------------------------------
    inf.time[j,] <-rnorm(n=n.ind, mean=inf.time.avg, sd=inf.time.sd)
    
    # Definición del día de infección en state
    # Los días previos a la infección se guardan como "0", y el día de infección como "I"
    for (i in 1:n.ind){
      state.inc[j, i, 1:(ceiling(inf.time[j,i])-1)] = 0
      state.inc[j, i, ceiling(inf.time[j,i])] = "I"
    }
    
    #--------------------------------------------------------
    # -- Se definen individuos que ingresan en el hospital --
    #--------------------------------------------------------
    u <- runif(n.ind)
    ind.H <- which(u<=prob.rc[j,])
    
    # -- Para cada individuo seleccionado (que tiene la enfermedad y es hospitalizado) --
    for (i in ind.H){
      # Variables repetidas
      ind.inf.time <- inf.time[j,i]
      
      # Time since infection until hospital admission
      t.inf.until.hosp <- rnorm(n=1, mean=12-0.05*age.inc[j,i], sd=1)
      state.inc[j,i,(ceiling(ind.inf.time)+1):ceiling((ind.inf.time + t.inf.until.hosp-1))] = "I"
      
      #-------------------------------------------------
      # -- Simulación del primer estado del individuo --
      #-------------------------------------------------
      v1 <- runif(1)
      if (v1 <= prob.HW) {# Patient in hospital ward
        v2 <- runif(1)
        if (v2 <= prob.HW.death) {# Patient dies in HW
          time.HW.death <- rexp(1, 0.1)# Of those admitted in hospital ward, the time to death 
          state.inc[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.HW.death)-1] = "H"
          state.inc[j,i,ceiling(ind.inf.time + t.inf.until.hosp + time.HW.death)] = "H.Dead"
          final.state.inc[j,i] = "Dead"
        } else if ( (v2 > prob.HW.death) && (v2 <= prob.HW.death+prob.HW.ICU) ) {# Patient goes to ICU
          time.HW.ICU <- rweibull(1, shape=shape.HW.ICU, scale=scale.HW.ICU)# Time since hospital ward admission to ICU
          state.inc[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.HW.ICU)] = "H"
          final.state.inc[j,i] = "ICU"
        } else {# Patient discharged
          time.HW.disc <- rweibull(1, shape=shape.HW.disc, scale=scale.HW.disc )# Time since hospital ward admission to discharge is
          state.inc[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.HW.disc)-1] = "H"
          state.inc[j,i,ceiling(ind.inf.time + t.inf.until.hosp + time.HW.disc)] = "H.Discharge"
          final.state.inc[j,i] = "Discharge"}
      } else {
        #---------------------------------------------
        # Patient in ICU
        v3 <- runif(1)
        if (v3 <= prob.ICU.death) {# Patient dies in ICU
          time.ICU.death <- rweibull(1, shape=shape.ICU.death, scale=scale.ICU.death)# Time from admission in ICU to death
          state.inc[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.ICU.death)-1] = "ICU"
          state.inc[j,i,ceiling(ind.inf.time + t.inf.until.hosp + time.ICU.death)] = "ICU.Dead"
          final.state.inc[j,i] = "Dead"
        } else {# Patient goes to hospital ward
          time.ICU.HW <- rweibull(1, shape=shape.ICU.HW, scale=scale.ICU.HW ) # Time since admission in ICU till return to ward
          state.inc[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.ICU.HW)] = "ICU"
          final.state.inc[j,i] = "HOS"
        }}
      
      #-------------------------------------------------------------------------------
      # -- Simulación del resto de estados para pacientes que queden en el hospital --
      #-------------------------------------------------------------------------------
      # Los pacientes que llegan a este punto no han muerto ni han salido del hospital
      # Son los que su estado final todavía está por decidir
      # Se repite el bucle while hasta que los individuos mueran, salgan del hospital o se acabe la simulación
      while((final.state.inc[j,i]=="HOS") | (final.state.inc[j,i]=="ICU")){
        # Calcular el índice del siguiente día, en el que no se sabe el estado (es NA)
        i.final <- min(which(is.na(state.inc[j,i,])))

        # Comprobar si se sobrepasa del tiempo de simulación
        if (is.infinite(i.final)){
          break
        }
        
        # -- Simulación --
        if(final.state.inc[j,i]=="HOS"){
          # Si el estado final actual es HOS
          v2 <- runif(1)
          if (v2 <= prob.HW.death) {# Patient dies in HW
            time.HW.death <- rexp(1, 0.1)# Of those admitted in hospital ward, the time to death 
            state.inc[j, i, i.final: pmin(n.time,ceiling(i.final+time.HW.death)-1)] = "H"
            state.inc[j, i, pmin(n.time,ceiling(i.final+time.HW.death))] = "H.Dead"
            final.state.inc[j,i] = "Dead"
          } else if ( (v2 > prob.HW.death) && (v2 <= prob.HW.death+prob.HW.ICU) ) {# Patient goes to ICU
            time.HW.ICU <- rweibull(1, shape=shape.HW.ICU, scale=scale.HW.ICU)# Time since hospital ward admission to ICU
            state.inc[j,i,i.final: pmin(n.time,ceiling(i.final+time.HW.ICU))] = "H"
            final.state.inc[j,i] = "ICU"
          } else {# Patient discharged
            time.HW.disc <- rweibull(1, shape=shape.HW.disc, scale=scale.HW.disc)# Time since hospital ward admission to discharge is
            state.inc[j,i,i.final: pmin(n.time,ceiling(i.final+time.HW.disc)-1)] = "H"
            state.inc[j,i,pmin(n.time,ceiling(i.final+time.HW.disc))] = "H.Discharge"
            final.state.inc[j,i] = "Discharge"}
        } else {
          # Si el estado final actual es UCI
          v3 <- runif(1)
          if (v3 <= prob.ICU.death) {# Patient dies in ICU
            time.ICU.death <- rweibull(1, shape=shape.ICU.death, scale=scale.ICU.death)# Time from admission in ICU to death
            state.inc[j,i,i.final : pmin(n.time,ceiling(i.final+time.ICU.death)-1)] = "ICU"
            state.inc[j,i,pmin(n.time,ceiling(i.final+time.ICU.death))] = "ICU.Dead"
            final.state.inc[j,i] = "Dead"
          } else {# Patient goes to hospital ward
            time.ICU.HW <- rweibull(1, shape=shape.ICU.HW, scale=scale.ICU.HW ) # Time since admission in ICU till return to ward
            state.inc[j,i,i.final : pmin(n.time,ceiling(i.final+time.ICU.HW))] = "ICU"
            final.state.inc[j,i] = "HOS"
          }}
        
      } # end while
    } # end i in n.ind
    
    #---------------------------------------------------------------------------------
    # Almacenar número de pacientes por simulación en cada estado por día (HOS, ICU, Dead, Discharge)
    #---------------------------------------------------------------------------------
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


# ---- Resultados no condicionales ----
# El sistema produce resultados por hilo y es necesario juntarlos
# Se obtienen matrices de dimensiones simulaciones*pacientes*dias
get.sim.results <- function(res, name){
  return(do.call(rbind, lapply(res, function(x){x[[name]]})))
}

n.HOS.inc <- get.sim.results(res, 'n.HOS.inc')
n.ICU.inc <- get.sim.results(res, 'n.ICU.inc')
n.Dead.inc <- get.sim.results(res, 'n.Dead.inc')
n.Discharge.inc <- get.sim.results(res, 'n.Discharge.inc')
n.H.Dead.inc <- get.sim.results(res, 'n.H.Dead.inc')
n.ICU.inc.Dead.inc <- get.sim.results(res, 'n.ICU.inc.Dead.inc')

# Se calcula el número de individuos por día en cada categoría (pacientes*dias)
nHOS.inc <- nICU.inc <- nDead.inc <- nDischarge.inc <- nH.Dead.inc <- nICU.inc.Dead  <- rep(0, length.out= n.time) 
for (k in 1:n.time){
  nHOS.inc[k] <- sum(n.HOS.inc[,k], na.rm=T)/m
  nICU.inc[k] <- sum(n.ICU.inc[,k], na.rm=T)/m
  nDead.inc[k] <- sum(n.Dead.inc[,k], na.rm=T)/m
  nDischarge.inc[k] <- sum(n.Discharge.inc[,k], na.rm=T)/m
  nH.Dead.inc[k] <- sum(n.H.Dead.inc[,k], na.rm=T)/m
  nICU.inc.Dead[k] <-sum(n.ICU.inc.Dead.inc[,k], na.rm=T)/m
}


# ---- Examen de días por encima del límite ----
check.hosp.capacity <- function(hosp, icu, neto, t){
  par(mfrow=c(1,1))
  # ---- Ver si en algún momento se superó el número de camas ---- #
  cap.convencional <- area.capacity.stats['mediana','Hospitalización convencional']
  cap.uci <- sum(area.capacity.stats['mediana',c('U. Críticas CON respirador', 'U. Críticas SIN respirador')])
  
  # Número de días que se sobrepasa
  sim.tot.hosp <- hosp+icu
  dias.sobrepasados.convencional <- sum(sim.tot.hosp>=cap.convencional)
  dias.sobrepasados.uci <- sum(icu>=cap.uci)
  
  # Gráficas
  plot(NA, xlim=c(0,n.time), ylim=c(0,max(max(sim.tot.hosp)+20)), xlab="Días", ylab="Casos", main=t)
  
  add.range <- function(capacity.stats, unidad, col){
    # Esta función añade el área de cada unidad hospitalaria, desde p10 a p90 a una gráfica
    mediana <- capacity.stats['mediana',unidad]
    p10 <- capacity.stats['percentil10',unidad]
    p90 <- capacity.stats['percentil90',unidad]
    abline(h=p10, col='black', lty=2)
    abline(h=p90, col='black', lty=2) 
    abline(h=mediana, col='red', lty=1)
    rect(0-50,p90,
         n.time+50,p10,
         col= col, lwd=0)    
  }
  add.range(area.capacity.stats,'Hospitalización convencional',rgb(0,1,0,alpha=0.1))
  add.range(area.capacity.stats,'U. Críticas CON respirador',rgb(0,0,1,alpha=0.1))
  add.range(area.capacity.stats,'U. Críticas SIN respirador',rgb(1,0,0,alpha=0.1))
  
  lines(hosp, type="l",lty=1, lwd=2, col='pink')
  lines(icu, type="l",lty=1, lwd=2, col='red')
  lines(sim.tot.hosp, type="l",lty=1, lwd=2, col='orange')
  lines(neto,lty=1, lwd=2, col='green')
  
  legend("topright", legend = c("nHOS", "nICU",'nHOS+nICU','Cambio neto (in-out)'),
         col = c('pink','red','orange','green'), lty=c(1,1,1,1,2), pch = c(NA,NA,NA,NA), bty = "n")
  
  title(sub=paste('Días sobrepasados en HOSP: ', dias.sobrepasados.convencional), adj=1, line=2, font=2,cex.sub = 0.75)
  title(sub=paste('Días sobrepasados en UCI: ', dias.sobrepasados.uci), adj=1, line=3, font=2,cex.sub = 0.75)
  
  # Por cuánto se sobrepasa
  # plot(sim.tot.hosp[sim.tot.hosp>=cap]-cap, ylab='Pacientes sin cama', xlab='Días')
}

check.hosp.capacity.interactive <- function(hosp, icu, neto, tipo, cap.stats, time){
  datos <- data.frame(days=1:time, hos=hosp, icu=icu, cambio.neto=neto)
  
  p <- ggplot(data=datos,aes(x=days,y=hos)) + geom_line(color="#E69F00") +
    geom_line(aes(x=days, y=icu), color="#56B4E9") + 
    geom_line(aes(x=days, y=cambio.neto), color="#009E73") +
    geom_line(aes(x=days, y=icu+hos), color="#111111") + 
    ggtitle(tipo) + labs(y="Hospitalizados", x = "Días") +
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) 
  
  font <- list(
    family = "Roboto Condensed",
    size = 10,
    color = "white"
  )
  label <- list(
    bgcolor = "#232F34",
    bordercolor = "transparent",
    font = font
  )
  
  ggplotly(p, tooltip="y") %>% 
    style(hoverlabel = label) %>%
    layout(hovermode = "x unified")
}

cambio.neto <- nHOS+nICU-(nDischarge+nDead+nH.Dead+nICU.Dead)
check.hosp.capacity(nHOS, nICU, cambio.neto, t='Condicional')
check.hosp.capacity.interactive(nHOS, nICU, cambio.neto, 'Condicional', area.capacity.stats, time=n.time)

cambio.neto <- nHOS.inc+nICU.inc-(nDischarge.inc+nDead.inc+nH.Dead.inc+nICU.inc.Dead)
check.hosp.capacity(nHOS.inc, nICU.inc, cambio.neto, t='No condicional')
check.hosp.capacity.interactive(nHOS, nICU, cambio.neto, 'Condicional', area.capacity.stats, time=n.time)







