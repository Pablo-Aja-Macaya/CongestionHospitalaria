#### Simulación secuencial ####

##########################################
# ------ Variables de simulación ------- #
##########################################

m <- 1000 # Number of samples m=1000
n.ind <- 1000 # infected individuals N=1000
n.time <- 250 # Number of days (follow-up time)

age <- gender <- inf.time <- prob.rc <- final.state <- matrix(rep(NA, length.out=m*n.ind), nrow = m, ncol = n.ind) 
state <- rep(NA, m*n.ind*n.time)
dim(state) <- c(m, n.ind, n.time)

##########################################
# ------------ Simulación -------------- #
##########################################
n.HOS <- n.ICU <- n.Dead <- n.Discharge <- n.H.Dead <- n.ICU.Dead  <- matrix(rep(NA, length.out= m*n.time), nrow = m, ncol = n.time) 

set.seed(123)

for (j in 1:m) {
  # cat("j=",j,"\n")
  if(j%%10==0) cat("j=",j,"\n")
  
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
  
  # Cuándo empieza la infección
  inf.time[j,] <- rnorm(n=n.ind, mean=60, sd=10)
  
  #----------------------------------------------------
  # We define the times with infection as the state "I"
  #----------------------------------------------------
  for (i in 1:n.ind){
    # Ceiling redondea hacia arriba (1.1->2)
    # Todo lo que está antes del punto de infección es 0, y el punto de infección es I
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

