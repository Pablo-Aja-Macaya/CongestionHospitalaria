for (j in 1:m){
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
  
  # n.HOS.inc <- n.ICU.inc <- n.Dead.inc <- n.Discharge.inc <- n.H.Dead.inc <- n.ICU.inc.Dead.inc  <- matrix(rep(NA, length.out= m*n.time), nrow = m, ncol = n.time) 
  
  for (k in 1:n.time){
    n.HOS.inc[j,k] <- length(which(state.inc[j, ,k]=="H"))
    n.ICU.inc[j,k] <- length(which(state.inc[j, ,k]=="ICU"))
    n.H.Dead.inc[j,k] <- length(which(state.inc[j, ,k]=="H.Dead"))
    n.Discharge.inc[j,k] <- length(which(state.inc[j, ,k]=="H.Discharge"))
    n.ICU.inc.Dead.inc[j,k] <- length(which(state.inc[j, ,k]=="ICU.Dead"))
    n.Dead.inc[j,k] <- n.H.Dead.inc[j,k] + n.ICU.inc.Dead.inc[j,k]
  }
} # end j in m
