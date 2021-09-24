## ATENCION: en la no condicional falta adaptar esta vectorización y poner los pmin

tmp <- rbern(n.ind, prob=prob.w)
gender[j,] <- tmp

# Age and hospital admission real data distribution
# Mujeres (1)
condicion <- which(tmp==1)
n.interval <- sample(1:length(woman.age.prob),size=length(condicion),prob=woman.age.prob, replace=T)
age[j,][condicion] <- women.mean.age.interval[n.interval]
prob.rc[j,][condicion] <- prob.rc.woman[n.interval]

# Hombres (0)
condicion <- which(tmp==0)
n.interval <- sample(1:length(man.age.prob),size=length(condicion),prob=man.age.prob, replace=T)
age[j,][condicion] <- men.mean.age.interval[n.interval]
prob.rc[j,][condicion] <- prob.rc.man[n.interval]


# Paralelizada: 24, Secuencial: 56
# Paralelizada vectorizando age y propb.rc: 18-20, paralelizada sin vectorizar: 22


# ---- Vectorización de simulación de estados ----

# We define the times with infection as the state "I"
state[j,,][col(state[j,,]) >= 1 & col(state[j,,]) <= (ceiling(inf.time[j,])-1)] <- 0
state[j,,][col(state[j,,]) == ceiling(inf.time[j,])] <- 'I'

# Times of patients in hospital
u <- runif(n.ind)
ind.H <- which(u<=prob.rc[j,])

# Time since infection until hospital admission
t.inf.until.hosp <- rnorm(n=n.ind, mean=12-0.05*age[j,], sd=1)
dia.posterior.inf <- ceiling(inf.time[j,])+1 # día inicial de infección +1
dia.prev.hospitalizacion <- ceiling(inf.time[j,] + t.inf.until.hosp - 1) # útlimo día de infección antes de hospital
state[j,ind.H,][col(state[j,ind.H,]) >= dia.posterior.inf[ind.H] & col(state[j,ind.H,]) <= dia.prev.hospitalizacion[ind.H]]<- 'I'

# The parameters of the Weibulls are computed only once
scale.ICU.death <- 15.5 * ( (100 - abs(age[j,]- 60) - 10*gender[j,]) / 62) 
scale.ICU.HW <- 16.3 * ( (100 - abs(age[j,]- 60) - 10*gender[j,])/62)  
scale.HW.disc <- 8.4 * ((60 + age[j,]-10*gender[j,])/100)     
scale.HW.ICU <- 4.2


####################
# - First states - #
####################
# Quiénes entran a HW y quiénes a UCI
v1 <- runif(length(ind.H))

# Empieza en HW
patient.to.hos <- which(v1<=prob.HW)
v2 <- runif(length(patient.to.hos))

# Empieza en UCI
patient.to.icu <- which(!v1<=prob.HW)
v3 <- runif(length(patient.to.icu))

###########################################
# Entra en HW y muere
patient.hw.death <- which(v2<=prob.HW.death)
patients <- ind.H[patient.to.hos][patient.hw.death]
time.HW.death <- rexp(length(patient.hw.death), 0.1)
state.cols <- col(matrix(state[j,patients,,drop=FALSE], nrow=length(patients), ncol=n.time))
# Periodo entre HW y evento -1
d1 <- ceiling(inf.time[j,patients] + t.inf.until.hosp[patients])
d2 <- ceiling(inf.time[j,patients] + t.inf.until.hosp[patients] + time.HW.death)-1
state[j,patients,][state.cols >= d1 & state.cols <= d2] <- 'H'
# Día de evento
d3 <- ceiling(inf.time[j,patients] + t.inf.until.hosp[patients] + time.HW.death)
state[j,patients,][state.cols == d3] <- 'H.Dead'
# Estado final
final.state[j, patients] <- 'Dead'

#--------------------------------------------#

# Entra en HW y va a ICU
patient.hw.icu <- which(v2>prob.HW.death & v2 <= prob.HW.death+prob.HW.ICU)
patients <- ind.H[patient.to.hos][patient.hw.icu]
time.HW.ICU <- rweibull(length(patient.hw.icu), shape=1.6, scale=scale.HW.ICU)
state.cols <- col(matrix(state[j,patients,,drop=FALSE], nrow=length(patients), ncol=n.time))
# Periodo entre HW y evento
d1 <- ceiling(inf.time[j,patients] + t.inf.until.hosp[patients])
d2 <- ceiling(inf.time[j,patients] + t.inf.until.hosp[patients] + time.HW.ICU)
state[j,patients,][state.cols >= d1 & state.cols <= d2] <- 'H'
# Estado final
final.state[j, patients] <- 'ICU'

#----------------------------------------------#

# Entra en HW y se va
patient.hw.discharge <- which(v2 > prob.HW.death+prob.HW.ICU)
patients <- ind.H[patient.to.hos][patient.hw.discharge]
time.HW.disc <- rweibull(length(patient.hw.discharge), shape=2.6, scale=scale.HW.disc[patients])
state.cols <- col(matrix(state[j,patients,,drop=FALSE], nrow=length(patients), ncol=n.time))
# Periodo entre HW y evento
d1 <- ceiling(inf.time[j,patients] + t.inf.until.hosp[patients])
d2 <- ceiling(inf.time[j,patients] + t.inf.until.hosp[patients] + time.HW.disc)-1
state[j,patients,][state.cols >= d1 & col(state[j,patients,]) <= d2] <- 'H'
# Día de evento
d3 <- ceiling(inf.time[j,patients] + t.inf.until.hosp[patients] + time.HW.disc)
state[j,patients,][state.cols == d3] <- 'H.Discharge'
# Estado final
final.state[j, patients] <- 'Discharge'

###################################################
# Entra en UCI y muere
patient.icu.death <- which(v3<=prob.ICU.death)
patients <- ind.H[patient.to.hos][patient.icu.death]
time.ICU.death <- rweibull(length(patient.icu.death), shape=1.4, scale=scale.ICU.death[patients])
state.cols <- col(matrix(state[j,patients,,drop=FALSE], nrow=length(patients), ncol=n.time))
# Periodo entre UCI y evento
d1 <- ceiling(inf.time[j,patients] + t.inf.until.hosp[patients])
d2 <- ceiling(inf.time[j,patients] + t.inf.until.hosp[patients] + time.ICU.death)-1
state[j,patients,][state.cols >= d1 & state.cols <= d2] <- 'ICU'
# Día de evento
d3 <- ceiling(inf.time[j,patients] + t.inf.until.hosp[patients] + time.ICU.death)
state[j,patients,][state.cols == d3] <- 'ICU.Dead'
# Estado final
final.state[j, patients] <- 'Dead'

# Entra en UCI y va a HW
patient.icu.hw <- which(!v3<=prob.ICU.death)
patients <- ind.H[patient.to.hos][patient.icu.hw]
time.ICU.HW <- rweibull(length(patient.icu.hw), shape=1.8, scale=scale.ICU.HW[patients] )
state.cols <- col(matrix(state[j,patients,,drop=FALSE], nrow=length(patients), ncol=n.time))
# Periodo entre UCI y evento
d1 <- ceiling(inf.time[j,patients] + t.inf.until.hosp[patients])
d2 <- ceiling(inf.time[j,patients] + t.inf.until.hosp[patients] + time.ICU.HW)
state[j,patients,][state.cols >= d1 & state.cols <= d2] <- 'ICU'
# Estado final
final.state[j, patients] <- 'HOS'


########################
# - Following states - #
########################
keep.simulating = TRUE
while (keep.simulating){
  # Cuántos quedan sin resolución
  remaining <- which((final.state[j,]=="HOS") | (final.state[j,]=="ICU"))
  # Si no queda ninguno el bucle acaba
  if (length(remaining)==0){
    keep.simulating = FALSE
    break
  } else if (length(remaining==1)){
    i.final <- apply(is.na(state[j,remaining,,drop=F]), 1, function(r){min(which(r))})
  } else {
    i.final <- apply(is.na(state[j,remaining,]), 1, function(r){min(which(r))})
  }
  
  # Si el paciente está en HW
  patients.in.hw <- which(final.state[j,remaining]=='HOS')
  v2 <- runif(length(patients.in.hw))
  
  # Si el paciente está en UCI
  patients.in.icu <- which(final.state[j,remaining]=='ICU')
  v3 <- runif(length(patients.in.icu))  
  
  #------------------------------------------------------------#
  
  # Paciente muere en HW
  patient.hw.death <- which(v2<=prob.HW.death)
  patients <- remaining[patients.in.hw][patient.hw.death]
  time.HW.death <- rexp(length(patient.hw.death), 0.1)
  state.cols <- col(matrix(state[j,patients,,drop=FALSE], nrow=length(patients), ncol=n.time))
  # Periodo entre HW y evento
  d1 <- i.final[patient.hw.death]
  d2 <- ceiling(i.final[patient.hw.death]+time.HW.death)-1
  state[j,patients,][state.cols >= d1 & state.cols <= d2] <- 'H'
  # Día de evento
  d3 <- ceiling(i.final[patient.hw.death]+time.HW.death)
  state[j,patients,][state.cols == d3] <- 'H.Dead'
  # Estado final
  final.state[j,patients] <- 'Dead'
  
  #-------------------------------------------------------------#
  
  # Paciente va de HW a UCI
  patient.hw.icu <- which((v2 > prob.HW.death) & (v2 <= prob.HW.death+prob.HW.ICU))
  patients <- remaining[patients.in.hw][patient.hw.icu]
  time.HW.ICU <- rweibull(1, shape=1.6, scale=scale.HW.ICU)
  state.cols <- col(matrix(state[j,patients,,drop=FALSE], nrow=length(patients), ncol=n.time))
  # Periodo entre HW y evento
  d1 <- i.final[patient.hw.icu]
  d2 <- ceiling(i.final[patient.hw.icu]+time.HW.ICU)
  state[j,patients,][state.cols >= d1 & state.cols <= d2] <- 'H'
  # Estado final
  final.state[j,patients] <- 'ICU'  
  
  #-------------------------------------------------------------#
  
  # Paciente se va de HW
  patient.hw.discharge <- which((v2 > prob.HW.death) & (v2 > prob.HW.death+prob.HW.ICU))
  patients <- remaining[patients.in.hw][patient.hw.discharge]
  time.HW.disc <- rweibull(length(patient.hw.discharge), shape=2.6, scale=scale.HW.disc[patients] )
  state.cols <- col(matrix(state[j,patients,,drop=FALSE], nrow=length(patients), ncol=n.time))
  # Periodo entre HW y evento
  d1 <- i.final[patient.hw.discharge]
  d2 <- ceiling(i.final[patient.hw.discharge]+time.HW.disc)-1
  state[j,patients,][state.cols >= d1 & state.cols <= d2] <- 'H'
  # Día de evento
  d3 <- ceiling(i.final[patient.hw.discharge]+time.HW.disc)
  state[j,patients,][state.cols == d3] <- 'H.Discharge'
  # Estado final
  final.state[j,patients] <- 'Discharge'    
  
  #--------------------------------------------------------------#
  
  # Paciente muere en UCI
  patient.icu.death <- which(v3<=prob.ICU.death)
  patients <- remaining[patients.in.icu][patient.icu.death]
  time.ICU.death <- rweibull(length(patient.icu.death), shape=1.4, scale=scale.ICU.death[patients])
  state.cols <- col(matrix(state[j,patients,,drop=FALSE], nrow=length(patients), ncol=n.time))
  # Periodo entre HW y evento
  d1 <- i.final[patient.icu.death]
  d2 <- ceiling(i.final[patient.icu.death]+time.ICU.death)-1
  state[j,patients,][state.cols >= d1 & state.cols <= d2] <- 'ICU'
  # Día de evento
  d3 <- ceiling(i.final[patient.icu.death]+time.ICU.death)
  state[j,patients,][state.cols == d3] <- 'ICU.Dead'
  # Estado final
  final.state[j,patients] <- 'Dead'  
  
  #----------------------------------------------------------------#
  
  # Paciente va de UCI a HW
  patient.icu.hw <- which(v3>prob.ICU.death)
  patients <- remaining[patients.in.icu][patient.icu.hw]
  time.ICU.HW <-  rweibull(length(patient.icu.hw), shape=1.8, scale=scale.ICU.HW[patients] )
  state.cols <- col(matrix(state[j,patients,,drop=FALSE], nrow=length(patients), ncol=n.time))
  # Periodo entre HW y evento
  d1 <- i.final[patient.icu.hw]
  d2 <- ceiling(i.final[patient.icu.hw]+time.ICU.HW)
  state[j,patients,][state.cols >= d1 & state.cols <= d2] <- 'ICU'
  # Estado final
  final.state[j,patients] <- 'HOS'  
  
}










