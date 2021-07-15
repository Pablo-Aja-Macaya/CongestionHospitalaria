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




for (i in 1:n.ind){
  # Ceiling redondea hacia arriba (1.1->2)
  state[j, i, 1:(ceiling(inf.time[j,i])-1)] = 0
  state[j, i, ceiling(inf.time[j,i])] = "I"
}


# para cada individuo hay que poner en k qué tiene cada día

1:(ceiling(inf.time[j,])-1)

state[j,i,][]


tmp <- lapply(ceiling(inf.time[j,])-1, function(v){1:v})




# Paralelizada: 24, Secuencial: 56
# Paralelizada vectorizando age y propb.rc: 18-20, paralelizada sin vectorizar: 22
