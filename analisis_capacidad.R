library(Rlab)
library(data.table)
library(scales)
library(foreach)
library(doParallel)
library(DT)
library(readr)
library(glue)

#  Variables de datos
area.sanitaria <- 'all' # si se pone 'all' se eligen todas
areas.hospitales <- data.frame(read_csv("datos/areas_hospitales_correspondencia.csv"))
capacidad <- data.frame(read_csv("datos/capacidadasistencial.csv", locale = locale(encoding = "ISO-8859-1"))) # capacidad asistencial
names(capacidad) <- tolower(names(capacidad))

# Meter áreas sanitarias (este paso seguramente es temporal, en el futuro vendrá dentro de capacidad)
capacidad <- merge(areas.hospitales, capacidad, by='hospital')

# Eliminar columna id (no hace falta)
capacidad$id <- NULL

# Filtrar por el área sanitaria que se quiera
filter.cases <- function(df,a){
  # Filtra el dataframe de casos según área sanitaria
  # Si se pone 'all' se cogen todas las áreas
  if (a=='all'){
    casos <- df
  } else {
    casos <- subset(df, area==a)
  }
  return(casos)
}
capacidad <- filter.cases(capacidad, 'Santiago de Compostela - Barbanza')

# Hospitales
hospitales <- sort(unique(capacidad$hospital))

# Unidades
unidades <- sort(unique(capacidad$unidad))

# Lista para guardar dataframes de cada hospital con la unidad y cuentas
hospital.capacity.stats <- list()

# Capacidad en cada hospital
par(mfrow=c(2,3))
for (h in hospitales){
  # Inicialización de dataframe de unidades*medidas para el hospital
  hospital.capacity.stats[[h]] <- data.frame(matrix(ncol = length(unidades), nrow = 3))
  names(hospital.capacity.stats[[h]]) <- unidades
  row.names(hospital.capacity.stats[[h]]) <- c('mediana','percentil10','percentil90')
  for (u in unidades){
    # Total de camas de este hospital por unidad y ordenadas por fecha
    tmp <- subset(capacidad, hospital==h & unidad==u)
    tmp <- tmp[order(tmp$fecha_envio),]
    
    tot <- tmp$total_camas
    
    # Mediana y percentiles
    mediana <- median(tot, na.rm=T)
    percentiles <- quantile(tot, probs = seq(0, 1, by= 0.1), na.rm=T)
    
    # Histograma
    hist(tot, main=glue('{h} \n({u})'), col='gray', xlab='Total camas') 
    title(sub=paste('Mediana:', mediana), adj=1, line=2, font=2,cex.sub = 0.75)
    title(sub=paste('Percentil 10:', percentiles[['10%']]), adj=1, line=3, font=2,cex.sub = 0.75)
    title(sub=paste('Percentil 90:', percentiles[['90%']]), adj=1, line=4, font=2,cex.sub = 0.75)
    
    # Plot de número de camas a lo largo de la pandemia
    plot(total_camas ~ fecha_envio, tmp, xaxt = "n", type = "l", main=glue('{h} \n({u})'), xlab=NA)
    axis.Date(1, at=seq(min(tmp$fecha_envio), max(tmp$fecha_envio), length.out=10), 
              format='%b %Y', las=2, cex.axis=0.8)    

    plot(ocupadas_covid19 ~ fecha_envio, tmp, ylim=c(0,max(tmp$ocupadas_no_covid19, tmp$ocupadas_covid19, na.rm=T)),
         xaxt = "n", type="l",lty=2, lwd=2, col='red', ylab='ocupadas', xlab=NA,main=glue('{h} \n({u})'))
    lines(ocupadas_no_covid19 ~ fecha_envio, tmp, type="l",lty=2, lwd=2, col='blue')
    axis.Date(1, at=seq(min(tmp$fecha_envio), max(tmp$fecha_envio), length.out=10), 
              format='%b %Y', las=2, cex.axis=0.8)  
    
    # Meter resultados en la lista
    hospital.capacity.stats[[h]][[u]] <- c(mediana, percentiles[['10%']], percentiles[['90%']])
  }
}
par(mfrow=c(1,1))

# Resultados de cada hospital por unidad
hospital.capacity.stats


tmp <- hospital.capacity.stats[['COMPLEXO HOSPITALARIO UNIVERSITARIO DE OURENSE (CHUO)']]

plot(NA, xlim=c(0,100), ylim=c(0,1000))
abline(h=tmp['Hospitalización convencional']['mediana',], col='blue', lty=1)
abline(h=tmp['Hospitalización convencional']['percentil10',], col='red', lty=2)
abline(h=tmp['Hospitalización convencional']['percentil90',], col='red', lty=2)




tmp <- capacidad[order(capacidad$fecha_envio),]







