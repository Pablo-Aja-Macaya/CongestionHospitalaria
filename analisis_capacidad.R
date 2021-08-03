library(Rlab)
library(data.table)
library(scales)
library(foreach)
library(doParallel)
library(DT)
library(readr)
library(glue)
library(dplyr)

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
capacidad <- filter.cases(capacidad, 'all')

# Filtrar por referencia
# capacidad <- subset(capacidad, referencia==1)

# Añadir total de ocupadas
capacidad$total_ocupadas <- capacidad$ocupadas_covid19 + capacidad$ocupadas_no_covid19

# Hospitales
hospitales <- sort(unique(capacidad$hospital))

# Unidades
unidades <- sort(unique(capacidad$unidad))

# Lista para guardar dataframes de cada hospital con la unidad y cuentas
hospital.capacity.stats <- list()

# Capacidad en cada hospital
par(mfrow=c(2,2))
for (h in hospitales){
  # Inicialización de dataframe de unidades*medidas para el hospital
  hospital.capacity.stats[[h]] <- data.frame(matrix(ncol = length(unidades), nrow = 3))
  names(hospital.capacity.stats[[h]]) <- unidades
  row.names(hospital.capacity.stats[[h]]) <- c('mediana','percentil10','percentil90')
  for (u in unidades){
    # Total de camas de este hospital por unidad y ordenadas por fecha
    datos <- subset(capacidad, hospital==h & unidad==u)
    datos <- datos[order(datos$fecha_envio),]

    tot <- datos$total_camas
    fechas <- datos$fecha_envio
    tot.ocupadas <- datos$total_ocupadas
    
    # Mediana y percentiles
    mediana <- median(tot, na.rm=T)
    percentiles <- quantile(tot, probs = seq(0, 1, by= 0.1), na.rm=T)
    
    # Calcular en qué puntos se vería sobrepasado el hospital
    min.sobrepasado <- fechas[which(tot.ocupadas>percentiles[['10%']])]
    max.sobrepasado <- fechas[which(tot.ocupadas>percentiles[['90%']])]
    median.sobrepasado <- fechas[which(tot.ocupadas>mediana)]
    
    # Histograma
    hist(tot, main=glue('{h} \n({u})'), col='gray', xlab='Total camas') 
    title(sub=paste('Mediana:', mediana), adj=1, line=2, font=2,cex.sub = 0.75)
    title(sub=paste('Percentil 10:', percentiles[['10%']]), adj=1, line=3, font=2,cex.sub = 0.75)
    title(sub=paste('Percentil 90:', percentiles[['90%']]), adj=1, line=4, font=2,cex.sub = 0.75)
    
    # Plot de número de camas a lo largo de la pandemia
    plot(total_camas ~ fecha_envio, datos, ylim=c(0,max(tot, na.rm=T)+10), xaxt = "n", type = "l", main=glue('{h} \n({u})'), xlab=NA)
    for (d in min.sobrepasado){
      rect(d-1,0-20,
           d+1,max(tot, na.rm=T)+20,
           col= rgb(1,0,0,alpha=0.1), lwd=0)      
    }
    for (d in max.sobrepasado){
      rect(d-1,0-20,
           d+1,max(tot, na.rm=T)+20,
           col= rgb(1,0,0,alpha=0.5), lwd=0)      
    }
    for (d in median.sobrepasado){
      rect(d-1,0-20,
           d+1,max(tot, na.rm=T)+20,
           col= rgb(1,0,0,alpha=0.25), lwd=0)      
    }
    rect(fechas[1]-20,percentiles[['10%']],
         fechas[length(fechas)]+20,percentiles[['90%']],
         col= rgb(0,0,1,alpha=0.05), lwd=0)
    lines(ocupadas_no_covid19 ~ fecha_envio, datos, type="l",lty=1, lwd=1, col='blue')
    lines(ocupadas_covid19 ~ fecha_envio, datos, type="l",lty=1, lwd=1, col='red')
    lines(total_ocupadas ~ fecha_envio, datos, type="l",lty=1, lwd=1, col='green')
    abline(h=mediana, col='darkorchid', lty=1)
    # abline(h=percentiles[['10%']], col='deeppink', lty=5)
    # abline(h=percentiles[['90%']], col='darkslateblue', lty=5)


    axis.Date(1, at=seq(min(fechas), max(fechas), length.out=10), format='%b %Y', las=2, cex.axis=0.8)    

    # legend("topright", legend = c('total_camas','COVID19','NO COVID19'),
    #        col = c('black','red','blue'), lty=c(1,1,1), pch = c(NA,NA,NA), bty = "n")
    # 
    # Meter resultados en la lista
    hospital.capacity.stats[[h]][[u]] <- c(mediana, percentiles[['10%']], percentiles[['90%']])
  }
}
par(mfrow=c(1,1))

# Resultados de cada hospital por unidad
hospital.capacity.stats


# Juntar datos de hospitales para encontrar los stats del conjunto
merge.hospital.data <- function(hosp.data, parameter){
  res <- bind_rows(lapply(hosp.data, function(df){df[parameter,]}))
  res <- apply(res,2,sum)
  res <- as.data.frame(t(res))
  rownames(res) <- parameter
  return(res)
}
df_median <- merge.hospital.data(hospital.capacity.stats, 'mediana')
df_p10 <- merge.hospital.data(hospital.capacity.stats, 'percentil10')
df_p90 <- merge.hospital.data(hospital.capacity.stats, 'percentil90')

area.capacity.stats <- rbind(df_median, df_p10, df_p90)


plot.capacity.intervals <- function(capacity.stats, unidad){
  plot(NA, xlim=c(0,100), ylim=c(0,max(area.capacity.stats[,unidad])+10))
  abline(h=area.capacity.stats['mediana',unidad], col='blue', lty=1)
  abline(h=area.capacity.stats['percentil10',unidad], col='red', lty=2)
  abline(h=area.capacity.stats['percentil90',unidad], col='red', lty=2) 
  rect(0,area.capacity.stats['percentil90',unidad],
       100,area.capacity.stats['percentil10',unidad],
       col= rgb(0,0,1.0,alpha=0.1), lwd=0)
}

plot.capacity.intervals(area.capacity.stats,'Hospitalización convencional')
plot.capacity.intervals(area.capacity.stats,'U. Críticas CON respirador')
plot.capacity.intervals(area.capacity.stats,'U. Críticas SIN respirador')

