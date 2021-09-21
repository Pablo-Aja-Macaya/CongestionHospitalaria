# Librerias
library(Rlab)
library(data.table)
library(scales)
library(foreach)
library(doParallel)
library(DT)
library(readr)
library(glue)
library(dplyr)
library(zoo)


#  ---- Variables de datos ----
area.sanitaria <- "all" # "Coruña - Cee" # c('Ourense - Verín - O Barco de Valdeorras', 'Coruña - Cee') # si se pone 'all' se eligen todas
hosp.ref <- 1 # qué hospitales se seleccionan (1: referencias, 0: no referencias, 'all': todos)
outlier.filter.type <- 'sliding_median' # tipo de filtro de outliers
window.size <- 5 # para el filtro de outliers si se elige desplazamiento de ventana
areas.hospitales <- data.frame(read_csv("datos/areas_hospitales_correspondencia.csv"))
capacidad <- data.frame(read_csv("datos/capacidadasistencial.csv", locale = locale(encoding = "ISO-8859-1"))) # capacidad asistencial
names(capacidad) <- tolower(names(capacidad))

# Para crear buenas tablas
create.table <- function(df, capt){
  DT::datatable(df, extensions = c('FixedColumns'),
                options = list(scrollX = TRUE, paging=TRUE), 
                caption=htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: center; color: black;',
                  htmltools::em(capt)
                ))
}

# Cargar funciones de gráficas
source('./common/graficas.R')

# ---- Preprocesado de capacidad ----
# Meter áreas sanitarias
capacidad <- merge(areas.hospitales, capacidad, by='hospital')

# Eliminar columna id (no hace falta)
capacidad$id <- NULL

# ---- Filtrado de capacidad ----
filter.cap <- function(df,a){
  # Filtra el dataframe de casos según área sanitaria
  # Si se pone 'all' se cogen todas las áreas
  if (a=='all'){
    cap <- df
  } else {
    cap <- subset(df, area==a)
  }
  return(cap)
}

# Filtrar por el área sanitaria que se quiera
capacidad <- filter.cap(capacidad, area.sanitaria)

# Filtrar por referencia
filter.ref <- function(df, ref){
  if (ref=='all'){ # todos los hospitales
    return(df)
  } else if (ref==1){ # hospitales de referencia
    return(subset(df, referencia==1))
  } else if (ref==0){ # hospitales de no referencia
    return(subset(df, referencia==0))
  }
}
capacidad <- filter.ref(capacidad, hosp.ref)

create.table(capacidad, 'Capacidades')

# ---- Función de filtrado de outliers ----
filter.outliers <- function(df, filter.type, sel.col, h, u, window.size=NA){
  outliers <- c() # inicializar outliers (shiny protesta si no se hace)
  if (is.na(window.size)){
    window.size <- 5
  }
  
  if (is.na(filter.type)){
    return(df)
    
  } else if (filter.type=='boxplot'){
    # -- Método simple (por boxplot) --
    outliers <- boxplot(df[[sel.col]], plot=FALSE)$out
    
  } else if (filter.type=='extended'){
    # -- Método más refinado (boxplot y quedarse con los que no se separen más de un 10% de los valores del Q1 y Q3) --
    outliers <- boxplot(df[[sel.col]], plot=FALSE)$out
    
    # Calcular los límites típicos para que un valor sea considerado outlier
    quantiles <- quantile(df[[sel.col]], probs=c(0.1, 0.9), na.rm = TRUE)
    iqr <- IQR(df[[sel.col]], na.rm=TRUE)
    low <- quantiles[1]-1.5*iqr
    up <-  quantiles[2]+1.5*iqr  
    
    # Calcular el límite extendido (límite + límite*porcentaje)
    extra.pct <- 0.15
    lower <- low-low*extra.pct
    upper <- up+up*extra.pct
    
    # Filtrar los outliers
    cond <- outliers >= lower & outliers <= upper # ver cuáles se encuentran en el rango extendido de lower y upper
    outliers <- outliers[!cond] # quitar los que se encuentran en ese rango
    
  } else if (filter.type=='sliding_median'){
    df[[sel.col]] <- rollapply(df[[sel.col]], width=window.size, FUN=median, align='left', fill=NA, partial=TRUE)
    return(df)
    
  } else if (filter.type=='sliding_mean'){
    df[[sel.col]] <- rollapply(df[[sel.col]], width=window.size, FUN=mean, align='left', fill=NA, partial=TRUE)
    return(df)
  }
  
  # Eliminar outliers
  if (length(outliers)!=0){ # si hay algún outlier
    df <- df[-which(df[[sel.col]] %in% outliers),] # quitar las filas del dataset con los que hayan sido outliers
  }
  
  # print(glue('Se eliminan {length(outliers)} outliers de {sel.col} ({h} - {u})\n'))
  return(df)
  
}



# ---- Capacidad en cada hospital ----
# Quitar la unidad de centro no sanitario porque no aporta nada
capacidad <- subset(capacidad, unidad!='Centros no sanitarios')

# Hospitales
hospitales <- sort(unique(capacidad$hospital))

# Unidades
unidades <- sort(unique(capacidad$unidad))


# Lista para guardar dataframes de cada hospital con la unidad y cuentas
hospital.capacity.stats <- list()



for (h in hospitales){
  plot.tittle.line <- 1
  # Inicialización de dataframe de unidades*medidas para el hospital
  hospital.capacity.stats[[h]] <- data.frame(matrix(ncol = length(unidades), nrow = 3))
  names(hospital.capacity.stats[[h]]) <- unidades
  row.names(hospital.capacity.stats[[h]]) <- c('mediana','percentil10','percentil90')
  for (u in unidades){
    # Total de camas de este hospital por unidad y ordenadas por fecha
    datos <- subset(capacidad, hospital==h & unidad==u)
    datos <- datos[order(datos$fecha_envio),]

    # Añadir total de ocupadas
    datos$total_ocupadas <- datos$ocupadas_covid19 + datos$ocupadas_no_covid19
    
    # Variables repetidas
    tot <- datos$total_camas
    fechas <- datos$fecha_envio
    tot.ocupadas <- datos$total_ocupadas
    
    # ---- Gráficas ----
    # p1.with.outliers <- plot.interactive.hist(datos)
    # p2.with.outliers  <- plot.interactive.total.beds(datos)
    p3.with.outliers  <- plot.interactive.percent.patients(datos,u)
    
    # ---- Filtrar outliers del total de camas ----
    cat('---------------\n')
    datos <- filter.outliers(datos, filter.type=outlier.filter.type, sel.col='total_camas', h, u, window.size = window.size)
    datos <- filter.outliers(datos, filter.type=outlier.filter.type, sel.col='ocupadas_covid19', h, u, window.size = window.size)
    datos <- filter.outliers(datos, filter.type=outlier.filter.type, sel.col='ocupadas_no_covid19', h, u, window.size = window.size)
    
    # ---- Estadísticas -----
    # Mediana y percentiles
    mediana <- median(tot, na.rm=T)
    percentiles <- quantile(tot, probs = seq(0, 1, by= 0.1), na.rm=T)
    
    # Calcular en qué puntos se vería sobrepasado el hospital
    min.sobrepasado <- fechas[which(tot.ocupadas>percentiles[['10%']])]
    max.sobrepasado <- fechas[which(tot.ocupadas>percentiles[['90%']])]
    median.sobrepasado <- fechas[which(tot.ocupadas>mediana)]

    # ---- Gráficas ----
    p1.without.outliers <- plot.interactive.hist(datos)
    p2.without.outliers  <- plot.interactive.total.beds(datos)
    p3.without.outliers  <- plot.interactive.percent.patients(datos,u)
    
    ################################################################
    # ---- Análisis de capacidad junto ----
    annotations = list( 
      list( 
        x = 0.2,  
        y = 1.0,  
        text = "Histograma del total de camas",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      ),  
      list( 
        x = 0.8,  
        y = 1,  
        text = "Porcentaje de ocupación",  
        xref = "paper",  
        yref = "paper",  
        xanchor = "center",  
        yanchor = "bottom",  
        showarrow = FALSE 
      )
    )
    plot.title <- glue("<b>{h}</b>\n{u}")
    p <- subplot(p1.without.outliers, p3.without.outliers, margin = 0.04) %>% 
      layout(title = list(text=plot.title, font=list(size=15)),
             margin = list(l=20, r=20, b=20, t=120),
             annotations = annotations, hovermode = "x unified",
             yaxis=list(anchor="x", hoverformat = ".2f")
      )
    print(p)
    
    ################################################################
    # # ---- Comparación de outliers vs no outliers ----

    annotations = list(
      list(
        x = 0.23,
        y = 1.0,
        text = "Con outliers",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      ),
      list(
        x = 0.77,
        y = 1,
        text = "Datos tratados",
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE
      )
    )
    plot.title <- glue("<b>{h}</b>\n{u}")
    p <- subplot(style(p3.with.outliers, showlegend=F), p3.without.outliers, margin = 0.04, shareY= TRUE) %>%
              layout(title = glue("<b>{h}</b>\n{u}"),
                     titlefont=list(text=plot.title, font=list(size=15)),
                     margin = list(l=20, r=20, b=20, t=120),
                     annotations = annotations, hovermode = "x unified",
                     yaxis=list(anchor="x", hoverformat = ".2f")
                     )
    print(p)
    
    # Meter resultados en la lista
    hospital.capacity.stats[[h]][[u]] <- c(mediana, percentiles[['10%']], percentiles[['90%']])
  }
  
}

# ---- Resultados de cada hospital por unidad ----
hospital.capacity.stats

# ---- Juntar datos de hospitales para encontrar stats del conjunto ----
merge.hospital.data <- function(hosp.data, parameter){
  # Esta función coge una lista de dataframes, cada uno de un hospital, en donde
  # cada fila es una característica de la capacidad (ej: mediana) y cada columna la 
  # unidad del hospital. Se hace la suma de esta característica a lo largo de los hospitales
  res <- bind_rows(lapply(hosp.data, function(df){df[parameter,]}))
  res <- apply(res,2,sum)
  res <- as.data.frame(t(res))
  rownames(res) <- parameter
  return(res)
}
# Se calcula para cada característica, su suma a lo largo de los hospitales
df_median <- merge.hospital.data(hospital.capacity.stats, 'mediana')
df_p10 <- merge.hospital.data(hospital.capacity.stats, 'percentil10')
df_p90 <- merge.hospital.data(hospital.capacity.stats, 'percentil90')

# Se juntan en un df, resumiendo la capacidad del área seleccionada
area.capacity.stats <- rbind(df_median, df_p10, df_p90)
create.table(area.capacity.stats, capt='Capacidad del conjunto seleccionado')

# ---- Mostrar capacidad del área en cada unidad ----
plot.capacity.intervals <- function(capacity.stats, unidad){
  mediana <- capacity.stats['mediana',unidad]
  p10 <- capacity.stats['percentil10',unidad]
  p90 <- capacity.stats['percentil90',unidad]
  
  plot(NA, xlim=c(0,100), ylim=c(0,max(capacity.stats[,unidad])+10), main=unidad, ylab='Camas')
  abline(h=p10, col='red', lty=2)
  abline(h=p90, col='red', lty=2) 
  abline(h=mediana, col='blue', lty=1)
  rect(0-50,p90,
       100+50,p10,
       col= rgb(0,0,1.0,alpha=0.1), lwd=0)
  title(sub=paste('Mediana:', mediana), adj=1, line=2, font=2,cex.sub = 0.75)
  title(sub=paste('Percentil 10:', p10), adj=1, line=3, font=2,cex.sub = 0.75)
  title(sub=paste('Percentil 90:', p90), adj=1, line=4, font=2,cex.sub = 0.75)
}

par(mfrow=c(1,3))
plot.capacity.intervals(area.capacity.stats,'Hospitalización convencional')
plot.capacity.intervals(area.capacity.stats,'U. Críticas CON respirador')
plot.capacity.intervals(area.capacity.stats,'U. Críticas SIN respirador')
par(mfrow=c(1,1))

















# # -- Histograma ----
# hist(tot, breaks = 30, main=NA, col='gray', xlab='Total camas') 
# title(sub=paste('Mediana:', mediana), adj=1, line=2, font=2,cex.sub = 0.75)
# title(sub=paste('Percentil 10:', percentiles[['10%']]), adj=1, line=3, font=2,cex.sub = 0.75)
# title(sub=paste('Percentil 90:', percentiles[['90%']]), adj=1, line=4, font=2,cex.sub = 0.75)
# title("Histograma de camas", line = plot.tittle.line)
# 
# # -- Plot de número de camas a lo largo de la pandemia ----
# plot(total_camas ~ fecha_envio, datos, ylim=c(0,max(tot, na.rm=T)+max(tot, na.rm=T)*0.25), xaxt = "n", type = "l", main=NA, xlab=NA, ylab='Camas')
# # Columnas rojas (días donde se sobrepasa una de las estadísticas)
# # Cuanto más rojizas más gravedad
# for (d in min.sobrepasado){
#   rect(d-1, 0-20,
#        d+1, max(tot, na.rm=T)+80,
#        col= rgb(1,0,0,alpha=0.05), lwd=0)      
# }
# for (d in max.sobrepasado){
#   rect(d-1, 0-20,
#        d+1, max(tot, na.rm=T)+80,
#        col= rgb(1,0,0,alpha=0.3), lwd=0)      
# }
# for (d in median.sobrepasado){
#   rect(d-1, 0-20,
#        d+1, max(tot, na.rm=T)+80,
#        col= rgb(1,0,0,alpha=0.15), lwd=0)      
# }
# # Área entre percentil10 y percentil90
# rect(fechas[1]-20,percentiles[['10%']],
#      fechas[length(fechas)]+20,percentiles[['90%']],
#      col= rgb(0,0,1,alpha=0.05), lwd=0)
# # Datos de ocupación
# lines(ocupadas_no_covid19 ~ fecha_envio, datos, type="l",lty=1, lwd=1, col='blue')
# lines(ocupadas_covid19 ~ fecha_envio, datos, type="l",lty=1, lwd=1, col='red')
# lines(total_ocupadas ~ fecha_envio, datos, type="l",lty=1, lwd=1, col='green')
# abline(h=mediana, col='darkorchid', lty=1)
# # abline(h=percentiles[['10%']], col='deeppink', lty=5)
# # abline(h=percentiles[['90%']], col='darkslateblue', lty=5)
# 
# axis.Date(1, at=seq(min(fechas), max(fechas), length.out=10), format='%b %Y', las=2, cex.axis=0.8)    
# legend('topright',legend = c('Total camas','Total ocupadas','Ocupadas por COVID','Ocupadas por no COVID','Mediana total camas'), 
#        col = c("black","green", "red", "blue", "darkorchid"), lwd = 2, xpd = TRUE, cex = 0.5, bty = 'n')
# 
# title("Total de camas", line = plot.tittle.line)
# 
# # -- Plot de porcentaje de ocupación a lo largo de la pandemia ----
# ocupados.covid.pct <- (datos$ocupadas_covid19/datos$total_camas)*100
# ocupados.nocovid.pct <- (datos$ocupadas_no_covid19/datos$total_camas)*100
# ocupados.total.pct <- ((datos$ocupadas_covid19+datos$ocupadas_no_covid19)/datos$total_camas)*100
# df.ocupados.pct <- data.frame(ocupados.covid.pct, ocupados.nocovid.pct, ocupados.total.pct, fecha_envio=datos$fecha_envio)
# 
# plot(ocupados.covid.pct ~ fecha_envio, df.ocupados.pct, ylim=c(0,100), xaxt = "n", type = "l", main=NA, xlab=NA, ylab='Ocupación (%)', col='red')
# 
# add.risk.scale = function(u){
#   # https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov/documentos/Actuaciones_respuesta_COVID_26.03.2021.pdf
#   # Dependiendo de la unidad el porcentaje de ocupación es más o menos preocupante
#   if (u %in% c('Hospitalización convencional')){
#     risks <- data.frame(nueva.normalidad = c(-10,2), bajo = c(2,5),
#                         medio = c(5,10), alto = c(10,15), muy.alto = c(15,110))
#   } else if (u %in% c('U. Críticas CON respirador','U. Críticas SIN respirador')){
#     risks <- data.frame(nueva.normalidad = c(0,5), bajo = c(5,10),
#                         medio = c(10,15), alto = c(15,25), muy.alto = c(25,110))
#   } else {return(NULL)}
#   # Se añade el color a cada nivel
#   risk.alpha <- 0.1
#   risks <- rbind(risks, c(rgb(0,1,0,alpha=risk.alpha), rgb(1,1,0,alpha=risk.alpha),
#                           rgb(1,0.7,0,alpha=risk.alpha), rgb(1,0,1,alpha=risk.alpha),
#                           rgb(1,0,0,alpha=risk.alpha)))
#   # Dibujo de áreas de riesgo en el color correspondiente
#   apply(risks, 2, function(l){
#     rect(fechas[1]-50, l[1],
#          fechas[length(fechas)]+50, l[2],
#          col= l[3], lwd=0.08)   
#     
#   })
#   # Datos se ponen ahora para pisar los cuadros de color
#   lines(ocupados.covid.pct ~ fecha_envio, df.ocupados.pct, type="l",lty=1, lwd=1, col='red')
#   lines(ocupados.nocovid.pct ~ fecha_envio, df.ocupados.pct, type="l",lty=1, lwd=1, col='blue')
#   lines(ocupados.total.pct ~ fecha_envio, df.ocupados.pct, type="l",lty=1, lwd=1, col='green')
#   
# }
# add.risk.scale(u)
# 
# axis.Date(1, at=seq(min(fechas), max(fechas), length.out=10), format='%b %Y', las=2, cex.axis=0.8)   
# 
# legend('topright',legend = c('Total ocupadas','Ocupadas por COVID','Ocupadas por no COVID'), 
#        col = c("green", "red", "blue"), lwd = 2, xpd = TRUE, cex = 1, bty = 'n')
# 
# title("Porcentaje de ocupación", line = plot.tittle.line)
# 
# title(glue('{h} \n({u})'), line = -3, outer = TRUE) # título general (hospital y unidad)
