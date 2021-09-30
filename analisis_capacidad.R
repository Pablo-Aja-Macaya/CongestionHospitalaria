###########################################################
# ---------------- Análisis capacidad ------------------- #
###########################################################

# Este script contiene el procesado y análisis de la capacidad asistencial de cada hospital en un conjunto seleccionado

# A tener en cuenta: Es llamado por sim_datos_agregados.R, de tal manera que hay
# ciertas variables en este script que deben comentarse si se usa sim_datos_agregados.R, como
# area.sanitaria, hosp.ref, outlier.filter.type y window.size
# Esto es un compromiso necesario para poder ejecutar analisis.capacidad.R por separado

# Más información disponible en el GitHub del repositorio: https://github.com/Pablo-Aja-Macaya/CongestionHospitalaria

###########################################################

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
library(plotly)

#  ---- Variables de datos ----
area.sanitaria <- 'Vigo' # "Coruña - Cee" # c('Ourense - Verín - O Barco de Valdeorras', 'Coruña - Cee') # si se pone 'all' se eligen todas
hosp.ref <- 1 # qué hospitales se seleccionan (1: referencias, 0: no referencias, 'all': todos)
outlier.filter.type <- 'sliding_median' # tipo de filtro de outliers
window.size <- 5 # para el filtro de outliers si se elige desplazamiento de ventana
areas.hospitales <- data.frame(read_csv("datos/areas_hospitales_correspondencia.csv"))
capacidad <- data.frame(read_csv("datos/capacidadasistencial.csv", locale = locale(encoding = "ISO-8859-1"))) # capacidad asistencial
names(capacidad) <- tolower(names(capacidad))

# Para crear buenas tablas
source('./common/create_table.R')

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

create_table(capacidad, 'Capacidades')

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
    # ---- Análisis de capacidad individual ----
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
    
    # ---- Meter resultados en la lista ----
    hospital.capacity.stats[[h]][[u]] <- c(mediana, percentiles[['10%']], percentiles[['90%']])
  }
  
}

# ---- Resultados en conjunto ----
res <- plot.merged.capacity(capacidad, c('ocupados.covid.pct','ocupados.nocovid.pct','ocupados.total.pct'), 'mean', c("COVID19", "No COVID19", "Total"), outlier.filter.type, window.size)
plots <- res$plots

plot.title <- "<b>Porcentaje de camas ocupadas por unidad en el conjunto seleccionado</b>"
{ # Anotaciones para gráficas (posición de títulos y textos)
  annotations = list(
    list(
      x = 0.5,
      y = 1.0,
      text = unidades[1],
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE
    ),
    list(
      x = 0.5,
      y = 0.65,
      text = unidades[2],
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE
    ),
    list(
      x = 0.5,
      y = 0.3,
      text = unidades[3],
      xref = "paper",
      yref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      showarrow = FALSE
    )
  )
}
subplot(plots, margin = 0.04, nrows = 3, shareX=TRUE) %>%
  layout(title = list(text=plot.title, font=list(size=15)),
         margin = list(l=20, r=20, b=20, t=100),
         annotations = annotations, hovermode = "x unified",
         legend = list(orientation = 'h', xanchor = "center", x = 0.5),
         showlegend = F)


# # ---- Resultados de cada hospital por unidad ----
# hospital.capacity.stats

# # ---- Juntar datos de hospitales para encontrar stats del conjunto ----
# merge.hospital.data <- function(hosp.data, parameter){
#   # Esta función coge una lista de dataframes, cada uno de un hospital, en donde
#   # cada fila es una característica de la capacidad (ej: mediana) y cada columna la 
#   # unidad del hospital. Se hace la suma de esta característica a lo largo de los hospitales
#   res <- bind_rows(lapply(hosp.data, function(df){df[parameter,]}))
#   res <- apply(res,2,sum)
#   res <- as.data.frame(t(res))
#   rownames(res) <- parameter
#   return(res)
# }
# # Se calcula para cada característica, su suma a lo largo de los hospitales
# df_median <- merge.hospital.data(hospital.capacity.stats, 'mediana')
# df_p10 <- merge.hospital.data(hospital.capacity.stats, 'percentil10')
# df_p90 <- merge.hospital.data(hospital.capacity.stats, 'percentil90')
# 
# # Se juntan en un df, resumiendo la capacidad del área seleccionada
# area.capacity.stats <- rbind(df_median, df_p10, df_p90)
# create_table(area.capacity.stats, capt='Capacidad del conjunto seleccionado')

# # ---- Mostrar capacidad del área en cada unidad ----
# plot.capacity.intervals <- function(capacity.stats, unidad){
#   mediana <- capacity.stats['mediana',unidad]
#   p10 <- capacity.stats['percentil10',unidad]
#   p90 <- capacity.stats['percentil90',unidad]
#   
#   plot(NA, xlim=c(0,100), ylim=c(0,max(capacity.stats[,unidad])+10), main=unidad, ylab='Camas')
#   abline(h=p10, col='red', lty=2)
#   abline(h=p90, col='red', lty=2) 
#   abline(h=mediana, col='blue', lty=1)
#   rect(0-50,p90,
#        100+50,p10,
#        col= rgb(0,0,1.0,alpha=0.1), lwd=0)
#   title(sub=paste('Mediana:', mediana), adj=1, line=2, font=2,cex.sub = 0.75)
#   title(sub=paste('Percentil 10:', p10), adj=1, line=3, font=2,cex.sub = 0.75)
#   title(sub=paste('Percentil 90:', p90), adj=1, line=4, font=2,cex.sub = 0.75)
# }
# 
# par(mfrow=c(1,3))
# plot.capacity.intervals(area.capacity.stats,'Hospitalización convencional')
# plot.capacity.intervals(area.capacity.stats,'U. Críticas CON respirador')
# plot.capacity.intervals(area.capacity.stats,'U. Críticas SIN respirador')
# par(mfrow=c(1,1))
