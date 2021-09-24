library(ggplot2)
library(plotly)

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

################################################################

plot.interactive.hist <- function(datos, title='Histograma del total de camas', labs=c('Cantidad','Frecuencia')){
  # Histograma
  p <- plot_ly(datos, x = ~total_camas, type = "histogram", name='Frecuencia de camas', nbinsx=20)
  return(ggplotly(p))
}


################################################################

plot.interactive.total.beds <- function(datos, title='Total camas', labs=c('Fecha envío','Camas')){
  # -- Plot de número de camas a lo largo de la pandemia ----
  
  p <- plot_ly(datos, x=~fecha_envio, y=~total_camas, type='scatter', mode='lines', name='Total camas') %>%
    add_trace(y=~ocupadas_covid19, name='Ocupadas COVID19') %>%
    add_trace(y=~ocupadas_no_covid19, name='Ocupadas no COVID19') %>%
    add_trace(y=~total_ocupadas, name='Ocupadas')

  return(p)
}



################################################################

plot.interactive.percent.patients <- function(datos, u, title='', labs=c('Fecha envío','Camas')){
  # ---- Plot de porcentaje de ocupación a lo largo de la pandemia ----
  # Cálculo de porcentajes
  ocupados.covid.pct <- (datos$ocupadas_covid19/datos$total_camas)*100
  ocupados.nocovid.pct <- (datos$ocupadas_no_covid19/datos$total_camas)*100
  ocupados.total.pct <- ((datos$ocupadas_covid19+datos$ocupadas_no_covid19)/datos$total_camas)*100
  df.ocupados.pct <- data.frame(ocupados.covid.pct, ocupados.nocovid.pct, ocupados.total.pct, fecha_envio=datos$fecha_envio)
  
  p <- plot_ly(df.ocupados.pct, x=~fecha_envio, y=~ocupados.covid.pct, type='scatter', mode='lines', name='COVID19', color="firebrick4") %>%
    add_trace(y=~ocupados.nocovid.pct, name='No COVID19', color="dodgerblue3") %>%
    add_trace(y=~ocupados.total.pct, name='Total', color="darkgreen") %>% 
    layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5), 
           yaxis=list(range=c(-10,100), title='Ocupadas (%)'))
  
  # # ---- Dependiendo de la unidad el porcentaje de ocupación es más o menos preocupante ----
  # # https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov/documentos/Actuaciones_respuesta_COVID_26.03.2021.pdf
  # if (u %in% c('Hospitalización convencional')){
  #   risks <- data.frame(x1=c(min(df.ocupados.pct$fecha_envio)),
  #                       x2=c(max(df.ocupados.pct$fecha_envio)),
  #                       y1=c(0,2,5,10,15),
  #                       y2=c(2,5,10,15,100)
  #   )
  #   
  # } else if (u %in% c('U. Críticas CON respirador','U. Críticas SIN respirador')){
  #   risks <- data.frame(x1=c(min(df.ocupados.pct$fecha_envio)),
  #                       x2=c(max(df.ocupados.pct$fecha_envio)),
  #                       y1=c(0,5,10,15,25),
  #                       y2=c(5,10,15,25,100)
  #   )
  # } else {return(NULL)}
  # 
  # # Añadir colores
  # risks$t <- c('green','yellow','orange','red','red')
  # 

  
  return(p)
}


################################################################
# Resultados en conjunto
get.unidades.list <- function(cap, sel.cols = c('ocupados.covid.pct')){
  # Obtiene una lista de listas. 
  # Cada lista pertenece a un unidad, y contiene una lista con un dataframe para cada hospital
  # Estructura:
  # --> Unidad1
  # ----> df.hospital1
  # -------> fecha_envio | ocupados.covid.pct | ocupados.nocovid.pct
  unidades.stat.list <- list()
  unidades <- sort(unique(capacidad$unidad))
  hospitales <- sort(unique(capacidad$hospital))
  for (u in unidades){
    unidades.stat.list[[u]] <- list()
    for (h in hospitales){
      df <- subset(cap, hospital==h & unidad==u)[,c('fecha_envio','ocupadas_covid19','ocupadas_no_covid19','total_camas')]
      
      # Calcular porcentajes
      df$ocupados.covid.pct <- 100 * df[['ocupadas_covid19']] / df[['total_camas']]
      df$ocupados.nocovid.pct <- 100 * df[['ocupadas_no_covid19']] / df[['total_camas']]
      df$ocupados.total.pct <- 100 * (df[['ocupadas_covid19']] + df[['ocupadas_no_covid19']]) / df[['total_camas']]
      
      unidades.stat.list[[u]][[h]] <- df[,c('fecha_envio', sel.cols, 'total_camas', 'ocupadas_covid19', 'ocupadas_no_covid19')]
    }
  }
  return(unidades.stat.list)
}

reduce.unidades.stat.list <- function(l, c){
  # Hace un merge de una lista de data.frames según una llave primaria (by='fecha_envio')
  # y una columna objetivo
  l.filtered <- lapply(l, function(df){df[,c('fecha_envio', c)]}) 
  reduced.unit <- Reduce(function(df1, df2) merge(df1, df2, by='fecha_envio', all.x=T), l.filtered)
  return(reduced.unit)
}

get.sel.stat.function <- function(df, sel.stat.function='median'){
  # Aplica una función sobre un dataframe, el cual su primera columna debe ser un id
  # que no se usa
  if(sel.stat.function=='median'){
    sel.stat <- apply(df[,-1], 1, function(x){median(x,na.rm=T)})
  } else if (sel.stat.function=='mean'){
    sel.stat <- rowMeans(df[,-1], na.rm=T)
  } else if (sel.stat.function=='quantile'){
    sel.stat <- quantile(df[,-1], probs = seq(0, 1, by= 0.1), na.rm=T)
  } else if (sel.stat.function=='sum'){
    sel.stat <- rowSums(df[,-1], na.rm=T)
  }
  return(sel.stat)
}

plot.merged.capacity <- function(cap, sel.cols = c('ocupados.covid.pct'), sel.stat.function = 'median', names=c(NA,NA,NA)){
  # Obtener una lista de listas, cada una con un dataframe correspondiente a 
  # datos de una combinación de hospital-unidad (ej: porcentaje de camas ocupadas por covid)
  # Estructura:
  # --> Unidad1
  # ----> df.hospital1
  # -------> fecha_envio | ocupados.covid.pct | ocupados.nocovid.pct
  unidades.stat.list <- get.unidades.list(cap, sel.cols)
  
  plot.list <- list()
  for(u in names(unidades.stat.list)){
    df.list <- list()
    for(c in sel.cols){
      # Se reducen los dataframes de todos los hospitales en la lista, 
      # con la llave primaria siendo fecha_envio y la columna elegida c,
      # obteniendo un dataframe de varias columnas (fecha_envio, stat_hosp1, stat_hosp2...)
      reduced.unit <- reduce.unidades.stat.list(unidades.stat.list[[u]], c)
      
      # Se reducen las columnas usando la función elegida con sel.stat.function
      # (Ej: mediana de las columnas de interés)
      sel.stat <- get.sel.stat.function(reduced.unit, sel.stat.function)
      
      # Almacenado en una lista
      df.list[[c]] <- data.frame(fecha_envio=reduced.unit[['fecha_envio']], sel.stat)
      
    }
    tmp <- reduce.unidades.stat.list(unidades.stat.list[[u]], 'total_camas')
    percentiles <- get.sel.stat.function(tmp, 'quantile')
    
    per.min <- percentiles[['10%']]
    per.max <- percentiles[['90%']]
    mediana <- percentiles[['50%']]
    
    cat(u, ':', per.min, per.max, mediana, '\n')
    
    # En df.list habrá una lista de dataframes, los cuales reducimos de nuevo según la fecha_envio
    res.df <- reduce.unidades.stat.list(df.list, 'sel.stat')
    # names(res.df) <- c('fecha_envio', sel.cols)
    
    p <- plotly_build(plot_ly(res.df, x = ~fecha_envio, y=~sel.stat.x, type='scatter', mode='lines', name=names[1], color="firebrick4") %>%
                        add_trace(y=~sel.stat.y, name=names[2], color="dodgerblue3") %>%
                        add_trace(y=~sel.stat, name=names[3], color="green") %>%
                        layout(title = glue("{u}\n {sel.cols}"),
                               yaxis = list(range=c(-10,100), title='Ocupadas (%)', hoverformat = ".2f%")))
    
    plot.list[[u]] <- p
    
  }
  return(plot.list)
}