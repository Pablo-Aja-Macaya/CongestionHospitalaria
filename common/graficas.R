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
