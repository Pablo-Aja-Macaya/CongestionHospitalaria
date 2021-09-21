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
  p <- plot_ly(datos, x = ~total_camas, type = "histogram", name='Camas', nbinsx=20)
  
  # p <- ggplot(datos, aes(x=total_camas)) + 
  #   geom_histogram(binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  #   ggtitle(title) + labs(x = labs[1], y= labs[2]) +
  #   theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) 
  # 
  return(ggplotly(p))
}


################################################################

plot.interactive.total.beds <- function(datos, title='Total camas', labs=c('Fecha envío','Camas')){
  # -- Plot de número de camas a lo largo de la pandemia ----
  
  p <- plot_ly(datos, x=~fecha_envio, y=~total_camas, type='scatter', mode='lines', name='Total camas') %>%
    add_trace(y=~ocupadas_covid19, name='Ocupadas COVID19') %>%
    add_trace(y=~ocupadas_no_covid19, name='Ocupadas no COVID19') %>%
    add_trace(y=~total_ocupadas, name='Ocupadas')
  
  
  # p <- ggplot(datos, aes(x=fecha_envio, y=total_camas)) + geom_line(color="black") +
  #   geom_line(aes(x=fecha_envio, y=ocupadas_covid19), color="#E76F51") + 
  #   geom_line(aes(x=fecha_envio, y=ocupadas_no_covid19), color="#E9C46A") +
  #   geom_line(aes(x=fecha_envio, y=total_ocupadas), color="#2A9D8F") + 
  #   geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), linetype=3, col='darkgray', size=0.25) +
  #   # geom_hline(yintercept = mediana, linetype=3, col='darkorchid', size=0.25) +
  #   ggtitle(title) + labs(x = labs[1], y= labs[2]) +
  #   theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) 
  # 
  # 
  # p <- ggplotly(p, tooltip="y") %>% 
  #   style(hoverlabel = label) %>%
  #   layout(hovermode = "x unified")
  
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
  
  p <- plot_ly(df.ocupados.pct, x=~fecha_envio, y=~ocupados.covid.pct, type='scatter', mode='lines', name='Ocupadas COVID19', color="firebrick4") %>%
    add_trace(y=~ocupados.nocovid.pct, name='Ocupadas no COVID19', color="dodgerblue3") %>%
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
  # # ---- Gráfica ----
  # p <- ggplot(df.ocupados.pct, aes(x=fecha_envio, y=ocupados.covid.pct)) + geom_line(color="#E76F51") +
  #   geom_line(aes(x=fecha_envio, y=ocupados.nocovid.pct), color="#E9C46A") +
  #   geom_line(aes(x=fecha_envio, y=ocupados.total.pct), color="#2A9D8F") + 
  #   geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), linetype=3, col='darkgray', size=0.25) +
  #   ggtitle(title) + labs(x = labs[1], y= labs[2]) +
  #   annotate("rect", xmin = risks$x1, xmax = risks$x2, ymin = risks$y1, ymax = risks$y2,
  #            alpha = .1, fill = risks$t) +
  #   ylim(0,100) + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) 
  # 
  # 
  # p <- ggplotly(p, tooltip="y") %>% 
  #   style(hoverlabel = label) %>%
  #   layout(hovermode = "x unified")
  
  return(p)
}

# 
# annotations = list( 
#   list( 
#     x = 0.2,  
#     y = 1.0,  
#     text = "Histograma del total de camas",  
#     xref = "paper",  
#     yref = "paper",  
#     xanchor = "center",  
#     yanchor = "bottom",  
#     showarrow = FALSE 
#   ),  
#   list( 
#     x = 0.8,  
#     y = 1,  
#     text = "Porcentaje de ocupación",  
#     xref = "paper",  
#     yref = "paper",  
#     xanchor = "center",  
#     yanchor = "bottom",  
#     showarrow = FALSE 
#  # annotations = list( 
#   list( 
#     x = 0.2,  
#     y = 1.0,  
#     text = "Histograma del total de camas",  
#     xref = "paper",  
#     yref = "paper",  
#     xanchor = "center",  
#     yanchor = "bottom",  
#     showarrow = FALSE 
#   ),  
#   list( 
#     x = 0.8,  
#     y = 1,  
#     text = "Porcentaje de ocupación",  
#     xref = "paper",  
#     yref = "paper",  
#     xanchor = "center",  
#     yanchor = "bottom",  
#     showarrow = FALSE 
#   )
# )
# 
# # Histograma
# p1 <- plot_ly(datos, x = ~total_camas, type = "histogram", name='Camas')
# 
# 
# # Capacidad
# p2 <- plot_ly(datos, x=~fecha_envio, y=~total_camas, type='scatter', mode='lines', name='Total camas') %>%
#   add_trace(y=~ocupadas_covid19, name='Ocupadas COVID19') %>%
#   add_trace(y=~ocupadas_no_covid19, name='Ocupadas no COVID19') %>%
#   add_trace(y=~total_ocupadas, name='Ocupadas')
# p2
# 
# 
# # Cálculo de porcentajes
# ocupados.covid.pct <- (datos$ocupadas_covid19/datos$total_camas)*100
# ocupados.nocovid.pct <- (datos$ocupadas_no_covid19/datos$total_camas)*100
# ocupados.total.pct <- ((datos$ocupadas_covid19+datos$ocupadas_no_covid19)/datos$total_camas)*100
# df.ocupados.pct <- data.frame(ocupados.covid.pct, ocupados.nocovid.pct, ocupados.total.pct, fecha_envio=datos$fecha_envio)
# 
# p3 <- plot_ly(df.ocupados.pct, x=~fecha_envio, y=~ocupados.covid.pct, type='scatter', mode='lines', name='Ocupadas COVID19') %>%
#   add_trace(y=~ocupados.nocovid.pct, name='Ocupadas no COVID19') %>%
#   add_trace(y=~ocupados.total.pct, name='Total')%>% layout(legend = list(orientation = 'h'))
# 
# p3
# 



