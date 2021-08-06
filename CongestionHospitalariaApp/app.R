#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(Rlab)
library(data.table)
library(scales)
library(foreach)
library(doParallel)
library(DT)
library(readr)
library(glue)
library(dplyr)

#  ---- Variables de datos ----
areas.hospitales <- data.frame(read_csv("../datos/areas_hospitales_correspondencia.csv"))
capacidad.org <- data.frame(read_csv("../datos/capacidadasistencial.csv", locale = locale(encoding = "ISO-8859-1"))) # capacidad asistencial
names(capacidad.org) <- tolower(names(capacidad.org))

# ---- Preprocesado de capacidad ----
# Meter áreas sanitarias
capacidad.org <- merge(areas.hospitales, capacidad.org, by='hospital')

# Eliminar columna id (no hace falta)
capacidad.org$id <- NULL

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

#################
# ---- APP ---- #
#################
ui <- fluidPage(
    titlePanel(
        h2(strong("Congestión Hospitalaria"), style = "color:#008080"),
    ),
    hr(),
    sidebarLayout(
        sidebarPanel(
            h4(strong("Variables comunes"), style = "color:#008080"),
            selectInput("area.sanitaria", 
                        strong("Área sanitaria:"), 
                        choices = c("Coruña - Cee", "Ferrol", "Lugo - A Mariña - Monforte de Lemos", 
                                    "Ourense - Verín - O Barco de Valdeorras", "Pontevedra - O Salnés", 
                                    "Santiago de Compostela - Barbanza", "Vigo"),
                        selected = "Coruña - Cee"
            ),
            hr(),
            h4(strong("Variables de simulación"), style = "color:#008080"),
            numericInput("m", strong("Simulaciones:"),
                        min = 0, max = 2000, value = 100),
            numericInput("n.ind", strong("Individuos:"),
                         min = 0, max = 1000, value = 1000),
            numericInput("n.time", strong("Días:"),
                         min = 0, max = 300, value = 250),
            numericInput("par.m.size", strong("Simulaciones por núcleo:"),
                         min = 0, max = 200, value = 100),
            numericInput("par.m.loops", strong("Tandas de simulación:"),
                         min = 1, max = 20, value = 10),
            fluidRow(
                column(12,actionButton("ejecutar_simulacion","Simulación"),align='center')
            ),

            
            
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Análisis capacidad", 
                                 h3(strong('Análisis de capacidad')),
                                 # textOutput("sel.area.sanitaria"),
                                 plotOutput("analisis", height=800)                                 
                        ),
                        tabPanel("Simulación", 
                                 h3(strong('Simulación'))                           
                        )
            )

        ),
    ),
    
    fluidRow(
        column(
            tabsetPanel(type = "tabs",
                        tabPanel("Capacidades", 
                                 br(),
                                 dataTableOutput("table.capacidades")                             
                        ),
                        tabPanel("Pacientes",
                                 br(),
                                 p('Tabla')
                        )
            ),
        width = 12  )
    )

)

# Define server logic ----
server <- function(input, output) {
    output$sel.area.sanitaria <- renderText({ 
        paste("Lugar:", input$area.sanitaria)
    })
    capacidad.filter <- reactive({
        # Filtrar por el área sanitaria que se quiera
        capacidad <- filter.cap(capacidad.org, input$area.sanitaria)
        # Filtrar por referencia
        capacidad <- subset(capacidad, referencia==1)
        # Añadir total de ocupadas
        capacidad$total_ocupadas <- capacidad$ocupadas_covid19 + capacidad$ocupadas_no_covid19
        capacidad
    })
    analisis.capacidad <- reactive({
        capacidad <- capacidad.filter()
        # Hospitales
        hospitales <- sort(unique(capacidad$hospital))
        
        # Unidades
        unidades <- sort(unique(capacidad$unidad))
        
        # Lista para guardar dataframes de cada hospital con la unidad y cuentas
        hospital.capacity.stats <- list()
        
        par(mfrow=c(4,2))
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
                plot(total_camas ~ fecha_envio, datos, ylim=c(0,max(tot, na.rm=T)+max(tot, na.rm=T)*0.25), xaxt = "n", type = "l", main=glue('{h} \n({u})'), xlab=NA)
                for (d in min.sobrepasado){
                    rect(d-1,0-20,
                         d+1,max(tot, na.rm=T)+50,
                         col= rgb(1,0,0,alpha=0.05), lwd=0)      
                }
                for (d in max.sobrepasado){
                    rect(d-1,0-20,
                         d+1,max(tot, na.rm=T)+50,
                         col= rgb(1,0,0,alpha=0.3), lwd=0)      
                }
                for (d in median.sobrepasado){
                    rect(d-1,0-20,
                         d+1,max(tot, na.rm=T)+20,
                         col= rgb(1,0,0,alpha=0.15), lwd=0)      
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
                legend('topright',legend = c('Total camas','Total ocupadas','Ocupadas por COVID','Ocupadas por no COVID','Mediana total camas'), 
                       col = c("black","green", "red", "blue", "darkorchid"), lwd = 2, xpd = TRUE, cex = 0.5, bty = 'n')
                # legend("topright", legend = c('total_camas','COVID19','NO COVID19'),
                #        col = c('black','red','blue'), lty=c(1,1,1), pch = c(NA,NA,NA), bty = "n")
                # 
                # Meter resultados en la lista
                hospital.capacity.stats[[h]][[u]] <- c(mediana, percentiles[['10%']], percentiles[['90%']])
            }
        }
        par(mfrow=c(1,1))
        
        
    })
    output$table.capacidades <- renderDataTable(capacidad.filter(), options = list(scrollX = TRUE, pageLength = 5))
    output$analisis <- renderPlot(analisis.capacidad())
}

# Run the app ----
shinyApp(ui = ui, server = server)


