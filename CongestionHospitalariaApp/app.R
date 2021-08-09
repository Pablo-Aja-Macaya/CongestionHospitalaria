#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(shinyjs)
library(shinycssloaders)
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
casos.org <- data.frame(read_csv("../datos/sivies_agreg_area_sanitaria.csv")) # casos base
capacidad.org <- data.frame(read_csv("../datos/capacidadasistencial.csv", locale = locale(encoding = "ISO-8859-1"))) # capacidad asistencial
names(capacidad.org) <- tolower(names(capacidad.org))

#########################
# ------- Casos ------- #
#########################
filter.cases <- function(df,area){
    # Filtra el dataframe de casos según área sanitaria
    # Si se pone 'all' se cogen todas las áreas
    if (area=='all'){
        casos <- df
    } else {
        casos <- subset(df, area_sanitaria==area)
    }
    return(casos)
}

get.group.total <- function(df, s){
    # Coge un dataframe y el sexo objetivo y saca el total de individuos para cada rango de edad
    grupos <- unique(subset(df, sexo==s & grupo_edad!='')$grupo_edad)
    grupos <- grupos[order(grupos)] # ordenar
    l <- list()
    for (g in grupos){
        tot <- sum(subset(df, sexo==s & grupo_edad==g)$cantidad)
        l[[g]] <- tot
    }
    return(unlist(l))
}

get.mean.age <- function(interval.vector){
    # Coge un vector como c([10,20), [20,30)) y lo transforma en c(15, 25)
    tmp <- gsub("\\[|)", "", names(interval.vector))
    rango.edad <- strsplit(tmp, ',')
    age.interval <- unlist(lapply(strsplit(tmp, '-'), function(x){mean(as.numeric(x), na.rm=T)}))
    age.interval <- age.interval[!is.na(age.interval)]
    return(age.interval)
}

# Quitar grupos de edad nulos
casos.org <- subset(casos.org, grupo_edad!='NULL')


#########################
# ----- Capacidad ----- #
#########################

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
    shinyjs::useShinyjs(),
    titlePanel(
        h2(strong("Congestión Hospitalaria"), style = "color:#008080"),
    ),
    br(),
    # ---- Variables y resultados ----
    sidebarLayout(
        sidebarPanel(
            # ---- Botón de simulación ----
            fluidRow(
                column(12,actionButton("ejecutar_simulacion","Simulación"),align='center')
            ),
            # ---- Variables comunes ----
            h4(strong("Variables comunes"), style = "color:#008080"),
            selectInput("area.sanitaria", 
                        strong("Área sanitaria:"), 
                        choices = c("Coruña - Cee", "Ferrol", "Lugo - A Mariña - Monforte de Lemos", 
                                    "Ourense - Verín - O Barco de Valdeorras", "Pontevedra - O Salnés", 
                                    "Santiago de Compostela - Barbanza", "Vigo"),
                        selected = "Coruña - Cee"
            ),
            hr(),
            # ---- Variables de simulación ----
            h4(strong("Variables de simulación"), style = "color:#008080"),
            fluidRow(
                column(6,
                       numericInput("num.cores", strong("Número de hilos:"),
                                    min = 0, max = 6, value = 4),                     
                ),
                column(6,
                       numericInput("m", strong("Simulaciones:"),
                                    min = 0, max = 2000, value = 100),                     
                ),
            ),
            fluidRow(
                column(6,
                       numericInput("n.ind", strong("Individuos:"),
                                    min = 0, max = 1000, value = 1000),                  
                ),
                column(6,
                       numericInput("n.time", strong("Días:"),
                                    min = 0, max = 300, value = 250),                  
                ),
            ),
            fluidRow(
                column(6,
                       numericInput("par.m.size", strong("Simulaciones por núcleo:"),
                                    min = 0, max = 200, value = 10),                     
                ),
                column(6,
                       numericInput("par.m.loops", strong("Tandas de simulación:"),
                                    min = 1, max = 20, value = 10),                     
                ),
            ),
            hr(),
            
            # ---- Probabilidades ----
            h4(strong("Probabilidades iniciales"), style = "color:#008080"),
            fluidRow(
                column(6,
                       sliderInput("prob.ICU", strong("Ingresar en UCI:"),
                                   min = 0, max = 1, value = 0.5, ticks=F),                       
                ),
                column(6,
                       sliderInput("prob.HW", strong("Ingresar en hospital:"),
                                   min = 0, max = 1, value = 0.5, ticks=F),                       
                ),
            ),
        
            h4(strong("Probabilidades desde hospital"), style = "color:#008080"),
            fluidRow(
                column(4,
                       sliderInput("prob.HW.death", strong("Muerte:"),
                                   min = 0, max = 1, value = 0.5, ticks=F),                     
                ),
                column(4,
                       sliderInput("prob.HW.ICU", strong("Ir a UCI:"),
                                   min = 0, max = 1, value = 0.5, ticks=F),                      
                ),
                column(4,
                       sliderInput("prob.HW.disc", strong("Discharge:"),
                                   min = 0, max = 1, value = 0.5, ticks=F),                    
                ),
            ),

            h4(strong("Probabilidades desde UCI"), style = "color:#008080"),
            fluidRow(
                column(6,
                       sliderInput("prob.ICU.death", strong("Muerte:"),
                                   min = 0, max = 1, value = 0.5, ticks=F),                     
                ),
                column(6,
                       sliderInput("prob.ICU.HW", strong("Ir a hospital:"),
                                   min = 0, max = 1, value = 0.5, ticks=F),                     
                ),
            ),

            hr(),
            
            # ---- Proporciones muestrales ----
            h4(strong("Variables comunes"), style = "color:#008080"),
            dataTableOutput("table.prob.resumen"),
            numericInput('prob.w',
                         strong('Probabilidad de ser mujer (prob.w):'),
                         min = 0, max = 1, value = 0),
            numericInput('prob.m',
                         strong('Probabilidad de ser hombre (prob.m):'),
                         min = 0, max = 1, value = 0),
            numericInput('prob.rc.real',
                         strong('Proporciones muestrales de hospitalizados (prob.rc.real):'),
                         min = 0, max = 1, value = 0),
            hr(),

            
        ),
        # ---- Apartados ----
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Análisis capacidad", 
                                 h3(strong('Análisis de capacidad')),
                                 plotOutput("analisis", height=800) %>% withSpinner()
                                 
                        ),
                        tabPanel("Simulación", 
                                 h3(strong('Simulación')),
                                 plotOutput("res.condicional") %>% withSpinner()
                        )
            )

        ),
    ),
    # ---- Tablas ----
    
    fluidRow(
        column(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Capacidades",
                                 br(),
                                 dataTableOutput("table.capacidades")                             
                        ),
                        tabPanel("Casos",
                                 br(),
                                 dataTableOutput("table.casos") 
                        ),
                        tabPanel("Hospitalizados",
                                 br(),
                                 dataTableOutput("table.hospitalizados") 
                        )
            ),
        width = 12  )
    )

)


# Define server logic ----
server <- function(input, output, session) {

    ##############################################################
    # ---- Casos y hospitalizados ----
    casos.filter <- reactive({
        # Casos elegidos
        casos <- filter.cases(casos.org, input$area.sanitaria)
        casos
    })
    get.hospitalizados <- reactive({
        # Hospitalizados en estos casos
        hospitalizados <- subset(casos.filter(), estado=='HOS')   
        hospitalizados
    })

    # ---- Proporciones de casos ----
    proporciones <- reactiveValues()
    observe({
        # -- Proporciones de casos -- #
        women.age.interval <- get.group.total(casos.filter(), 'M')
        men.age.interval <- get.group.total(casos.filter(), 'H')
        
        # Calculo de la media de cada intervalo de edad (se usa en la simulación) Ej: Transforma "[20,30)" a 25
        # PELIGRO: si en los datos de input no se representan todos los grupos en ambos sexos
        # la simulación podría estar mal equilibrada (ej: si sólo hay hombres de +70 y mujeres de -50
        # la simulación no creará individuos hombres de 50 años)
        proporciones$women.mean.age.interval <- get.mean.age(women.age.interval)
        proporciones$men.mean.age.interval <- get.mean.age(men.age.interval)
        
        # Individuos totales por sexo
        total.m <- sum(men.age.interval)
        total.w <- sum(women.age.interval)
        total <- total.w + total.m
        
        # Probabilidad de pertencer a cada sexo
        proporciones$prob.w <- total.w/total
        proporciones$prob.m <- total.m/total
        
        # Probabilidad de pertenecer a cada rango de edad
        proporciones$woman.age.prob <- women.age.interval/total.w
        proporciones$man.age.prob <- men.age.interval/total.m
        
        # -- Proporciones de hospitalizados -- #
        # Hospitalizados por rango de edad y sexo
        women.age.hosp.interval <-  get.group.total(get.hospitalizados(), 'M')
        men.age.hosp.interval <- get.group.total(get.hospitalizados(), 'H')
        
        # Total de hospitalizados por sexo
        total.w.hosp <- sum(women.age.hosp.interval)
        total.m.hosp <- sum(men.age.hosp.interval)
        total.hosp <- total.w.hosp + total.m.hosp
        
        # Probabilidad de pertenecer a cada sexo en hospitalizados
        prob.w.hosp <- total.w.hosp/total.hosp
        prob.m.hosp <- total.m.hosp/total.hosp
        
        # Probabilidad de ser hospitalizado por sexo
        woman.age.prob.hosp <- women.age.hosp.interval/total.w.hosp
        man.age.prob.hosp <- men.age.hosp.interval/total.m.hosp
        
        # Proporciones muestrales de hospitalizados por sexo
        proporciones$prob.rc.woman <- women.age.hosp.interval/women.age.interval
        proporciones$prob.rc.man <- men.age.hosp.interval/men.age.interval
        
        # Proporciones muestrales de hospitalizados:
        proporciones$prob.rc.real <- ( total.hosp ) / ( total )
        
        # Tabla resumen
        # t.prueba <- bind_cols(lapply(list(proporciones$woman.age.prob,
        #                                   proporciones$man.age.prob,
        #                                   proporciones$prob.rc.woman,
        #                                   proporciones$prob.rc.man), function(x){data.frame(round(x,4))}))
        # names(t.prueba) <- c('woman.age.prob', 'man.age.prob', 'prob.rc.woman', 'prob.rc.man')
        # output$table.prob.resumen <- renderDataTable(t.prueba, options = list(scrollX = TRUE, pageLength = 5))

        # Outputs
        updateNumericInput(session, "prob.w", value = proporciones$prob.w)
        updateNumericInput(session, "prob.m", value = proporciones$prob.m)
        updateNumericInput(session, "prob.rc.real", value = proporciones$prob.rc.real)
        shinyjs::disable("prob.w")
        shinyjs::disable("prob.m")
        shinyjs::disable("prob.rc.real")
        
    }, priority=5)
    
    ##############################################################
    # ---- Capacidad ----
    capacidades <- reactiveValues()
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
        capacidades$hospital.capacity.stats <- hospital.capacity.stats
        
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
        df_median <- merge.hospital.data(capacidades$hospital.capacity.stats, 'mediana')
        df_p10 <- merge.hospital.data(capacidades$hospital.capacity.stats, 'percentil10')
        df_p90 <- merge.hospital.data(capacidades$hospital.capacity.stats, 'percentil90')
        
        # Se juntan en un df, resumiendo la capacidad del área seleccionada
        area.capacity.stats <- rbind(df_median, df_p10, df_p90)
        capacidades$area.capacity.stats <- area.capacity.stats
    })
    output$analisis <- renderPlot(analisis.capacidad())
    
    ##############################################################
    # ---- Probabilidades de simulación ----
    probabilidades <- reactiveValues()
    observe({
        # -- Paciente es admitido en hospital -- #
        # Probabilidad de ir directamente a UCI
        probabilidades$prob.ICU <- input$prob.ICU
        # Probabilidad de quedarse en hospital ward (HW) primero
        probabilidades$prob.HW <- input$prob.HW
        
        # -- Opciones en HW -- #
        # Probabilidad de morir antes de pasar a UCI
        probabilidades$prob.HW.death <- input$prob.HW.death
        # Probabilidad de entrar en UCI
        probabilidades$prob.HW.ICU <- input$prob.HW.ICU
        # Probabilidad de irse de HW sin entrar en UCI 
        probabilidades$prob.HW.disc <- input$prob.HW.disc
        
        # -- Opciones en UCI-- #
        # Probabilidad de morir tras ser admitido en UCI
        probabilidades$prob.ICU.death <- input$prob.ICU.death
        
        # Probabilidad de ser transferido a HW después de UCI
        probabilidades$prob.ICU.HW <- input$prob.ICU.HW        
    }, priority=4)    
    
    ##############################################################
    # ---- Simulación condicional ----
    resultados <- reactiveValues()
    observeEvent(input$ejecutar_simulacion,{
        prob.rc.real <- proporciones$prob.rc.real
        prob.rc.woman <- proporciones$prob.rc.woman
        prob.rc.man <- proporciones$prob.rc.man
        prob.w <- proporciones$prob.w
        prob.m <- proporciones$prob.m
        woman.age.prob <- proporciones$woman.age.prob
        man.age.prob <- proporciones$man.age.prob
        men.mean.age.interval <- proporciones$men.mean.age.interval
        women.mean.age.interval <- proporciones$women.mean.age.interval
        
        # -- Paciente es admitido en hospital -- #
        prob.ICU <- probabilidades$prob.ICU
        prob.HW <- probabilidades$prob.HW
        
        # -- Opciones en HW -- #
        prob.HW.death <- probabilidades$prob.HW.death
        prob.HW.ICU <- probabilidades$prob.HW.ICU
        prob.HW.disc <- probabilidades$prob.HW.disc
        
        # -- Opciones en UCI-- #
        prob.ICU.death <- probabilidades$prob.ICU.death
        prob.ICU.HW <- probabilidades$prob.ICU.HW     
        
        # Variables de simulación
        num.cores <- input$num.cores # número de hilos usados en simulación
        registerDoParallel(num.cores) 
        
        m <- input$m # simulaciones
        n.ind <- input$n.ind # individuos infectados
        n.time <- input$n.time # días (follow-up time)
        
        par.m.size <- input$par.m.size # cuántas simulaciones por núcleo
        par.m.loops <- input$par.m.loops # cuántas tandas
        
        cat(m, n.ind, n.time, par.m.size, par.m.loops,'\n')
        cat(prob.ICU.death)
        # Cada bucle paralelo crea una lista de resultados (n.HOS, n.ICU...)
        # Al final se tiene una lista de longitud=par.m.loops, cada una con una lista de resultados
        set.seed(123)
        
        res <- foreach (par.m=1:par.m.loops, .errorhandling="pass") %dopar% {
            # Se inicializan matrices vacías necesarias para la simulación
            # Dependiendo de cada una pueden tener dos dimensiones (simulacion*individuo)
            # o tres (simulacion*individuo*dia)
            age <- gender <- inf.time <- prob.rc <- final.state <- matrix(rep(NA, length.out=par.m.size*n.ind), nrow = par.m.size, ncol = n.ind) 
            n.HOS <- n.ICU <- n.Dead <- n.Discharge <- n.H.Dead <- n.ICU.Dead  <- matrix(rep(NA, length.out= par.m.size*n.time), nrow = par.m.size, ncol = n.time) 
            
            # Inicialización de matriz state, la cual contendrá, para cada simulación, el estado
            # de cada individuo por día
            state <- rep(NA, par.m.size*n.ind*n.time)
            dim(state) <- c(par.m.size, n.ind, n.time)
            
            for (j in 1:par.m.size) { # j = simulación
                #-------------------------------------------------------------
                # -- Definición del sexo de cada individuo usando Bernoulli --
                #-------------------------------------------------------------
                # Mujeres serán 1 y hombres 0
                bern.dist <- rbern(n.ind, prob=prob.w)
                gender[j,] <- bern.dist
                
                #-------------------------------------------------
                # -- Edad y posibilidad de ingresar en hospital --
                #-------------------------------------------------
                # Con la distribución de edades reales se selecciona una etapa (ej: 54.5) y su 
                # probabilidad de hospitalización (ej: 0.28) para cada individuo i en una simulación j
                # Mujeres (1)
                condicion <- which(bern.dist==1)
                n.interval <- sample(1:length(woman.age.prob),size=length(condicion),prob=woman.age.prob, replace=T)
                age[j,][condicion] <- women.mean.age.interval[n.interval]
                prob.rc[j,][condicion] <- prob.rc.woman[n.interval]
                
                # Hombres (0)
                condicion <- which(bern.dist==0)
                n.interval <- sample(1:length(man.age.prob),size=length(condicion),prob=man.age.prob, replace=T)
                age[j,][condicion] <- men.mean.age.interval[n.interval]
                prob.rc[j,][condicion] <- prob.rc.man[n.interval]
                
                #-----------------------------------------------------
                # -- Día en el que se infecta (distribución normal) --
                # ----------------------------------------------------
                inf.time[j,] <- rnorm(n=n.ind, mean=60, sd=10)
                
                # Definición del día de infección en state
                # Los días previos a la infección se guardan como "0", y el día de infección como "I"
                for (i in 1:n.ind){
                    # Ceiling redondea hacia arriba (1.1->2)
                    state[j, i, 1:(ceiling(inf.time[j,i])-1)] = 0
                    state[j, i, ceiling(inf.time[j,i])] = "I"
                }
                
                #--------------------------------------------------------
                # -- Se definen individuos que ingresan en el hospital --
                #--------------------------------------------------------
                u <- runif(n.ind) # esto genera random deviates of the uniform distribution 
                ind.H <- which(u<=prob.rc[j,]) # qué individuos tienen la valor alatorio u <= probabilidad de ser hospitalizados
                
                # -- Para cada individuo seleccionado (que tiene la enfermedad y es hospitalizado) --
                for (i in ind.H){
                    # -- Tiempo desde infección hasta el día de hospitalización --
                    # Cada día en state desde el día de infección hasta el día de hospitalización se define como "I"
                    t.inf.until.hosp <- rnorm(n=1, mean=12-0.05*age[j,i], sd=1)
                    state[j,i,(ceiling(inf.time[j,i])+1):ceiling((inf.time[j,i] + t.inf.until.hosp-1))] = "I"
                    
                    # -- Parámetros Weibull según edad y sexo --
                    scale.ICU.death <- 15.5 * ( (100 - abs(age[j,i]- 60) - 10*gender[j,i]) / 62)
                    scale.ICU.HW <- 16.3 * ( (100 - abs(age[j,i]- 60) - 10*gender[j,i])/62)
                    scale.HW.disc <- 8.4 * ((60 + age[j,i]-10*gender[j,i])/100) 
                    scale.HW.ICU <- 4.2
                    
                    #-------------------------------------------------
                    # -- Simulación del primer estado del individuo --
                    #-------------------------------------------------
                    v1 <- runif(1) # probabilidad aleatoria de que empiece en HW (v1<=prob.HW) o en ICU (v1>=prob.HW)
                    
                    if (v1 <= prob.HW) {
                        # Paciente entra en hospital ward (HW)
                        v2 <- runif(1)
                        if (v2 <= prob.HW.death) {# Patient dies in HW
                            time.HW.death <- rexp(1, 0.1)# Of those admitted in hospital ward, the time to death 
                            state[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp) : ceiling(inf.time[j,i] + t.inf.until.hosp + time.HW.death)-1] = "H"
                            state[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp + time.HW.death)] = "H.Dead"
                            final.state[j,i] = "Dead"
                        } else if ( (v2 > prob.HW.death) && (v2 <= prob.HW.death+prob.HW.ICU) ) {# Patient goes to ICU
                            time.HW.ICU <- rweibull(1, shape=1.6, scale=scale.HW.ICU)# Time since hospital ward admission to ICU
                            state[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp) : ceiling(inf.time[j,i] + t.inf.until.hosp + time.HW.ICU)] = "H"
                            final.state[j,i] = "ICU"
                        } else {# Patient discharged
                            time.HW.disc <- rweibull(1, shape=2.6, scale=scale.HW.disc )# Time since hospital ward admission to discharge is
                            state[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp) : ceiling(inf.time[j,i] + t.inf.until.hosp + time.HW.disc)-1] = "H"
                            state[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp + time.HW.disc)] = "H.Discharge"
                            final.state[j,i] = "Discharge"}
                    } else {
                        # Paciente entra en UCI
                        v3 <- runif(1)
                        if (v3 <= prob.ICU.death) {# Patient dies in ICU
                            time.ICU.death <- rweibull(1, shape=1.4, scale=scale.ICU.death)# Time from admission in ICU to death
                            state[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp) : ceiling(inf.time[j,i] + t.inf.until.hosp + time.ICU.death)-1] = "ICU"
                            state[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp + time.ICU.death)] = "ICU.Dead"
                            final.state[j,i] = "Dead"
                        } else {# Patient goes to hospital ward
                            time.ICU.HW <- rweibull(1, shape=1.8, scale=scale.ICU.HW ) # Time since admission in ICU till return to ward
                            state[j,i,ceiling(inf.time[j,i] + t.inf.until.hosp) : ceiling(inf.time[j,i] + t.inf.until.hosp + time.ICU.HW)] = "ICU"
                            final.state[j,i] = "HOS"
                        }}
                    
                    #-------------------------------------------------------------------------------
                    # -- Simulación del resto de estados para pacientes que queden en el hospital --
                    #-------------------------------------------------------------------------------
                    # Los pacientes que llegan a este punto no han muerto ni han salido del hospital
                    # Son los que su estado final todavía está por decidir
                    # Se repite el bucle while hasta que los individuos mueran, salgan del hospital o se acabe la simulación
                    while((final.state[j,i]=="HOS") | (final.state[j,i]=="ICU")){
                        # Calcular el índice del siguiente día, en el que no se sabe el estado (es NA)
                        i.final <- min(which(is.na(state[j,i,])))
                        
                        # Comprobar si se sobrepasa del tiempo de simulación
                        if (is.infinite(i.final)){
                            break
                        }
                        
                        # -- Simulación --
                        if(final.state[j,i]=="HOS"){
                            # Si el estado final actual es hospital
                            v2 <- runif(1)
                            if (v2 <= prob.HW.death) {# Patient dies in HW
                                time.HW.death <- rexp(1, 0.1)# Of those admitted in hospital ward, the time to death 
                                state[j, i, i.final: pmin(n.time, ceiling(i.final+time.HW.death)-1)] = "H"
                                state[j, i, pmin(n.time, ceiling(i.final+time.HW.death))] = "H.Dead"
                                final.state[j,i] = "Dead"
                            } else if ( (v2 > prob.HW.death) && (v2 <= prob.HW.death+prob.HW.ICU) ) {# Patient goes to ICU
                                time.HW.ICU <- rweibull(1, shape=1.6, scale=scale.HW.ICU)# Time since hospital ward admission to ICU
                                state[j,i,i.final: pmin(n.time, ceiling(i.final+time.HW.ICU))] = "H"
                                final.state[j,i] = "ICU"
                            } else {# Patient discharged
                                time.HW.disc <- rweibull(1, shape=2.6, scale=scale.HW.disc )# Time since hospital ward admission to discharge is
                                state[j,i,i.final: pmin(n.time, ceiling(i.final+time.HW.disc)-1)] = "H"
                                state[j,i,pmin(n.time, ceiling(i.final+time.HW.disc))] = "H.Discharge"
                                final.state[j,i] = "Discharge"}
                        } else {
                            # Si el estado final actual es UCI
                            v3 <- runif(1)
                            if (v3 <= prob.ICU.death) {# Patient dies in ICU
                                time.ICU.death <- rweibull(1, shape=1.4, scale=scale.ICU.death)# Time from admission in ICU to death
                                state[j,i,i.final : pmin(n.time, ceiling(i.final+time.ICU.death)-1)] = "ICU"
                                state[j,i,pmin(n.time, ceiling(i.final+time.ICU.death))] = "ICU.Dead"
                                final.state[j,i] = "Dead"
                            } else {# Patient goes to hospital ward
                                time.ICU.HW <- rweibull(1, shape=1.8, scale=scale.ICU.HW ) # Time since admission in ICU till return to ward
                                state[j,i,i.final :pmin(n.time, ceiling(i.final+time.ICU.HW))] = "ICU"
                                final.state[j,i] = "HOS"
                            }}
                    } # end while
                } # end i in n.ind
                
                #---------------------------------------------------------------------------------
                # Almacenar número de pacientes por simulación en cada estado por día (HOS, ICU, Dead, Discharge)
                #---------------------------------------------------------------------------------
                for (k in 1:n.time){
                    n.HOS[j,k] <- length(which(state[j, ,k]=="H"))
                    n.ICU[j,k] <- length(which(state[j, ,k]=="ICU"))
                    n.H.Dead[j,k] <- length(which(state[j, ,k]=="H.Dead"))
                    n.Discharge[j,k] <- length(which(state[j, ,k]=="H.Discharge"))
                    n.ICU.Dead[j,k] <- length(which(state[j, ,k]=="ICU.Dead"))
                    n.Dead[j,k] <- n.H.Dead[j,k] + n.ICU.Dead[j,k]
                }
            } # end j in m
            # Esta línea saca fuera del bucle paralelo la información
            list(n.HOS=n.HOS,n.ICU=n.ICU,n.H.Dead=n.H.Dead,n.Discharge=n.Discharge,n.ICU.Dead=n.ICU.Dead,n.Dead=n.Dead)
        }
        
        stopImplicitCluster()
        
        
        # ----- Resultados condicionales ---- 
        # El sistema produce resultados por hilo y es necesario juntarlos
        # Se obtienen matrices de dimensiones simulaciones*pacientes*dias
        get.sim.results <- function(res, name){
            return(do.call(rbind, lapply(res, function(x){x[[name]]})))
        }
        n.HOS <- get.sim.results(res, 'n.HOS')
        n.ICU <- get.sim.results(res, 'n.ICU')
        n.H.Dead <- get.sim.results(res, 'n.H.Dead')
        n.Discharge <- get.sim.results(res, 'n.Discharge')
        n.ICU.Dead <- get.sim.results(res, 'n.ICU.Dead')
        n.Dead <- get.sim.results(res, 'n.Dead')
        
        # Se calcula el número de individuos por día en cada categoría (pacientes*dias)
        nHOS <- nICU <- nDead <- nDischarge <- nH.Dead <- nICU.Dead  <- rep(0, length.out= n.time) 
        for (k in 1:n.time){
            nHOS[k] <- sum(n.HOS[,k], na.rm=T)/m
            nICU[k] <- sum(n.ICU[,k], na.rm=T)/m
            nDead[k] <- sum(n.Dead[,k], na.rm=T)/m
            nDischarge[k] <- sum(n.Discharge[,k], na.rm=T)/m
            nH.Dead[k] <- sum(n.H.Dead[,k], na.rm=T)/m
            nICU.Dead[k] <- sum(n.ICU.Dead[,k], na.rm=T)/m
        }
        

        resultados$cambio.neto <- nHOS+nICU-(nDischarge+nDead+nH.Dead+nICU.Dead)
        resultados$nHOS <- nHOS
        resultados$nICU <- nICU
        

    }, priority=3)
    
    plot.condicional.res <- reactive({
        # ---- Examen de días por encima del límite ----
        check.hosp.capacity <- function(hosp, icu, neto, t, cap.stats){
            par(mfrow=c(1,1))
            print(cap.stats)
            # ---- Ver si en algún momento se superó el número de camas ---- #
            cap.convencional <- cap.stats['mediana','Hospitalización convencional']
            cap.uci <- sum(cap.stats['mediana',c('U. Críticas CON respirador', 'U. Críticas SIN respirador')])

            # Número de días que se sobrepasa
            sim.tot.hosp <- hosp+icu
            dias.sobrepasados.convencional <- sum(sim.tot.hosp>=cap.convencional)
            dias.sobrepasados.uci <- sum(icu>=cap.uci)

            # Gráficas
            plot(NA, xlim=c(0,n.time), ylim=c(0,max(max(sim.tot.hosp)+20)), xlab="Días", ylab="Casos", main=t)

            add.range <- function(cap.stats, unidad, col){
                mediana <- cap.stats['mediana',unidad]
                p10 <- cap.stats['percentil10',unidad]
                p90 <- cap.stats['percentil90',unidad]
                abline(h=p10, col='black', lty=2)
                abline(h=p90, col='black', lty=2)
                abline(h=mediana, col='red', lty=1)
                rect(0-50,p90,
                     n.time+50,p10,
                     col= col, lwd=0)
            }
            add.range(cap.stats,'Hospitalización convencional',rgb(0,1,0,alpha=0.1))
            add.range(cap.stats,'U. Críticas CON respirador',rgb(0,0,1,alpha=0.1))
            add.range(cap.stats,'U. Críticas SIN respirador',rgb(1,0,0,alpha=0.1))

            lines(hosp, type="l",lty=1, lwd=2, col='pink')
            lines(icu, type="l",lty=1, lwd=2, col='red')
            lines(sim.tot.hosp, type="l",lty=1, lwd=2, col='orange')
            lines(neto,lty=1, lwd=2, col='green')

            title(sub=paste('Días sobrepasados en HOSP: ', dias.sobrepasados.convencional), adj=1, line=2, font=2,cex.sub = 0.75)
            title(sub=paste('Días sobrepasados en UCI: ', dias.sobrepasados.uci), adj=1, line=3, font=2,cex.sub = 0.75)

        }
        n.time <- input$n.time # días (follow-up time)
        check.hosp.capacity(resultados$nHOS, resultados$nICU,
                            resultados$cambio.neto, t='Condicional',
                            capacidades$area.capacity.stats)

    })
    observe({
        output$res.condicional <- renderPlot(plot.condicional.res())
    })
    
    #############################################################
    # ---- Outputs ----
    output$table.capacidades <- renderDataTable(capacidad.filter(), options = list(scrollX = TRUE, pageLength = 5))
    output$table.casos <- renderDataTable(casos.filter(), options = list(scrollX = TRUE, pageLength = 5))
    output$table.hospitalizados <- renderDataTable(get.hospitalizados(), options = list(scrollX = TRUE, pageLength = 5))
    
    output$prob.rc.real <- renderText(proporciones$prob.rc.real)
    output$prob.w <- renderText(proporciones$prob.w)
    output$prob.m <- renderText(proporciones$prob.m)

    


    
}

# Run the app ----
shinyApp(ui = ui, server = server)


