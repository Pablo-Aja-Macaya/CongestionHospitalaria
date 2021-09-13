#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Shiny
library(shiny)
library(shinyjs)
library(shinycssloaders)
library(shinybusy)
library(shinythemes)
library(shinyWidgets)
library(shiny.i18n)
library(shinyBS)

# Simulación
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


# ---- Examen de días por encima del límite ----
check.hosp.capacity <- function(hosp, icu, neto, t, cap.stats, time){
    par(mfrow=c(1,1))
    # ---- Ver si en algún momento se superó el número de camas ---- #
    cap.convencional <- cap.stats['mediana','Hospitalización convencional']
    cap.uci <- sum(cap.stats['mediana',c('U. Críticas CON respirador', 'U. Críticas SIN respirador')])
    
    # Número de días que se sobrepasa
    sim.tot.hosp <- hosp+icu
    dias.sobrepasados.convencional <- sum(sim.tot.hosp>=cap.convencional)
    dias.sobrepasados.uci <- sum(icu>=cap.uci)
    
    # Gráficas
    plot(NA, xlim=c(0,time), ylim=c(0,max(max(sim.tot.hosp)+20)), xlab="Días", ylab="Casos", main=t)
    
    add.range <- function(cap.stats, unidad, col){
        mediana <- cap.stats['mediana',unidad]
        p10 <- cap.stats['percentil10',unidad]
        p90 <- cap.stats['percentil90',unidad]
        abline(h=p10, col='black', lty=2)
        abline(h=p90, col='black', lty=2)
        abline(h=mediana, col='red', lty=1)
        rect(0-50,p90,
             time+50,p10,
             col= col, lwd=0)
    }
    add.range(cap.stats,'Hospitalización convencional',rgb(0,1,0,alpha=0.1))
    add.range(cap.stats,'U. Críticas CON respirador',rgb(0,0,1,alpha=0.1))
    add.range(cap.stats,'U. Críticas SIN respirador',rgb(1,0,0,alpha=0.1))
    
    lines(hosp, type="l",lty=1, lwd=2, col='pink')
    lines(icu, type="l",lty=1, lwd=2, col='red')
    lines(sim.tot.hosp, type="l",lty=1, lwd=2, col='orange')
    lines(neto,lty=1, lwd=2, col='green')
    
    legend("topright", legend = c("nHOS", "nICU",'nHOS+nICU','Cambio neto (in-out)'),
           col = c('pink','red','orange','green'), lty=c(1,1,1,1,2), pch = c(NA,NA,NA,NA), bty = "n")

    title(sub=paste('Días sobrepasados en HOSP: ', dias.sobrepasados.convencional), adj=1, line=2, font=2,cex.sub = 0.75)
    title(sub=paste('Días sobrepasados en UCI: ', dias.sobrepasados.uci), adj=1, line=3, font=2,cex.sub = 0.75)
    
}


# ---- Función de filtrado de outliers ----
filter.outliers <- function(df, filter.type, sel.col, h, u){
    outliers <- c() # inicializar outliers (shiny protesta si no se hace)
    
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
        df[[sel.col]] <- rollapply(df[[sel.col]], width=5, FUN=median, align='left', fill=NA)
        return(df)
        
    } else if (filter.type=='sliding_mean'){
        df[[sel.col]] <- rollapply(df[[sel.col]], width=5, FUN=mean, align='left', fill=NA)
        return(df)
    }
    
    # Eliminar outliers
    if (length(outliers)!=0){ # si hay algún outlier
        df <- df[-which(df[[sel.col]] %in% outliers),] # quitar las filas del dataset con los que hayan sido outliers
    }
    
    print(glue('Se eliminan {length(outliers)} outliers de {sel.col} ({h} - {u})\n'))
    return(df)
    
}



#######################
# ----- Weibull ----- #
#######################
# Dos maneras de calcular Weibull:
# - Con fórmulas / manualmente
# - Automáticamente con "eweibull" a partir de datos reales

# -- Con eweibull --
load("../datos/full_weibull.Rdata")

get.pams <- function(df, age, sex){
    # Encontrar fila más cercana a una edad por sexo
    # (Sólo para datos condicionales)
    matrix.by.sex <- subset(df, sexo==sex)
    sel.row <- matrix.by.sex[which.min(abs(matrix.by.sex$edad - age)),]
    return(sel.row)
}

#################
# ---- APP ---- #
#################

# Diccionario de traducciones
translator <- Translator$new(translation_json_path = "./translations/translation.json")
translator$set_translation_language("en") # lenguaje por defecto
languages <- list('Español'='es', 'English'='en')

library(markdown)
ui <- fluidPage(
    # theme = shinytheme("lumen"),
    shinyjs::useShinyjs(),
    shiny.i18n::usei18n(translator),

    tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {
                        background-color: #008080 !important; color: white !important
                    }
                    #sidebar, Sidebar {
                        background-color: white;
                        border: 0px solid white;
                        padding: 0; 
                        margin: 0;
                    }
                    a{
                        color: #008080;
                    }
                    '
                    )), # color de highlight de tabla
    tags$style(".fa-quesion {color: black}"),
    add_busy_bar(color = "#008080"),
    
    title=translator$t("Hospital congestion"),
    
    br(),
    fluidRow(
        column(7,
            titlePanel(h2(strong('CEDCOVID:', translator$t("Hospital congestion")), style = "color:#008080")),
            ),
        column(5,
               fluidRow(
                   column(4,selectInput('selected_language',
                                        NULL,
                                        choices = languages,
                                        selected = translator$get_key_translation()), align='center'),
                   column(4,actionButton("automatic.var",translator$t("Default variables"), 
                                         ), align='center'),
                   column(4,actionButton("ejecutar_simulacion",translator$t("Execute simulation"), icon("paper-plane"), 
                                          style="color: #fff; background-color: #008080; border-color: #2e6da4;"), align='center'), 
               ),

        )
    ),
    br(),
    # ---- Variables y resultados ----
    sidebarLayout(
        div(id='Sidebar',
            sidebarPanel(id='sidebar',
                bsCollapse(id = "collapseExample", open = "Panel 2",
                           # ---- Variables comunes ----
                           bsCollapsePanel(
                               h4(strong(translator$t("Common variables")), style = "color:#008080"),
                               selectInput("area.sanitaria",
                                           strong(translator$t("Sanitary area")),
                                           choices = c("Coruña - Cee", "Ferrol", "Lugo - A Mariña - Monforte de Lemos",
                                                       "Ourense - Verín - O Barco de Valdeorras", "Pontevedra - O Salnés",
                                                       "Santiago de Compostela - Barbanza", "Vigo"),
                                           selected = "Coruña - Cee"
                               ),
                               
                               selectInput("hosp.ref",
                                           strong(translator$t("Hospitals")),
                                           choices = list('All'='all'),
                                           selected = 'all'),
                               
                               selectInput("modo.weibull",
                                           strong(translator$t("Weibull calculations")),
                                           choices = list("Manual/Fórmula" = 'manual',
                                                          'Automático' = 'automatico'),
                                           selected = 'manual'),
                               
                               selectInput("outlier.filter.type",
                                           strong(translator$t("Outlier removal")),
                                           choices = list('Permissive boxplot'='extended'),
                                           selected = 'extended'),
                               style = "info"),
                           # ---- Variables de simulación ----
                           bsCollapsePanel(
                               h4(strong(translator$t("Simulation variables")), style = "color:#008080"), 
                               p(strong('Info', style = "color:#008080"), circleButton("helpbox_simulacion_button",icon("question"),size='xs')), 
                               uiOutput("helpbox_simulacion"),
                               fluidRow(
                                   column(12,
                                          numericInput("m", strong(translator$t("Simulations")),
                                                       min = 0, max = 2000, value = 100),                     
                                   ),
                                   column(12,
                                          numericInput("n.ind", strong(translator$t("Individuals")),
                                                       min = 0, max = 1000, value = 1000),                  
                                   ),
                                   column(12,
                                          numericInput("n.time", strong(translator$t("Days")),
                                                       min = 0, max = 300, value = 250),                  
                                   ),
                               ),
                               fluidRow(
                                   column(12,
                                          numericInput("num.cores", strong(translator$t("Threads")),
                                                       min = 0, max = 6, value = 4),                     
                                   ),
                                   column(12,
                                          numericInput("par.m.loops", strong(translator$t("Groups")),
                                                       min = 1, max = 20, value = 10),                     
                                   ),
                               ),
                               fluidRow(
                                   column(12,
                                          numericInput("inf.time.avg", strong(translator$t("Average day of infection")),
                                                       min = 100, max = 200, value = 100),                     
                                   ),
                                   column(12,
                                          numericInput("inf.time.sd", strong(translator$t("Deviation from infection day")),
                                                       min = 1, max = 25, value = 10),                     
                                   ),
                               ),
                               style = "info"),
                           # ---- Probabilidades ----
                           bsCollapsePanel(
                               h4(strong(translator$t("Probabilities"),style = "color:#008080")),
                               p(strong('Info', style = "color:#008080"), circleButton("helpbox_probabilidades_button",icon("question"),size='xs')),
                               uiOutput("helpbox_probabilidades"),
                               h5(strong(translator$t("Initial probabilities")), style = "color:#008080"),
                               fluidRow(
                                   column(12,
                                          sliderInput("prob.ICU", strong(translator$t("ICU admission")),
                                                      min = 0, max = 1, value = 0.5, ticks=F),                       
                                   ),
                                   column(12,
                                          sliderInput("prob.HW", strong(translator$t("Hospital admission")),
                                                      min = 0, max = 1, value = 0.5, ticks=F),                       
                                   ),
                               ),
                               
                               h5(strong(translator$t("Probabilities in hospital")), style = "color:#008080"),
                               fluidRow(
                                   column(12,
                                          sliderInput("prob.HW.death", strong(translator$t("Death")),
                                                      min = 0, max = 1, value = 0.5, ticks=F),                     
                                   ),
                                   column(12,
                                          sliderInput("prob.HW.ICU", strong(translator$t("ICU")),
                                                      min = 0, max = 1, value = 0.5, ticks=F),                      
                                   ),
                                   column(12,
                                          sliderInput("prob.HW.disc", strong(translator$t("Discharge")),
                                                      min = 0, max = 1, value = 0.5, ticks=F),                    
                                   ),
                               ),
                               
                               h5(strong(translator$t("Probabilities in ICU")), style = "color:#008080"),
                               fluidRow(
                                   column(12,
                                          sliderInput("prob.ICU.death", strong(translator$t("Death")),
                                                      min = 0, max = 1, value = 0.5, ticks=F),                     
                                   ),
                                   column(12,
                                          sliderInput("prob.ICU.HW", strong(translator$t("Hospital")),
                                                      min = 0, max = 1, value = 0.5, ticks=F),                     
                                   ),
                               ),
                               style = "info"),
                           # ---- Proporciones muestrales ----
                           bsCollapsePanel(
                               h4(strong(translator$t("Variables calculated from hospital data")), style = "color:#008080"),
                               dataTableOutput("table.prob.resumen"),
                               numericInput('prob.w',
                                            strong(translator$t("Probability of being a woman (prob.w)")),
                                            min = 0, max = 1, value = 0),
                               numericInput('prob.m',
                                            strong(translator$t("Probability of being a man (prob.m)")),
                                            min = 0, max = 1, value = 0),
                               numericInput('prob.rc.real',
                                            strong(translator$t("Hospitalized proportions (prob.rc.real)")),
                                            min = 0, max = 1, value = 0),
                               style = "info"
                            )

                           ########
                ),
                # ---- Imagen citic ----
                hr(),
                img(src='logo_citic.png', align = "center", width="60%", style="display: block; margin-left: auto; margin-right: auto;"),
                br(),
                img(src='logo_gain.png', align = "center", width="60%", style="display: block; margin-left: auto; margin-right: auto;"),
                br(),
                img(src='logo_feder.jpg', align = "center", width="60%", style="display: block; margin-left: auto; margin-right: auto;"),
                br(),
                img(src='logo_xunta.png', align = "center", width="60%", style="display: block; margin-left: auto; margin-right: auto;"),
                
                width=3)            
        ),


        # ---- Apartados ----
        mainPanel(
            # ---- Tabsets ----
            tabsetPanel(id='tabset_resultados', type = "tabs",
                        tabPanel(translator$t("Capacity analysis"), 
                                 h4(strong(translator$t("Capacity analysis")),circleButton("helpbox_analisis_capacidad_button",icon("question"),size='xs')),
                                 uiOutput("helpbox_analisis_capacidad"),
                                 plotOutput("analisis") %>% withSpinner()
                                 
                        ),
                        tabPanel(translator$t("Capacities"),
                                 br(),
                                 dataTableOutput("table.capacidades")
                        ),
                        tabPanel(translator$t("Cases"),
                                 br(),
                                 dataTableOutput("table.casos")
                        ),
                        tabPanel(translator$t("Hospitalized"),
                                 br(),
                                 dataTableOutput("table.hospitalizados")
                        ),
                        tabPanel(translator$t("Simulation"), 
                                 h4(strong(translator$t("Simulation"))),
                                 plotOutput("res.condicional") %>% withSpinner(),
                                 plotOutput("res.incondicional") %>% withSpinner()
                        )
                        # tabPanel('Datos',
                        #     fluidRow(
                        #         column(
                        #             br(),
                        #             tabsetPanel(type = "tabs",
                        # 
                        #             ),
                        #             width = 12)
                        #     )
                        # )
            ),


        width=9),
    ),


)



# Define server logic ----
server <- function(input, output, session) {
    # ---- Actualizar traducciones ----
    observeEvent(input$selected_language, {
        shiny.i18n::update_lang(session, input$selected_language)
    })
    
    observe({
        # Actualizar traducciones de variables
        translator$set_translation_language(input$selected_language)
        
        hosp.ref.choices <- list()
        hosp.ref.choices[[translator$t('All')]] <- 'all'
        hosp.ref.choices[[translator$t('Refference')]] <- 1
        hosp.ref.choices[[translator$t('Non refference')]] <- 0
        updateSelectInput(session, 'hosp.ref', label = NULL, choices = hosp.ref.choices,
                          selected = NULL)
        
        modo.weibull.choices <- list()
        modo.weibull.choices[[translator$t('Manual/Formula')]] <- 'manual'
        modo.weibull.choices[[translator$t('Automatic')]] <- 'automatico'
        updateSelectInput(session, 'modo.weibull', label = NULL, choices = modo.weibull.choices,
                          selected = NULL)   
        
        outlier.removal.choices <- list()
        outlier.removal.choices[[translator$t('Permissive boxplot')]] <- 'extended'
        outlier.removal.choices[[translator$t('Typical boxplot')]] <- 'boxplot'
        outlier.removal.choices[[translator$t('Sliding window median')]] <- 'sliding_median'
        outlier.removal.choices[[translator$t('Sliding window average')]] <- 'sliding_mean'
        outlier.removal.choices[[translator$t('None')]] <- NA
        updateSelectInput(session, 'outlier.filter.type', label = NULL, choices = outlier.removal.choices,
                          selected = NULL)   
    })

    # ---- Ocultar sidebar ----
    observeEvent(input$toggleSidebar, {
        shinyjs::toggle(id = "Sidebar")
    })
    
    # ---- Variables automáticas ----
    observeEvent(input$automatic.var,{

        print('Automáticas')
        m.value <- 100

        updateNumericInput(session,"m", label = NULL,
                           min = NULL, max = NULL, value = m.value)

    })
    
    # observe({
    #     if(req())
    # })
    
    # ---- Comentarios de ayuda ----
    output$helpbox_simulacion = renderUI({
        if (input$helpbox_simulacion_button %% 2){
            helpText(translator$t("These variables guide the simulation system. It will use N simulations, each with J individuals and K days. These simulations will be split in groups and divided among the selected threads."))
        } else {
            return()
        }
    })
    output$helpbox_probabilidades = renderUI({
        if (input$helpbox_probabilidades_button %% 2){
            helpText(translator$t("The system simulates the state of each patient across the selected days (Hospital, ICU, discharge or death). The transition between each state is given by these variables."))
        } else {
            return()
        }
    })
    output$helpbox_analisis_capacidad = renderUI({
        if (input$helpbox_analisis_capacidad_button %% 2){
            helpText(translator$t("The analysis indicates, for each hospital and unit (ICU, conventional), its capacity (black), the number of occupied beds (COVID, red or non Covid, blue), the area where the avaliable beds  orbit (purple) and the days where a limit was achieved (red columns)."))
        } else {
            return()
        }
    })
    
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
        capacidad <- filter.ref(capacidad, input$hosp.ref)
        capacidad
    })
    analisis.capacidad <- reactive({
        capacidad <- capacidad.filter()
        
        # Quitar la unidad de centro no sanitario porque no aporta nada
        capacidad <- subset(capacidad, unidad!='Centros no sanitarios')
        
        # Hospitales
        hospitales <- sort(unique(capacidad$hospital))
        
        # Unidades
        unidades <- sort(unique(capacidad$unidad))
        
        # Lista para guardar dataframes de cada hospital con la unidad y cuentas
        hospital.capacity.stats <- list()
        
        par(mfrow=c(length(hospitales)*3,2), mar=c(5,4,6,2))
        for (h in hospitales){
            plot.tittle.line <- 1
            outlier.filter.type <- input$outlier.filter.type
            # Inicialización de dataframe de unidades*medidas para el hospital
            hospital.capacity.stats[[h]] <- data.frame(matrix(ncol = length(unidades), nrow = 3))
            names(hospital.capacity.stats[[h]]) <- unidades
            row.names(hospital.capacity.stats[[h]]) <- c('mediana','percentil10','percentil90')
            for (u in unidades){
                # Total de camas de este hospital por unidad y ordenadas por fecha
                datos <- subset(capacidad, hospital==h & unidad==u)
                datos <- datos[order(datos$fecha_envio),]
                
                # Filtrar outliers del total de camas
                # cat('---------------\n')
                # datos <- filter.outliers(datos, filter.type='extended', sel.col='total_camas', h, u)
                # datos <- filter.outliers(datos, filter.type='boxplot', sel.col='ocupadas_covid19', h, u)
                # datos <- filter.outliers(datos, filter.type='boxplot', sel.col='ocupadas_no_covid19', h, u)
                
                # Añadir total de ocupadas
                datos$total_ocupadas <- datos$ocupadas_covid19 + datos$ocupadas_no_covid19
                
                # Variables repetidas
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
                
                # -- Histograma ----
                # hist(tot, breaks = 30, main=glue('{h} \n({u})'), col='gray', xlab='Total camas') 
                # title(sub=paste('Mediana:', mediana), adj=1, line=2, font=2,cex.sub = 0.75)
                # title(sub=paste('Percentil 10:', percentiles[['10%']]), adj=1, line=3, font=2,cex.sub = 0.75)
                # title(sub=paste('Percentil 90:', percentiles[['90%']]), adj=1, line=4, font=2,cex.sub = 0.75)
                # title("Histograma de camas", line = plot.tittle.line)
                
                # # -- Plot de número de camas a lo largo de la pandemia ----
                # plot(total_camas ~ fecha_envio, datos, ylim=c(0,max(tot, na.rm=T)+max(tot, na.rm=T)*0.25), xaxt = "n", type = "l", main=NA, xlab=NA, ylab='Camas')
                # # Columnas rojas (días donde se sobrepasa una de las estadísticas)
                # # Cuanto más rojizas más gravedad
                # for (d in min.sobrepasado){
                #     rect(d-1, 0-20,
                #          d+1, max(tot, na.rm=T)+80,
                #          col= rgb(1,0,0,alpha=0.05), lwd=0)
                # }
                # for (d in max.sobrepasado){
                #     rect(d-1, 0-20,
                #          d+1, max(tot, na.rm=T)+80,
                #          col= rgb(1,0,0,alpha=0.3), lwd=0)
                # }
                # for (d in median.sobrepasado){
                #     rect(d-1, 0-20,
                #          d+1, max(tot, na.rm=T)+80,
                #          col= rgb(1,0,0,alpha=0.15), lwd=0)
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

                
                # -- Plot de porcentaje de ocupación a lo largo de la pandemia ----
                ocupados.covid.pct <- (datos$ocupadas_covid19/datos$total_camas)*100
                ocupados.nocovid.pct <- (datos$ocupadas_no_covid19/datos$total_camas)*100
                ocupados.total.pct <- ((datos$ocupadas_covid19+datos$ocupadas_no_covid19)/datos$total_camas)*100
                df.ocupados.pct <- data.frame(ocupados.covid.pct, ocupados.nocovid.pct, ocupados.total.pct, fecha_envio=datos$fecha_envio)
                
                plot(ocupados.covid.pct ~ fecha_envio, df.ocupados.pct, ylim=c(0,100), xaxt = "n", type = "l", main=glue('{h} \n({u})'), xlab=NA, ylab='Ocupación (%)', col='red')
                
                add.risk.scale = function(u){
                    # https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov/documentos/Actuaciones_respuesta_COVID_26.03.2021.pdf
                    # Dependiendo de la unidad el porcentaje de ocupación es más o menos preocupante
                    if (u %in% c('Hospitalización convencional')){
                        risks <- data.frame(nueva.normalidad = c(0,2), bajo = c(2,5),
                                            medio = c(5,10), alto = c(10,15), muy.alto = c(15,100))
                    } else if (u %in% c('U. Críticas CON respirador','U. Críticas SIN respirador')){
                        risks <- data.frame(nueva.normalidad = c(0,5), bajo = c(5,10),
                                            medio = c(10,15), alto = c(15,25), muy.alto = c(25,100))
                    } else {return(NULL)}
                    # Se añade el color a cada nivel
                    risk.alpha <- 0.1
                    risks <- rbind(risks, c(rgb(0,1,0,alpha=risk.alpha), rgb(1,1,0,alpha=risk.alpha),
                                            rgb(1,0.7,0,alpha=risk.alpha), rgb(1,0,1,alpha=risk.alpha),
                                            rgb(1,0,0,alpha=risk.alpha)))
                    # Dibujo de áreas de riesgo en el color correspondiente
                    apply(risks, 2, function(l){
                        rect(fechas[1], l[1],
                             fechas[length(fechas)], l[2],
                             col= l[3], lwd=0.08)   
                        
                    })
                    # Datos se ponen ahora para pisar los cuadros de color
                    lines(ocupados.covid.pct ~ fecha_envio, df.ocupados.pct, type="l",lty=1, lwd=1, col='red')
                    lines(ocupados.nocovid.pct ~ fecha_envio, df.ocupados.pct, type="l",lty=1, lwd=1, col='blue')
                    lines(ocupados.total.pct ~ fecha_envio, df.ocupados.pct, type="l",lty=1, lwd=1, col='green')
                    
                }
                add.risk.scale(u)
                
                axis.Date(1, at=seq(min(fechas), max(fechas), length.out=10), format='%b %Y', las=2, cex.axis=0.8)   
                
                title("Porcentaje de ocupación", line = plot.tittle.line)
                
                # title(glue('{h} \n({u})'), line = -3, outer = TRUE) # título general (hospital y unidad)
                
                
                ##############################################
                # Total de camas de este hospital por unidad y ordenadas por fecha
                datos <- subset(capacidad, hospital==h & unidad==u)
                datos <- datos[order(datos$fecha_envio),]
                
                # Filtrar outliers del total de camas
                # cat('---------------\n')
                datos <- filter.outliers(datos, filter.type=outlier.filter.type, sel.col='total_camas', h, u)
                datos <- filter.outliers(datos, filter.type=outlier.filter.type, sel.col='ocupadas_covid19', h, u)
                datos <- filter.outliers(datos, filter.type=outlier.filter.type, sel.col='ocupadas_no_covid19', h, u)

                # Añadir total de ocupadas
                datos$total_ocupadas <- datos$ocupadas_covid19 + datos$ocupadas_no_covid19
                
                # Variables repetidas
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
                
                # -- Plot de porcentaje de ocupación a lo largo de la pandemia ----
                ocupados.covid.pct <- (datos$ocupadas_covid19/datos$total_camas)*100
                ocupados.nocovid.pct <- (datos$ocupadas_no_covid19/datos$total_camas)*100
                ocupados.total.pct <- ((datos$ocupadas_covid19+datos$ocupadas_no_covid19)/datos$total_camas)*100
                df.ocupados.pct <- data.frame(ocupados.covid.pct, ocupados.nocovid.pct, ocupados.total.pct, fecha_envio=datos$fecha_envio)
                
                plot(ocupados.covid.pct ~ fecha_envio, df.ocupados.pct, ylim=c(0,100), xaxt = "n", type = "l", main=glue('{h} \n({u})'), xlab=NA, ylab='Ocupación (%)', col='red')
                
                add.risk.scale = function(u){
                    # https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov/documentos/Actuaciones_respuesta_COVID_26.03.2021.pdf
                    # Dependiendo de la unidad el porcentaje de ocupación es más o menos preocupante
                    if (u %in% c('Hospitalización convencional')){
                        risks <- data.frame(nueva.normalidad = c(0,2), bajo = c(2,5),
                                            medio = c(5,10), alto = c(10,15), muy.alto = c(15,100))
                    } else if (u %in% c('U. Críticas CON respirador','U. Críticas SIN respirador')){
                        risks <- data.frame(nueva.normalidad = c(0,5), bajo = c(5,10),
                                            medio = c(10,15), alto = c(15,25), muy.alto = c(25,100))
                    } else {return(NULL)}
                    # Se añade el color a cada nivel
                    risk.alpha <- 0.1
                    risks <- rbind(risks, c(rgb(0,1,0,alpha=risk.alpha), rgb(1,1,0,alpha=risk.alpha),
                                            rgb(1,0.7,0,alpha=risk.alpha), rgb(1,0,1,alpha=risk.alpha),
                                            rgb(1,0,0,alpha=risk.alpha)))
                    # Dibujo de áreas de riesgo en el color correspondiente
                    apply(risks, 2, function(l){
                        rect(fechas[1], l[1],
                             fechas[length(fechas)], l[2],
                             col= l[3], lwd=0.08)   
                        
                    })
                    # Datos se ponen ahora para pisar los cuadros de color
                    lines(ocupados.covid.pct ~ fecha_envio, df.ocupados.pct, type="l",lty=1, lwd=1, col='red')
                    lines(ocupados.nocovid.pct ~ fecha_envio, df.ocupados.pct, type="l",lty=1, lwd=1, col='blue')
                    lines(ocupados.total.pct ~ fecha_envio, df.ocupados.pct, type="l",lty=1, lwd=1, col='green')
                    
                }
                add.risk.scale(u)
                
                axis.Date(1, at=seq(min(fechas), max(fechas), length.out=10), format='%b %Y', las=2, cex.axis=0.8)   
                
                title("Porcentaje de ocupación (Outliers quitados)", line = plot.tittle.line)
                
                # title(glue('{h} \n({u})'), line = -3, outer = TRUE) # título general (hospital y unidad)
                
                
                
                
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

    output$analisis <- renderPlot(analisis.capacidad(), height=3000)
    
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
    # ---- Simulaciones ----
    resultados <- reactiveValues()
    observeEvent(input$ejecutar_simulacion,{
        updateTabsetPanel(session = session, inputId = "tabset_resultados", selected = translator$t("Capacity analysis"))
        # Proporciones
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
        
        par.m.loops <- input$par.m.loops # cuántas tandas
        par.m.size <- m/par.m.loops # cuántas simulaciones por núcleo
        
        inf.time.avg <- input$inf.time.avg
        inf.time.sd <- input$inf.time.sd
        
        modo.weibull <- input$modo.weibull
        
        ####################################
        # ---- Simulación condicional ---- #
        ####################################
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
                inf.time[j,] <- rnorm(n=n.ind, mean=inf.time.avg, sd=inf.time.sd)
                
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
                    # Variables repetidas
                    ind.age <- age[j,i]
                    ind.gender <- gender[j,i]
                    ind.inf.time <- inf.time[j,i]
                    
                    # Cada día en state desde el día de infección hasta el día de hospitalización se define como "I"
                    t.inf.until.hosp <- rnorm(n=1, mean=12-0.05*ind.age, sd=1)
                    state[j,i,(ceiling(ind.inf.time)+1):ceiling((ind.inf.time + t.inf.until.hosp-1))] = "I"
                    
                    # -- Parámetros Weibull según edad y sexo --
                    if(modo.weibull=='manual'){
                        # Modo: fórmula manual
                        # Scale
                        scale.ICU.death <- 15.5 * ( (100 - abs(ind.age- 60) - 10*ind.gender) / 62 )
                        scale.ICU.HW <- 16.3 * ( (100 - abs(ind.age- 60) - 10*ind.gender) / 62 )
                        scale.HW.disc <- 8.4 * ( (60 + ind.age-10*ind.gender)/100 )
                        scale.HW.ICU <- 4.2
                        # Shape
                        shape.ICU.death <- 1.4
                        shape.ICU.HW <- 1.8
                        shape.HW.disc <- 2.6
                        shape.HW.ICU <- 1.6        
                    } else if (modo.weibull=='automatico') {
                        # Modo: calculados por "eweibull" (pasa de 18 segundos a 36)
                        # Filas objetivo
                        weibull.ICU.death <- get.pams(weibull.ICU.death.cond, ind.age, ind.gender)
                        weibull.ICU.HW <- get.pams(weibull.ICU.HW.cond, ind.age, ind.gender)
                        weibull.HW.disc <- get.pams(weibull.HW.disc.cond, ind.age, ind.gender)
                        weibull.HW.ICU <- get.pams(weibull.HW.ICU.cond, ind.age, ind.gender)
                        # Scale
                        scale.ICU.death <- weibull.ICU.death$scale
                        scale.ICU.HW <- weibull.ICU.HW$scale
                        scale.HW.disc <- weibull.HW.disc$scale
                        scale.HW.ICU <- weibull.HW.ICU$scale
                        # Shape
                        shape.ICU.death <- weibull.ICU.death$shape
                        shape.ICU.HW <- weibull.ICU.HW$shape
                        shape.HW.disc <- weibull.HW.disc$shape
                        shape.HW.ICU <- weibull.HW.ICU$shape        
                    }
                    
                    
                    #-------------------------------------------------
                    # -- Simulación del primer estado del individuo --
                    #-------------------------------------------------
                    v1 <- runif(1) # probabilidad aleatoria de que empiece en HW (v1<=prob.HW) o en ICU (v1>=prob.HW)
                    
                    if (v1 <= prob.HW) {
                        # Paciente entra en hospital ward (HW)
                        v2 <- runif(1)
                        if (v2 <= prob.HW.death) {# Patient dies in HW
                            time.HW.death <- rexp(1, 0.1)# Of those admitted in hospital ward, the time to death 
                            state[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.HW.death)-1] = "H"
                            state[j,i,ceiling(ind.inf.time + t.inf.until.hosp + time.HW.death)] = "H.Dead"
                            final.state[j,i] = "Dead"
                        } else if ( (v2 > prob.HW.death) && (v2 <= prob.HW.death+prob.HW.ICU) ) {# Patient goes to ICU
                            time.HW.ICU <- rweibull(1, shape=shape.HW.ICU, scale=scale.HW.ICU)# Time since hospital ward admission to ICU
                            state[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.HW.ICU)] = "H"
                            final.state[j,i] = "ICU"
                        } else {# Patient discharged
                            time.HW.disc <- rweibull(1, shape=shape.HW.disc, scale=scale.HW.disc )# Time since hospital ward admission to discharge is
                            state[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.HW.disc)-1] = "H"
                            state[j,i,ceiling(ind.inf.time + t.inf.until.hosp + time.HW.disc)] = "H.Discharge"
                            final.state[j,i] = "Discharge"}
                    } else {
                        # Paciente entra en UCI
                        v3 <- runif(1)
                        if (v3 <= prob.ICU.death) {# Patient dies in ICU
                            time.ICU.death <- rweibull(1, shape=shape.ICU.death, scale=scale.ICU.death)# Time from admission in ICU to death
                            state[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.ICU.death)-1] = "ICU"
                            state[j,i,ceiling(ind.inf.time + t.inf.until.hosp + time.ICU.death)] = "ICU.Dead"
                            final.state[j,i] = "Dead"
                        } else {# Patient goes to hospital ward
                            time.ICU.HW <- rweibull(1, shape=shape.ICU.HW, scale=scale.ICU.HW ) # Time since admission in ICU till return to ward
                            state[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.ICU.HW)] = "ICU"
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
                                time.HW.ICU <- rweibull(1, shape=shape.HW.ICU, scale=scale.HW.ICU)# Time since hospital ward admission to ICU
                                state[j,i,i.final: pmin(n.time, ceiling(i.final+time.HW.ICU))] = "H"
                                final.state[j,i] = "ICU"
                            } else {# Patient discharged
                                time.HW.disc <- rweibull(1, shape=shape.HW.disc, scale=scale.HW.disc )# Time since hospital ward admission to discharge is
                                state[j,i,i.final: pmin(n.time, ceiling(i.final+time.HW.disc)-1)] = "H"
                                state[j,i,pmin(n.time, ceiling(i.final+time.HW.disc))] = "H.Discharge"
                                final.state[j,i] = "Discharge"}
                        } else {
                            # Si el estado final actual es UCI
                            v3 <- runif(1)
                            if (v3 <= prob.ICU.death) {# Patient dies in ICU
                                time.ICU.death <- rweibull(1, shape=shape.ICU.death, scale=scale.ICU.death)# Time from admission in ICU to death
                                state[j,i,i.final : pmin(n.time, ceiling(i.final+time.ICU.death)-1)] = "ICU"
                                state[j,i,pmin(n.time, ceiling(i.final+time.ICU.death))] = "ICU.Dead"
                                final.state[j,i] = "Dead"
                            } else {# Patient goes to hospital ward
                                time.ICU.HW <- rweibull(1, shape=shape.ICU.HW, scale=scale.ICU.HW ) # Time since admission in ICU till return to ward
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
        
        # Guardar resultados
        resultados$cambio.neto.cond <- nHOS+nICU-(nDischarge+nDead+nH.Dead+nICU.Dead)
        resultados$nHOS <- nHOS
        resultados$nICU <- nICU

        #######################################
        # ---- Simulación no condicional ---- #
        # Cálculo previo de Weibull 
        if(input$modo.weibull=='manual'){ # TODO: poner condicion
            # Modo: manual
            # Scale
            scale.ICU.death <- 15.5 
            scale.ICU.HW <- 16.3 
            scale.HW.disc <- 8.4
            scale.HW.ICU <- 4.2
            # Shape
            shape.ICU.death <- 1.4
            shape.ICU.HW <- 1.8
            shape.HW.disc <- 2.6
            shape.HW.ICU <- 1.6      
        } else if (input$modo.weibull=='automatico') {
            # Modo: calculados por "eweibull"
            # Scale
            scale.ICU.death <- weibull.ICU.death.inc["scale"]
            scale.ICU.HW <- weibull.ICU.HW.inc["scale"]
            scale.HW.disc <- weibull.HW.disc.inc["scale"]
            scale.HW.ICU <- weibull.HW.ICU.inc["scale"]    
            # Shape
            shape.ICU.death <- weibull.ICU.death.inc["shape"]
            shape.ICU.HW <- weibull.ICU.HW.inc["shape"]
            shape.HW.disc <- weibull.HW.disc.inc["shape"]
            shape.HW.ICU <- weibull.HW.ICU.inc["shape"]       
        }
        
        # Cada bucle paralelo crea una lista de resultados (n.HOS, n.ICU...)
        # Al final se tiene una lista de longitud=par.m.loops, cada una con una lista de resultados
        set.seed(123)
        res <- foreach (par.m=1:par.m.loops, .errorhandling="pas") %dopar% {
            # Se inicializan matrices vacías necesarias para la simulación
            # Dependiendo de cada una pueden tener dos dimensiones (simulacion*individuo)
            # o tres (simulacion*individuo*dia)
            age.inc <- gender.inc <- inf.time <- prob.rc <- final.state.inc <- matrix(rep(NA, length.out=par.m.size*n.ind), nrow = par.m.size, ncol = n.ind) 
            n.HOS.inc <- n.ICU.inc <- n.Dead.inc <- n.Discharge.inc <- n.H.Dead.inc <- n.ICU.inc.Dead.inc  <- matrix(rep(NA, length.out= par.m.size*n.time), nrow = par.m.size, ncol = n.time)
            
            # Inicialización de matriz state, la cual contendrá, para cada simulación, el estado
            # de cada individuo por día
            state.inc <- rep(NA, par.m.size*n.ind*n.time)
            dim(state.inc) <- c(par.m.size, n.ind, n.time)
            
            for (j in 1:par.m.size){
                #-------------------------------------------------------------
                # -- Definición del sexo de cada individuo usando Bernoulli --
                #-------------------------------------------------------------
                # Mujeres serán 1 y hombres 0
                gender.inc[j,] <- rbern(n.ind, prob=prob.w) # Gender from real data distribution
                
                #-------------------------------------------------
                # -- Edad y posibilidad de ingresar en hospital --
                #-------------------------------------------------
                for (i in 1:n.ind){
                    if (gender.inc[j,i] == 1) { # mujeres
                        n.interval <- sample(1:length(woman.age.prob),size=1,prob=woman.age.prob)
                        age.inc[j,i] <- women.mean.age.interval[n.interval]
                    } else { # hombres
                        n.interval <- sample(1:length(man.age.prob),size=1,prob=man.age.prob)
                        age.inc[j,i] <- men.mean.age.interval[n.interval]
                    }
                    
                }  
                prob.rc[j,] <- prob.rc.real
                
                #-----------------------------------------------------
                # -- Día en el que se infecta (distribución normal) --
                # ----------------------------------------------------
                inf.time[j,] <-rnorm(n=n.ind, mean=inf.time.avg, sd=inf.time.sd)
                
                # Definición del día de infección en state
                # Los días previos a la infección se guardan como "0", y el día de infección como "I"
                for (i in 1:n.ind){
                    state.inc[j, i, 1:(ceiling(inf.time[j,i])-1)] = 0
                    state.inc[j, i, ceiling(inf.time[j,i])] = "I"
                }
                
                #--------------------------------------------------------
                # -- Se definen individuos que ingresan en el hospital --
                #--------------------------------------------------------
                u <- runif(n.ind)
                ind.H <- which(u<=prob.rc[j,])
                
                # -- Para cada individuo seleccionado (que tiene la enfermedad y es hospitalizado) --
                for (i in ind.H){
                    # Variables repetidas
                    ind.inf.time <- inf.time[j,i]
                    
                    # Time since infection until hospital admission
                    t.inf.until.hosp <- rnorm(n=1, mean=12-0.05*age.inc[j,i], sd=1)
                    state.inc[j,i,(ceiling(ind.inf.time)+1):ceiling((ind.inf.time + t.inf.until.hosp-1))] = "I"
                    
                    #-------------------------------------------------
                    # -- Simulación del primer estado del individuo --
                    #-------------------------------------------------
                    v1 <- runif(1)
                    if (v1 <= prob.HW) {# Patient in hospital ward
                        v2 <- runif(1)
                        if (v2 <= prob.HW.death) {# Patient dies in HW
                            time.HW.death <- rexp(1, 0.1)# Of those admitted in hospital ward, the time to death 
                            state.inc[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.HW.death)-1] = "H"
                            state.inc[j,i,ceiling(ind.inf.time + t.inf.until.hosp + time.HW.death)] = "H.Dead"
                            final.state.inc[j,i] = "Dead"
                        } else if ( (v2 > prob.HW.death) && (v2 <= prob.HW.death+prob.HW.ICU) ) {# Patient goes to ICU
                            time.HW.ICU <- rweibull(1, shape=shape.HW.ICU, scale=scale.HW.ICU)# Time since hospital ward admission to ICU
                            state.inc[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.HW.ICU)] = "H"
                            final.state.inc[j,i] = "ICU"
                        } else {# Patient discharged
                            time.HW.disc <- rweibull(1, shape=shape.HW.disc, scale=scale.HW.disc )# Time since hospital ward admission to discharge is
                            state.inc[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.HW.disc)-1] = "H"
                            state.inc[j,i,ceiling(ind.inf.time + t.inf.until.hosp + time.HW.disc)] = "H.Discharge"
                            final.state.inc[j,i] = "Discharge"}
                    } else {
                        #---------------------------------------------
                        # Patient in ICU
                        v3 <- runif(1)
                        if (v3 <= prob.ICU.death) {# Patient dies in ICU
                            time.ICU.death <- rweibull(1, shape=shape.ICU.death, scale=scale.ICU.death)# Time from admission in ICU to death
                            state.inc[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.ICU.death)-1] = "ICU"
                            state.inc[j,i,ceiling(ind.inf.time + t.inf.until.hosp + time.ICU.death)] = "ICU.Dead"
                            final.state.inc[j,i] = "Dead"
                        } else {# Patient goes to hospital ward
                            time.ICU.HW <- rweibull(1, shape=shape.ICU.HW, scale=scale.ICU.HW ) # Time since admission in ICU till return to ward
                            state.inc[j,i,ceiling(ind.inf.time + t.inf.until.hosp) : ceiling(ind.inf.time + t.inf.until.hosp + time.ICU.HW)] = "ICU"
                            final.state.inc[j,i] = "HOS"
                        }}
                    
                    #-------------------------------------------------------------------------------
                    # -- Simulación del resto de estados para pacientes que queden en el hospital --
                    #-------------------------------------------------------------------------------
                    # Los pacientes que llegan a este punto no han muerto ni han salido del hospital
                    # Son los que su estado final todavía está por decidir
                    # Se repite el bucle while hasta que los individuos mueran, salgan del hospital o se acabe la simulación
                    while((final.state.inc[j,i]=="HOS") | (final.state.inc[j,i]=="ICU")){
                        # Calcular el índice del siguiente día, en el que no se sabe el estado (es NA)
                        i.final <- min(which(is.na(state.inc[j,i,])))
                        
                        # Comprobar si se sobrepasa del tiempo de simulación
                        if (is.infinite(i.final)){
                            break
                        }
                        
                        # -- Simulación --
                        if(final.state.inc[j,i]=="HOS"){
                            # Si el estado final actual es HOS
                            v2 <- runif(1)
                            if (v2 <= prob.HW.death) {# Patient dies in HW
                                time.HW.death <- rexp(1, 0.1)# Of those admitted in hospital ward, the time to death 
                                state.inc[j, i, i.final: pmin(n.time,ceiling(i.final+time.HW.death)-1)] = "H"
                                state.inc[j, i, pmin(n.time,ceiling(i.final+time.HW.death))] = "H.Dead"
                                final.state.inc[j,i] = "Dead"
                            } else if ( (v2 > prob.HW.death) && (v2 <= prob.HW.death+prob.HW.ICU) ) {# Patient goes to ICU
                                time.HW.ICU <- rweibull(1, shape=shape.HW.ICU, scale=scale.HW.ICU)# Time since hospital ward admission to ICU
                                state.inc[j,i,i.final: pmin(n.time,ceiling(i.final+time.HW.ICU))] = "H"
                                final.state.inc[j,i] = "ICU"
                            } else {# Patient discharged
                                time.HW.disc <- rweibull(1, shape=shape.HW.disc, scale=scale.HW.disc)# Time since hospital ward admission to discharge is
                                state.inc[j,i,i.final: pmin(n.time,ceiling(i.final+time.HW.disc)-1)] = "H"
                                state.inc[j,i,pmin(n.time,ceiling(i.final+time.HW.disc))] = "H.Discharge"
                                final.state.inc[j,i] = "Discharge"}
                        } else {
                            # Si el estado final actual es UCI
                            v3 <- runif(1)
                            if (v3 <= prob.ICU.death) {# Patient dies in ICU
                                time.ICU.death <- rweibull(1, shape=shape.ICU.death, scale=scale.ICU.death)# Time from admission in ICU to death
                                state.inc[j,i,i.final : pmin(n.time,ceiling(i.final+time.ICU.death)-1)] = "ICU"
                                state.inc[j,i,pmin(n.time,ceiling(i.final+time.ICU.death))] = "ICU.Dead"
                                final.state.inc[j,i] = "Dead"
                            } else {# Patient goes to hospital ward
                                time.ICU.HW <- rweibull(1, shape=shape.ICU.HW, scale=scale.ICU.HW ) # Time since admission in ICU till return to ward
                                state.inc[j,i,i.final : pmin(n.time,ceiling(i.final+time.ICU.HW))] = "ICU"
                                final.state.inc[j,i] = "HOS"
                            }}
                        
                    } # end while
                } # end i in n.ind
                
                #---------------------------------------------------------------------------------
                # Almacenar número de pacientes por simulación en cada estado por día (HOS, ICU, Dead, Discharge)
                #---------------------------------------------------------------------------------
                for (k in 1:n.time){
                    n.HOS.inc[j,k] <- length(which(state.inc[j, ,k]=="H"))
                    n.ICU.inc[j,k] <- length(which(state.inc[j, ,k]=="ICU"))
                    n.H.Dead.inc[j,k] <- length(which(state.inc[j, ,k]=="H.Dead"))
                    n.Discharge.inc[j,k] <- length(which(state.inc[j, ,k]=="H.Discharge"))
                    n.ICU.inc.Dead.inc[j,k] <- length(which(state.inc[j, ,k]=="ICU.Dead"))
                    n.Dead.inc[j,k] <- n.H.Dead.inc[j,k] + n.ICU.inc.Dead.inc[j,k]
                }
            } # end j in m
            # Esta línea saca fuera del bucle paralelo la información
            list(n.HOS.inc=n.HOS.inc,n.ICU.inc=n.ICU.inc,n.H.Dead.inc=n.H.Dead.inc,n.Discharge.inc=n.Discharge.inc,n.ICU.inc.Dead.inc=n.ICU.inc.Dead.inc,n.Dead.inc=n.Dead.inc)
        }
        stopImplicitCluster()
        
        
        # ---- Resultados no condicionales ----
        # El sistema produce resultados por hilo y es necesario juntarlos
        # Se obtienen matrices de dimensiones simulaciones*pacientes*dias
        get.sim.results <- function(res, name){
            return(do.call(rbind, lapply(res, function(x){x[[name]]})))
        }
        
        n.HOS.inc <- get.sim.results(res, 'n.HOS.inc')
        n.ICU.inc <- get.sim.results(res, 'n.ICU.inc')
        n.Dead.inc <- get.sim.results(res, 'n.Dead.inc')
        n.Discharge.inc <- get.sim.results(res, 'n.Discharge.inc')
        n.H.Dead.inc <- get.sim.results(res, 'n.H.Dead.inc')
        n.ICU.inc.Dead.inc <- get.sim.results(res, 'n.ICU.inc.Dead.inc')
        
        # Se calcula el número de individuos por día en cada categoría (pacientes*dias)
        nHOS.inc <- nICU.inc <- nDead.inc <- nDischarge.inc <- nH.Dead.inc <- nICU.inc.Dead  <- rep(0, length.out= n.time) 
        for (k in 1:n.time){
            nHOS.inc[k] <- sum(n.HOS.inc[,k], na.rm=T)/m
            nICU.inc[k] <- sum(n.ICU.inc[,k], na.rm=T)/m
            nDead.inc[k] <- sum(n.Dead.inc[,k], na.rm=T)/m
            nDischarge.inc[k] <- sum(n.Discharge.inc[,k], na.rm=T)/m
            nH.Dead.inc[k] <- sum(n.H.Dead.inc[,k], na.rm=T)/m
            nICU.inc.Dead[k] <-sum(n.ICU.inc.Dead.inc[,k], na.rm=T)/m
        }
        
        # Guardar resultados
        resultados$cambio.neto.inc <- nHOS.inc+nICU.inc-(nDischarge.inc+nDead+nH.Dead.inc+nICU.inc.Dead)
        resultados$nHOS.inc <- nHOS.inc
        resultados$nICU.inc <- nICU.inc

        updateTabsetPanel(session = session, inputId = "tabset_resultados", selected = translator$t("Simulation"))
    }, priority=3)
    
    plot.condicional.res <- reactive({
        n.time <- input$n.time # días (follow-up time)
        check.hosp.capacity(resultados$nHOS, resultados$nICU,
                            resultados$cambio.neto.cond, t='Condicional',
                            capacidades$area.capacity.stats, n.time)

    })
    plot.incondicional.res <- reactive({
        n.time <- input$n.time # días (follow-up time)
        check.hosp.capacity(resultados$nHOS.inc, resultados$nICU.inc,
                            resultados$cambio.neto.inc, t='Incondicional',
                            capacidades$area.capacity.stats, n.time)
        
    })
    observe({
        output$res.condicional <- renderPlot(plot.condicional.res())
        output$res.incondicional <- renderPlot(plot.incondicional.res())
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


