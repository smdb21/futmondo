#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(igraph)
library(networkD3)
library(data.table)
library(colorRamps)
library(ggplot2)
library(pals)
library(plotly)
library(tidyr)
library(lubridate)
library(DT)
# read data
t = read.csv("www/compras.txt",sep = "\t", encoding ="UTF-8")
t$Fecha <- as.Date(t$Fecha,format="%d/%m/%Y - %H:%M:%S")
t$Precio <- as.numeric(t$Precio)
setorderv(t,c("Fecha"),order = -1)
num_colores = nrow(distinct(t, t[,"Comprador"]))
colores <- primary.colors(num_colores, steps = 10, no.white = TRUE)
usuarios <- distinct(t['Comprador'])
usuarios <- arrange(usuarios, Comprador)
usuarios <- add_row(usuarios, Comprador = "Todos", .before = 1)
jugadores <- distinct(t['Futbolista'])
jugadores <- arrange(jugadores, Futbolista)
jugadores <- add_row(jugadores, Futbolista = "Todos", .before = 1)
# primera y ultima fecha
primera_fecha <- summarise(t, min(Fecha))[[1]]
ultima_fecha <- summarise(t, max(Fecha))[[1]]
# puntos / clasificacion
puntos = fread(file = "www/puntos.txt",sep = "\t", encoding ="UTF-8")
ultima_jornada <- max(puntos$jornada)

filter_tabla_compras <- function(df, tablaCompras_UsuarioInput, jugadorInput, tablaCompras_FechaRangoInput, tipoCompraInput){
    tmp <- t[t$Fecha >= as.Date(tablaCompras_FechaRangoInput[1]) & t$Fecha <= as.Date(tablaCompras_FechaRangoInput[2]), ]
    # browser()
    if (tipoCompraInput != "TODOS"){
        if (tipoCompraInput == "COMPRA FUTMONDO"){
            tmp <- tmp[tmp$Tipo == "COMPRA" & (tmp$Comprador=='futmondo' | tmp$Vendedor=='futmondo'),]
        }else if (tipoCompraInput == "COMPRA JUGADORES"){
            tmp <- tmp[tmp$Tipo == "COMPRA" & tmp$Comprador!='futmondo' & tmp$Vendedor!='futmondo',]
        }else{
            tmp <- tmp[tmp$Tipo == tipoCompraInput,]
        }
    }
    if (tablaCompras_UsuarioInput == "Todos" & jugadorInput == "Todos") {
        return(tmp[,c(0:6)])
    }else if (tablaCompras_UsuarioInput == "Todos") {
        return(tmp[tmp$Futbolista == jugadorInput ,c(0:6)])
    }else if (jugadorInput == "Todos") {
        return(tmp[tmp$Comprador == tablaCompras_UsuarioInput ,c(0:6)])
    }else{
        return(tmp[tmp$Comprador == tablaCompras_UsuarioInput & tmp$Futbolista == jugadorInput ,c(0:6)])
    }
}
get_jugador_foto_url <- function(df, jugador){
    tmp <- df[df$Futbolista == jugador & df$Tipo == 'COMPRA','Foto']
    url <- as.vector(tmp[1])
    return (url)
}
create_jugador_network_data <- function(tabla_compras, jugadorInput){
    # if(jugadorInput != "Todos"){
    # select the Comprador and Vendedor columns and call them "from", "to"
    networkData <- tibble(from=tabla_compras[,"Comprador"],to=tabla_compras[,"Vendedor"])
    # group by "from" abd "to" and count the pair frequency, creating a new column Freq
    networkData <- networkData %>%  group_by(from, to)  %>% summarise(Freq = n())
    return (networkData)
    # }else{
    #     return
    # }
}

createComprasBarChart <- function(tabla_compras){
    # create table with the sums of compras per usuario
    req(tabla_compras)
    tmp <- tabla_compras[tabla_compras$Comprador != 'futmondo',] # we discard futmondo as comprador
    tmp <- tmp %>%  group_by(Comprador)  %>% summarise(Gastado = sum(Precio)) %>% arrange(desc(Gastado))
    plot <- ggplot(data = tmp, 
                   aes(
                       x = reorder(Comprador,-Gastado), # order x by Gastado descending 
                       y = Gastado,
                       fill = Comprador)) + # color by Comprador
        geom_col() + # column plot
        scale_fill_manual(values = cols25(16)) + # 16 colors 
        labs(title = "Total gastado por jugador", subtitle = "Ordenado de más a menos gasto", x = NULL, y = "EUR") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0)) +
        scale_y_continuous(labels = scales::label_number_si()) # makes y labels to be '600M'
    ggplotly(plot)
}

createBarPlotJugadoresMasFichados <- function(jugadores_mas_fichados, top_n){
    # this table has 2 columns: Futbolista and n
    plot <- ggplot(data = jugadores_mas_fichados,
                   aes(x = reorder(Futbolista, -n), 
                       y = n,
                       fill = Futbolista)) + # color by Futbolista
        geom_col() + # column plot
        scale_fill_manual(values = glasbey(min(top_n,32))) + # top_n colors 
        labs(title = paste("Top ", top_n, " futbolistas más fichados"), x = "Futbolista", y = "Número de fichajes") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0))
    ggplotly(plot)
    
}
#  
#
ui <- navbarPage("Futmondo - OHY CAMPEÓN - v1.0.1",
                 tabPanel("Fichajes",
                          navlistPanel(widths = c(2,10), fluid = TRUE,
                                       "Fichajes",
                                       tabPanel("Mercado",
                                                sidebarLayout( 
                                                    sidebarPanel(
                                                        p("Filtra datos aquí:"),
                                                        selectInput("tablaCompras_UsuarioInput", "Jugador:", usuarios),
                                                        selectInput("tablaCompras_jugadorInput", "Futbolista:", jugadores),
                                                        radioButtons(inputId = "tablaCompras_tipoCompraInput", label="Tipo de compra", choices = c("Todos"="TODOS","Compras con futmondo"="COMPRA FUTMONDO","Compras entre jugadores"="COMPRA JUGADORES", "Clausulas"="CLAUSULA"), selected = "TODOS"),
                                                        dateRangeInput("tablaCompras_FechaRangoInput", 
                                                                       "Rango de fechas",
                                                                       separator = "-",
                                                                       start = primera_fecha, 
                                                                       end = ultima_fecha, 
                                                                       min = primera_fecha, 
                                                                       max = ultima_fecha, 
                                                                       weekstart = 1, 
                                                                       language = "es", 
                                                                       format = "dd-mm-yyyy"),
                                                        htmlOutput("jugadorFotoCompras"),
                                                        width = 2
                                                    ),
                                                    
                                                    # Show the table
                                                    mainPanel(
                                                        fluidRow(column(12, verbatimTextOutput("stats"))),
                                                        fluidRow(column(5,simpleNetworkOutput("jugadorGraphCompras")),
                                                                 column(7,plotlyOutput("sumaComprasGraph"))),
                                                        fluidRow(column(12,dataTableOutput("tablaComprasMercado"))),
                                                        width = 10
                                                    )
                                                )    
                                       ),
                                       tabPanel("Top Fichajes",
                                                sidebarLayout( 
                                                    sidebarPanel(
                                                        p("Futbolistas fichados más veces en el mercado"),
                                                        selectInput("tablaCompras_topFichadoInput", "Top:", c(1:32), selected = 10),
                                                        width = 3
                                                    ),
                                                    
                                                    # Show the table
                                                    mainPanel(
                                                        fluidRow(plotlyOutput("jugadoresMasFichadosGraph")),
                                                        dataTableOutput("tablaComprasTopJugadoresFichados"),
                                                        width = 9
                                                    )
                                                )   
                                       )
                          )
                          
                 ),
                 tabPanel("Gastos",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("usuarioGastos", "Filtra por Jugador:", c("Todos", usuarios), selected = TRUE, multiple = TRUE),
                                  width = 2
                              ),
                              
                              # Show the table
                              mainPanel(
                                  plotlyOutput("gastosPlot")
                              )
                          )
                 ),
                 tabPanel("Asociaciones",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("usuarioAsociaciones", "Filtra por Jugador:", c("Todos", usuarios), selected = TRUE),
                                  width = 2
                              ),
                              
                              # Show the table
                              mainPanel(
                                  forceNetworkOutput("asociacionesPlot")
                              )
                          )
                 ),
                 tabPanel("Clasificación",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("usuarioClasificacion", "Filtra grafico por Jugador:", c("Todos", usuarios), selected = TRUE, multiple = TRUE),
                                  selectInput("jornadaClasificacion", "Clasificacion en jornada:", c(paste0('Ultima: ', ultima_jornada), seq(ultima_jornada-1,1)), selected = TRUE),
                                  width = 2
                              ),
                              mainPanel(
                                  dataTableOutput(outputId = 'tablaClasificacion'),
                                  plotlyOutput(outputId = 'plotClasificacion')
                              )
                          ))
                 # ,
                 # navbarMenu("More",
                 #            tabPanel("Table",
                 #                     DT::dataTableOutput("table2")
                 #            ),
                 #            tabPanel("About",
                 #                     verbatimTextOutput("summary2")
                 #            )
                 # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    
    # remove last column with url
    rv <- reactiveValues(tabla_compras = t, jugadorNetworkData = create_jugador_network_data(t, "Todos"))
    # events from compras en mercado
    # fecha de rango para la tabla
    observeEvent(input$tablaCompras_FechaRangoInput,{
        # rv$tabla_compras <- filter_tabla_compras(t, input$tablaCompras_UsuarioInput, input$tablaCompras_jugadorInput, input$tablaCompras_FechaRangoInput, input$tablaCompras_tipoCompraInput)
        rv$jugadorNetworkData <- create_jugador_network_data(tabla_compras(), input$tablaCompras_jugadorInput)
    })
    # filtrar por usuario
    observeEvent(input$tablaCompras_UsuarioInput,{
        # rv$tabla_compras <- filter_tabla_compras(t, input$tablaCompras_UsuarioInput, input$tablaCompras_jugadorInput, input$tablaCompras_FechaRangoInput, input$tablaCompras_tipoCompraInput)
        rv$jugadorNetworkData <- create_jugador_network_data(tabla_compras(), input$tablaCompras_jugadorInput)
    }, ignoreInit = TRUE)
    # filtrar por jugador
    observeEvent(input$tablaCompras_jugadorInput,{
        # browser()
        # rv$tabla_compras <- filter_tabla_compras(t, input$tablaCompras_UsuarioInput, input$tablaCompras_jugadorInput, input$tablaCompras_FechaRangoInput, input$tablaCompras_tipoCompraInput)
        rv$jugadorNetworkData <- create_jugador_network_data(tabla_compras(), input$tablaCompras_jugadorInput)
        if (input$tablaCompras_jugadorInput == "Todos"){
            output$jugadorFotoCompras <- renderText("")
        }else{
            output$jugadorFotoCompras <- renderText({
                c(
                    '<img src="', get_jugador_foto_url(t,input$tablaCompras_jugadorInput), 
                    '" height="50px" width="50px">'
                )
            })
        }
    })
    observeEvent(input$tablaCompras_tipoCompraInput,{
        # rv$tabla_compras <- filter_tabla_compras(t, input$tablaCompras_UsuarioInput, input$tablaCompras_jugadorInput, input$tablaCompras_FechaRangoInput, input$tablaCompras_tipoCompraInput)
        rv$jugadorNetworkData <- create_jugador_network_data(tabla_compras(), input$tablaCompras_jugadorInput)
    })
    
    tabla_compras <- reactive({
        filter_tabla_compras(t, input$tablaCompras_UsuarioInput, input$tablaCompras_jugadorInput, input$tablaCompras_FechaRangoInput, input$tablaCompras_tipoCompraInput)
    })
    # filtrar por tipo de compra
    
    ##############################
    # OUTPUTS
    ##############################
    
    # Tabla de compras en mercado
    output$tablaComprasMercado <- renderDT(
        tabla_compras(),
        options = list(
            pageLength = 10
            # initComplete = I("function(settings, json) {alert('Done.');}")
        )
    )
    # grafico de barras con el dinero gastado en compras
    output$sumaComprasGraph <- renderPlotly(createComprasBarChart(tabla_compras()))
    
    # Tabla de jugadores más fichados por compra en mercado
    output$tablaComprasTopJugadoresFichados <- renderDT({
        ranking_jugadores_fichados <- t %>% count(Futbolista) %>% arrange(desc(n)) %>% slice(seq_len(input$tablaCompras_topFichadoInput))
        setnames(ranking_jugadores_fichados, c("Futbolista","n"), c("Futbolista","# veces fichado")) # rename column names as Futbolista
    },
    options = list(pageLength = 50
                   # initComplete = I("function(settings, json) {alert('Done.');}")
    )
    )
    # stats with the money spent by players
    output$stats <- renderText({
        req(tabla_compras())
        tmp <- tabla_compras()[tabla_compras()$Comprador != 'futmondo',] # we discard futmondo as comprador
        tmp <- tmp %>%  group_by(Comprador)  %>% summarise(Gastado = sum(Precio)) %>% arrange(desc(Gastado))
        jugador_max = as.character(tmp[[1,1]])
        jugador_max_gasto = format(tmp[[1,2]], big.mark=",")
        lastIndex = dim(tmp)[1]
        jugador_min = as.character(tmp[[lastIndex,1]])
        jugador_min_gasto = format(tmp[[lastIndex,2]], big.mark=",")
        return(paste(sep="", "Estadísticas desde ", input$tablaCompras_FechaRangoInput[1], " hasta ", input$tablaCompras_FechaRangoInput[2] ,":\n", 
                     "Media dinero gastado: ", format(mean(tmp$Gastado), big.mark=","), " eur\n",
                     "Total dinero gastado: ", format(sum(tmp$Gastado), big.mark=","), " eur\n",
                     "Jugador que más ha gastado: ", jugador_max, " (", jugador_max_gasto, " eur)\n",
                     "Jugador que menos ha gastado: ", jugador_min, " (", jugador_min_gasto, " eur)\n"
        )
        )
    })
    # bat plot con los jugadores más fichados
    output$jugadoresMasFichadosGraph <- renderPlotly({
        ranking_jugadores_fichados <- t %>% count(Futbolista) %>% arrange(desc(n)) %>% slice(seq_len(input$tablaCompras_topFichadoInput))
        createBarPlotJugadoresMasFichados(ranking_jugadores_fichados, input$tablaCompras_topFichadoInput)
    })
    
    
    output$gastosPlot <- renderPlotly({
        req(input$usuarioGastos)
        ######################### 
        # construimos tabla de gastos acumulados
        tmp <- t[,c('Fecha', 'Comprador', 'Precio')] %>% arrange(Fecha)
        ##############################
        tmp2 <- NULL
        for(usuario in input$usuarioGastos){
            if(usuario == 'Todos'){
                tmp3 <- tmp
                tmp3$Comprador = 'Todos'
            }else{
                tmp3 <- tmp[tmp$Comprador == usuario,]
            }
            tmp3 <- tmp3 %>% mutate(Fecha = Fecha) %>%
                mutate(`Gasto acumulado` = cumsum(Precio))
            tmp2 <- rbind(tmp2, tmp3)
        }
        # if(input$usuarioGastos != 'Todos'){
        #     plot <- ggplot(data=tmp2, aes(x=Fecha, y=`Gasto acumulado`, group=Comprador, color=Comprador)) +
        #         geom_line() +
        #         theme_classic()+
        #         scale_y_continuous(labels = scales::label_number_si())
        # }else{
        #     tmp <- tmp %>% mutate(Fecha = Fecha) %>%
        #         mutate(`Gasto acumulado` = cumsum(Precio))
        plot <- ggplot(data=tmp2, aes(x=Fecha, y=`Gasto acumulado`, group=Comprador, color=Comprador)) +
            geom_line() +
            theme_classic() +
            scale_y_continuous(labels = scales::label_number_si())
        # }
        
        
        ggplotly(plot)
    }
    )
    
    # we remove any transaction with futmondo
    no_futmondo <- filter(t, Comprador!="futmondo" & Vendedor!="futmondo")
    # select the Comprador and Vendedor columns and call them "from", "to"
    networkData <- tibble(from=no_futmondo[,"Comprador"],to=no_futmondo[,"Vendedor"])
    # group by "from" abd "to" and count the pair frequency, creating a new column Freq
    networkData <- networkData %>%  group_by(from, to)  %>% summarise(Freq = n())
    
    output$asociacionesPlot <- renderForceNetwork({
        usuario <- input$usuarioAsociaciones
        net <- networkData
        if (usuario != 'Todos'){
            net <- net[net$from == usuario | net$to == usuario,]
        }
        simpleNetwork(net, height="100px", width="100px",        
                      Source = 1,                 # column number of source
                      Target = 2,                 # column number of target
                      linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
                      charge = -900,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                      fontSize = 14,               # size of the node names
                      fontFamily = "serif",       # font og node names
                      linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
                      nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
                      opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                      zoom = T                    # Can you zoom on the figure?
        )
    })
    
    output$jugadorGraphCompras <- renderForceNetwork({
        data_to_plot <- rv$jugadorNetworkData
        if (dim(data_to_plot)[1] == 0){
            return(NULL)
        }
        
        simpleNetwork(data_to_plot, height="100px", width="100px",        
                      Source = 1,                 # column number of source
                      Target = 2,                 # column number of target
                      linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
                      charge = -900,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                      fontSize = 14,               # size of the node names
                      fontFamily = "serif",       # font og node names
                      linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
                      nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
                      opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                      zoom = T                    # Can you zoom on the figure?
        )
    })
    
    output$tablaClasificacion <- renderDT(
        {
            # datos de hasta la jornada x
            if (input$jornadaClasificacion == paste0('Ultima: ', ultima_jornada)){
                num_jornada <- ultima_jornada
            }else{
                num_jornada <- as.numeric(input$jornadaClasificacion)
            }
            if (!is.null(num_jornada)){
                puntos <- puntos[jornada<=num_jornada,]
            }
            clasificacion <- puntos[,.(total=sum(puntos)), by=jugador]
            setorder(clasificacion, -total)
            clasificacion
        },
        options = list(pageLength = nrow(usuarios), dom='t')
    )
}

# Run the application 
shinyApp(ui = ui, server = server)


