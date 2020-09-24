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
# read data
t = read.csv("www/compras.txt",sep = "\t", encoding ="UTF-8")
t$Fecha <- as.Date(t$Fecha,format="%d/%m/%Y - %H:%M:%S")
setorderv(t,c("Fecha"),order = 1)
num_colores = nrow(distinct(t, t[,"Comprador"]))
colores <- primary.colors(num_colores, steps = 10, no.white = TRUE)
usuarios <- distinct(t['Comprador'])
usuarios <- arrange(usuarios, Comprador)
usuarios <- add_row(usuarios, Comprador = "Todos", .before = 1)
jugadores <- distinct(t['Jugador'])
jugadores <- arrange(jugadores, Jugador)
jugadores <- add_row(jugadores, Jugador = "Todos", .before = 1)
primera_fecha <- summarise(t, min(Fecha))[[1]]
ultima_fecha <- summarise(t, max(Fecha))[[1]]

filter_by_usuario_jugador <- function(df, usuarioInput, jugadorInput, fechaRangoInput){
    tmp <- t[t$Fecha >= as.Date(fechaRangoInput[1]), ]
    if (usuarioInput == "Todos" & jugadorInput == "Todos") {
        return(tmp[,c(0:5)])
    }else if (usuarioInput == "Todos") {
        return(tmp[tmp$Jugador == jugadorInput ,c(0:5)])
    }else if (jugadorInput == "Todos") {
        return(tmp[tmp$Comprador == usuarioInput ,c(0:5)])
    }else{
        return(tmp[tmp$Comprador == usuarioInput & tmp$Jugador == jugadorInput ,c(0:5)])
    }
}
get_jugador_foto_url <- function(df, jugador){
    return (as.vector(df[df$Jugador==jugador,'Foto'][1])[1])
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
#  
#
ui <- navbarPage("Futmondo - smdb21",
    tabPanel("Fichajes",
             navlistPanel(widths = c(2,8),
                 "Opciones de fichajes",
                 tabPanel("Tabla de fichajes",
                          sidebarLayout( 
                              sidebarPanel(
                                  p("Filtra la tabla aquí o bien debajo de la última fila"),
                                  selectInput("usuarioInput", "Filtra tabla por Usuario:", usuarios),
                                  selectInput("jugadorInput", "Filtra tabla por Jugador:", jugadores),
                                  dateRangeInput("fechaRangoInput", 
                                                 "Fecha",
                                                 start = primera_fecha, 
                                                 end = ultima_fecha, 
                                                 min = primera_fecha, 
                                                 max = ultima_fecha, 
                                                 weekstart = 1, 
                                                 language = "es", 
                                                 format = "dd-mm-yyyy"),
                                  htmlOutput("jugadorFoto"),
                                  width = 3
                              ),
                              
                              # Show the table
                              mainPanel(
                                  dataTableOutput("tablaFichajes"),
                                  forceNetworkOutput("jugadorGraph"),
                                  width = 9
                              )
                          )    
                 ),
                 tabPanel("Fichajes mas comunes",
                          sidebarLayout( 
                              sidebarPanel(
                                  p("Jugadores fichados más veces"),
                                  selectInput("topFichadoInput", "Top:", c(1:25), selected = 5),
                                  width = 3
                              ),
                              
                              # Show the table
                              mainPanel(
                                  dataTableOutput("tablaTopJugadoresFichados"),
                                  width = 9
                              )
                          )   
                 )
             )
             
    ),
    tabPanel("Gastos",
             sidebarLayout(
                 sidebarPanel(
                     selectInput("usuarioGastos", "Filtra tabla por Usuario:", c("Todos", distinct(t, t[,"Comprador"])), selected = TRUE)
                 ),
                 
                 # Show the table
                 mainPanel(
                     plotOutput("gastosPlot")
                 )
             )
    ),
    tabPanel("Asociaciones",
             sidebarLayout(
                 sidebarPanel(
                     selectInput("usuarioGastos", "Filtra tabla por Usuario:", c("Todos", distinct(t, t[,"Comprador"])), selected = TRUE)
                 ),
                 
                 # Show the table
                 mainPanel(
                     forceNetworkOutput("asociacionesPlot")
                 )
             )
    ),
    navbarMenu("More",
               tabPanel("Table",
                        DT::dataTableOutput("table2")
               ),
               tabPanel("About",
                        verbatimTextOutput("summary2")
               )
    )
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
    rv <- reactiveValues(tabla_compras = t[,c(0:5)], jugadorNetworkData = create_jugador_network_data(t, "Todos"))
    observeEvent(input$fechaRangoInput,{
        rv$tabla_compras <- filter_by_usuario_jugador(t, input$usuarioInput, input$jugadorInput, input$fechaRangoInput)
        rv$jugadorNetworkData <- create_jugador_network_data(rv$tabla_compras, input$jugadorInput)
    })
    observeEvent(input$usuarioInput,{
        rv$tabla_compras <- filter_by_usuario_jugador(t, input$usuarioInput, input$jugadorInput, input$fechaRangoInput)
        rv$jugadorNetworkData <- create_jugador_network_data(rv$tabla_compras, input$jugadorInput)
    })
    observeEvent(input$jugadorInput,{
        rv$tabla_compras <- filter_by_usuario_jugador(t, input$usuarioInput, input$jugadorInput, input$fechaRangoInput)
        rv$jugadorNetworkData <- create_jugador_network_data(rv$tabla_compras, input$jugadorInput)
        if (input$jugadorInput == "Todos"){
            output$jugadorFoto <- renderText("")
         }else{
            output$jugadorFoto <- renderText({
                c(
                    '<img src="', get_jugador_foto_url(t,input$jugadorInput), 
                    '" height="50px" width="50px">'
                )
            })
        }
    })
    
    output$tablaFichajes <- renderDataTable(rv$tabla_compras,
                                    searchDelay = 200,
                                    options = list(
        pageLength = 50
        # initComplete = I("function(settings, json) {alert('Done.');}")
        )
    )
    
    output$tablaTopJugadoresFichados <- renderDataTable({
                                            ranking_jugadores_fichados <- t %>% count(Jugador) %>% arrange(desc(n)) %>% slice(seq_len(input$topFichadoInput))
                                            setnames(ranking_jugadores_fichados, c("Jugador","n"), c("Jugador","# veces fichado"))
                                            },
                                            searchDelay = 200,
                                            options = list(
                                                pageLength = 50
                                                # initComplete = I("function(settings, json) {alert('Done.');}")
                                            )
    )
    
    output$gastosPlot <- renderPlot(
        ggplot(data=t, aes(x=Fecha, y=Precio, group=Comprador, color=Comprador)) +
            geom_line() 
    )
    
    # we remove any transaction with futmondo
    no_futmondo <- filter(t, Comprador!="futmondo" & Vendedor!="futmondo")
    # select the Comprador and Vendedor columns and call them "from", "to"
    networkData <- tibble(from=no_futmondo[,"Comprador"],to=no_futmondo[,"Vendedor"])
    # group by "from" abd "to" and count the pair frequency, creating a new column Freq
    networkData <- networkData %>%  group_by(from, to)  %>% summarise(Freq = n())
    
    output$asociacionesPlot <- renderForceNetwork({
        simpleNetwork(networkData, height="100px", width="100px",        
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
    
    output$jugadorGraph <- renderForceNetwork({
         simpleNetwork(rv$jugadorNetworkData, height="100px", width="100px",        
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
}

# Run the application 
shinyApp(ui = ui, server = server)


