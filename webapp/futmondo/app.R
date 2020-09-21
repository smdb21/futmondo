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

# read data
t = read.csv("www/compras.txt",sep = "\t", encoding ="UTF-8")
t$Fecha <- as.POSIXct(t$Fecha,format="%d/%m/%Y - %H:%M:%S")
setorderv(t,c("Fecha"),order = 1)
num_colores = nrow(distinct(t, t[,"Comprador"]))
colores <- primary.colors(num_colores, steps = 10, no.white = TRUE)

#  
#
ui <- navbarPage("Futmondo - smdb21",
    tabPanel("Tabla de fichajes",
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
                 sidebarPanel(
                     selectInput("usuarioTabla", "Filtra tabla por Usuario:", c("Todos", distinct(t, t[,"Comprador"])), selected = TRUE)
                 ),
                 
                 # Show the table
                 mainPanel(
                     dataTableOutput("tablaFichajes")
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

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
   
    # remove last column with url
    tabla_compras <- t[,c(0:5)]
   
    
    output$tablaFichajes <- renderDataTable({
                                        if (input$usuarioTabla != "Todos") {
                                            filter(tabla_compras,Comprador==input$usuarioTabla | Vendedor==input$usuarioTabla)
                                        }else{
                                            tabla_compras
                                        }
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
}

# Run the application 
shinyApp(ui = ui, server = server)
