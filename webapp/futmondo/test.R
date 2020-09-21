library(ggplot2)
library(data.table)
library(colorRamps)
library(scales)

t = read.csv("www/compras.txt",sep = "\t", encoding ="UTF-8")
t$Fecha <- as.Date(as.POSIXct(t$Fecha,format="%d/%m/%Y - %H:%M:%S"))
setorderv(t,c("Fecha"),order = 1)
num_colores = nrow(distinct(t, t[,"Comprador"]))
colores <- primary.colors(num_colores, steps = 10, no.white = TRUE)

ggplot(data=t, aes(x=Fecha, y=Precio, group=Comprador, color=Comprador)) +
  geom_histogram() +
  scale_x_date(labels = date_format("%Y-%b"),
               breaks = seq(min(t$Fecha)-5, max(t$Fecha)+5, 30))


library(igraph)
library(networkD3)

# we remove any transaction with futmondo
no_futmondo <- filter(t, Comprador!="futmondo" & Vendedor!="futmondo")
# select the Comprador and Vendedor columns and call them "from", "to"
networkData <- tibble(from=no_futmondo[,"Comprador"],to=no_futmondo[,"Vendedor"])
# group by "from" abd "to" and count the pair frequency, creating a new column Freq
networkData <- networkData %>%  group_by(from, to)  %>% summarise(Freq = n())
network <- simpleNetwork(networkData, height="100px", width="100px",        
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
network



