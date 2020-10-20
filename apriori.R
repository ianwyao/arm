library(viridis)
library(arules)
library(TSP)
library(data.table)
#library(ggplot2)
#library(Matrix)
library(tcltk)
library(dplyr)
library(devtools)
library(purrr)
library(tidyr)
library(arulesViz)

df <- read.csv('artist_popularity_4.csv')
df1 <-  df[!duplicated(df[,'artist_mb']),]
#df1$artist_mb <- as.factor(df1$artist_mb)]

s <- strsplit(df$tags_mb, split = ";")
df1 <- data.frame(V1 = rep(df$artist_mb, sapply(s, length)), V2 = unlist(s))
# df1 <- df1[complete.cases(df1), ]


trans <- as(split(df1[,"V2"], df1[,"V1"]), "transactions")
#df1$tags_mb <- gsub(";",",",df1$tags_mb)
#artists <- subset(df1$artist_mb, df1$tags_mb != '')
#tag <- subset(df1$tags_mb, df1$tags_mb != '')

#Transactions_by_Artists <- split(tag, artists)

Art_Rule = arules::apriori(trans, parameter = list(supp=.01, conf=.01, minlen=2))
inspect(Art_Rule)

## Plot of which items are most frequent
itemFrequencyPlot(trans, topN=20, type="absolute")

## Sort rules by a measure such as conf, sup, or lift
SortedRulesK <- sort(Art_Rule, by="lift", decreasing=TRUE)
inspect(SortedRulesK[1:20])
(summary(SortedRulesK))

## Visualize
## tcltk

subrulesK <- head(sort(SortedRulesK, by="lift"),10)
plot(subrulesK)

plot(subrulesK, method="graph", engine="interactive")





SortedRules_lift <- sort(Art_Rule, by="lift", decreasing=TRUE)
inspect(SortedRules_lift[1:47])
art_rules<-SortedRules_lift[1:47]
inspect(art_rules)

Rules_DF2<-DATAFRAME(art_rules, separate = TRUE)
(head(Rules_DF2))
str(Rules_DF2)
## Convert to char
Rules_DF2$LHS<-as.character(Rules_DF2$LHS)
Rules_DF2$RHS<-as.character(Rules_DF2$RHS)


## Remove all {}
Rules_DF2[] <- lapply(Rules_DF2, gsub, pattern='[{]', replacement='')
Rules_DF2[] <- lapply(Rules_DF2, gsub, pattern='[}]', replacement='')

head(Rules_DF2)


###########################################
###### Do for SUp, Conf, and Lift   #######
###########################################
## Remove the sup, conf, and count
## USING LIFT
Rules_L<-Rules_DF2[c(1,2,5)]
names(Rules_L) <- c("SourceName", "TargetName", "Weight")
head(Rules_L,30)

## USING SUP
Rules_S<-Rules_DF2[c(1,2,3)]
names(Rules_S) <- c("SourceName", "TargetName", "Weight")
head(Rules_S,30)

## USING CONF
Rules_C<-Rules_DF2[c(1,2,4)]
names(Rules_C) <- c("SourceName", "TargetName", "Weight")
head(Rules_C,30)

## CHoose and set
#Rules_Sup<-Rules_C
Rules_Sup<-Rules_L
#Rules_Sup<-Rules_S

###########################################################################
#############       Build a NetworkD3 edgeList and nodeList    ############
###########################################################################

#edgeList<-Rules_Sup
# Create a graph. Use simplyfy to ensure that there are no duplicated edges or self loops
#MyGraph <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=TRUE))
#plot(MyGraph)

############################### BUILD THE NODES & EDGES ####################################
(edgeList<-Rules_Sup)
MyGraph <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=TRUE))

nodeList <- data.frame(ID = c(0:(igraph::vcount(MyGraph) - 1)), 
                       # because networkD3 library requires IDs to start at 0
                       nName = igraph::V(MyGraph)$name)
## Node Degree
(nodeList <- cbind(nodeList, nodeDegree=igraph::degree(MyGraph, 
                                                       v = igraph::V(MyGraph), mode = "all")))

## Betweenness
BetweenNess <- igraph::betweenness(MyGraph, 
                                   v = igraph::V(MyGraph), 
                                   directed = TRUE) 

(nodeList <- cbind(nodeList, nodeBetweenness=BetweenNess))

## This can change the BetweenNess value if needed
BetweenNess<-BetweenNess/100



## For scaling...divide by 
## RE:https://en.wikipedia.org/wiki/Betweenness_centrality
##/ ((igraph::vcount(MyGraph) - 1) * (igraph::vcount(MyGraph)-2))
## For undirected / 2)
## Min-Max Normalization
##BetweenNess.norm <- (BetweenNess - min(BetweenNess))/(max(BetweenNess) - min(BetweenNess))


## Node Degree


###################################################################################
########## BUILD THE EDGES #####################################################
#############################################################
# Recall that ... 
# edgeList<-Rules_Sup
getNodeID <- function(x){
  which(x == igraph::V(MyGraph)$name) - 1  #IDs start at 0
}
(getNodeID(" uk")) 

edgeList <- plyr::ddply(
  Rules_Sup, .variables = c("SourceName", "TargetName" , "Weight"), 
  function (x) data.frame(SourceID = getNodeID(x$SourceName), 
                          TargetID = getNodeID(x$TargetName)))

head(edgeList)
nrow(edgeList)

########################################################################
##############  Dice Sim ################################################
###########################################################################
#Calculate Dice similarities between all pairs of nodes
#The Dice similarity coefficient of two vertices is twice 
#the number of common neighbors divided by the sum of the degrees 
#of the vertices. Method dice calculates the pairwise Dice similarities 
#for some (or all) of the vertices. 
DiceSim <- igraph::similarity.dice(MyGraph, vids = igraph::V(MyGraph), mode = "all")
head(DiceSim)

#Create  data frame that contains the Dice similarity between any two vertices
F1 <- function(x) {data.frame(diceSim = DiceSim[x$SourceID +1, x$TargetID + 1])}
#Place a new column in edgeList with the Dice Sim3
head(edgeList)
edgeList <- plyr::ddply(edgeList,
                        .variables=c("SourceName", "TargetName", "Weight", 
                                     "SourceID", "TargetID"), 
                        function(x) data.frame(F1(x)))
head(edgeList)

##################################################################################
##################   color #################################################
######################################################
COLOR_P <- colorRampPalette(c("#00FF00", "#FF0000"), 
                            bias = nrow(edgeList), space = "rgb", 
                            interpolate = "linear")
COLOR_P
(colCodes <- COLOR_P(length(unique(edgeList$diceSim))))
edges_col <- sapply(edgeList$diceSim, 
                    function(x) colCodes[which(sort(unique(edgeList$diceSim)) == x)])
nrow(edges_col)

## NetworkD3 Object
#https://www.rdocumentation.org/packages/networkD3/versions/0.4/topics/forceNetwork
install.packages('networkD3')
D3_network_art <- networkD3::forceNetwork(
  Links = edgeList, # data frame that contains info about edges
  Nodes = nodeList, # data frame that contains info about nodes
  Source = "SourceID", # ID of source node 
  Target = "TargetID", # ID of target node
  Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
  NodeID = "nName", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
  Nodesize =  "nodeBetweenness",  # value from the node list (data frame) that contains value we want to use for a node size
  Group = "nodeDegree",  # value from the node list (data frame) that contains value we want to use for node color
  height = 700, # Size of the plot (vertical)
  width = 900,  # Size of the plot (horizontal)
  fontSize = 10, # Font size
  linkDistance = networkD3::JS("function(d) { return d.value*10; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
  linkWidth = networkD3::JS("function(d) { return d.value*10; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
  opacity = 1.0, # opacity
  zoom = TRUE, # ability to zoom when click on the node
  opacityNoHover = 1.0, # opacity of labels when static
  linkColour = "red"   ###"edges_col"red"# edge colors
) 

# Plot network
#D3_network_Tweets

# Save network as html file
networkD3::saveNetwork(D3_network_art, 
                       "NetD3_DCR2019_worldNewsL.html", selfcontained = TRUE)
install.packages('RColorBrewer')
library(RColorBrewer)
plot(SortedRulesK, control=list(jitter=2, col = rev(brewer.pal(9, 'Greens'))), shading = 'lift',method = 'grouped')
