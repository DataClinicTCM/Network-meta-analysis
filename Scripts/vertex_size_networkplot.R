
library(gemtc)
library(tidyverse)
library(readxl)
library(igraph)
library(eoffice)

networkplot <- function(i){
  mydata <- readxl::read_xlsx(path = 'data/Outcome_indicators_new.xlsx', sheet = i)
  network <- mtc.network(data.ab = mydata)
  
  nodes <- network[['data.ab']]
  yaowu <- network[['treatments']]$description
  number <- vector(mode="numeric",length=0)
  for (x in 1:length(yaowu)){
    number[x] <- sum(nodes[nodes$treatment == yaowu[x],]$sampleSize)
  }
  print(sort(number))
  plot(network, dynamic.edge.width=TRUE, vertex.color = '#DE3C3C', 
       vertex.label.color = '#222831', vertex.frame.color = "#222831", 
       vertex.label.family = 'serif', vertex.label.cex = 0.8, vertex.label.dist = 1.5, 
       vertex.label.degree = pi/2, edge.color = '#222831', layout = layout_in_circle,
       vertex.size = number/10)
  
  # topptx(filename = paste0('network',i,'.pptx'))
}

networkplot(1)
networkplot(2)
networkplot(3)
networkplot(4)
networkplot(5)
networkplot(6)
networkplot(7)

map(1:7, networkplot)
networkplot(9)
networkplot(10)
networkplot(11)
networkplot(15)
networkplot(16)
networkplot(17)
