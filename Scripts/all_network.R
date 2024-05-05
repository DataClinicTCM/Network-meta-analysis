library(readxl)
mydata_1 <- readxl::read_xlsx(path = 'data/Outcome_indicators.xlsx', sheet = 1)

no_ACEI <- sum(mydata_1[mydata_1$treatment == 'ACEI',]$sampleSize)
no_ARB <- sum(mydata_1[mydata_1$treatment == 'ARB',]$sampleSize)
no_CCB <- sum(mydata_1[mydata_1$treatment == 'CCB',]$sampleSize)

no_SLXM_ACEI <- sum(mydata_1[mydata_1$treatment == 'SLXM_ACEI',]$sampleSize)

no_herb_xiyao <- function(i,herb, xiyao){
  mydata <- readxl::read_xlsx(path = 'data/Outcome_indicators.xlsx', sheet = i)
  print(sum(mydata[mydata$treatment == paste0(herb, '_', xiyao),]$sampleSize))
}
no_herb_xiyao(1,'SLXM',"ACEI")
no_herb_xiyao(1,'TMGT',"ACEI")
no_herb_xiyao(1,'XMT',"ACEI")
no_herb_xiyao(1,'SLXM',"ARB")
no_herb_xiyao(1,'TMGT',"ARB")
no_herb_xiyao(1,'XMT',"ARB")
no_herb_xiyao(1,'QLDX',"ARB")
no_herb_xiyao(1,'SLXM',"CCB")
no_herb_xiyao(1,'QLDX',"CCB")
no_herb_xiyao(1,'XMT',"CCB")
no_herb_xiyao(1,'QJDH',"CCB")
no_herb_xiyao(1,'TMGT',"CCB")
no_herb_xiyao(1,'QGJY',"CCB")

mydata_1 %>% group_by(treatment) %>% summarise(n = n())


# -------------------------------------------------------------------------
library(eoffice)
library(readxl)
library(gemtc)
library(igraph)



networkplot <- function(i){
  mydata <- readxl::read_xlsx(path = 'data/Outcome_indicators.xlsx', sheet = i)
  network <- mtc.network(data.ab = mydata)
  plot(network, dynamic.edge.width=TRUE, vertex.color = '#FD1013', 
       vertex.label.color = '#222831', vertex.frame.color = '#393E46', 
       vertex.label.family = 'serif', vertex.label.cex = 0.8, vertex.label.dist = 1.5, 
       vertex.label.degree = pi/2, edge.color = '#bac2c6', layout = layout_in_circle)
  
  topptx(filename = paste0('network',i,'.pptx'))
}
networkplot(1)
networkplot(2)
map(1:7, networkplot)
networkplot(9)
networkplot(10)
networkplot(11)
networkplot(15)
networkplot(16)
networkplot(17)

