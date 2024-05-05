library(netmeta)
library(stringr)

# save pdf-plot as 5.58*5.10 inches(device size)

# example -----------------------------------------------------------------

data(Gurusamy2011) # 自带数据集
head(Gurusamy2011) # 查看数据

p1 = pairwise(treatment, 
              death,
              n, 
              studlab = study,
              data = Gurusamy2011, 
              sm = "OR")

IV1 = netmetabin(p1, 
                 ref = "Control/Placebo",
                 method = "Inverse",
                 comb.random = FALSE)

netgraph(IV1, 
         seq = "optimal", 
         col = "#BFBFBF",
         plastic = F,
         points = TRUE,
         pch = 21,
         cex.points =10*sqrt(n.trts/max(n.trts)),
         col.points = "#BFBFBF",
         bg.points = "#5C8286", 
         multiarm = FALSE,
         number.of.studies = TRUE)

forest(IV1,
       drop = TRUE,
       col.square = "#5C8286",
       sortvar = TE)

# myself data -------------------------------------------------------------

mydata <- readxl::read_xlsx(path = 'data/Outcome_indicators_new.xlsx', sheet = 1)

string <- unique(mydata$treatment) #看一下有多少不一样的干预
length(unique(mydata$treatment)) #不一样的干预个数

pattern <- paste0("[A-Z]{0,}[_]?", "ACEI") # xiyao的干预
intervene <- str_subset(string, pattern)

# 将提取的数据命名
mydata_ACEI <- mydata[mydata$treatment %in% intervene,]


p2 <- pairwise(treat = mydata_ACEI$treatment,
               n = mydata_ACEI$sampleSize,
               mean = mydata_ACEI$mean,
               sd = mydata_ACEI$std.dev,
               studlab = mydata_ACEI$study,
               data = mydata_ACEI)
IV2 <- netmeta(p2)
# Error: Network consists of 3 separate sub-networks.
# Use R function 'netconnection' to identify sub-networks.

netgraph(IV2, 
         seq = "optimal", 
         col = "#BFBFBF",
         plastic = F,
         points = TRUE,
         pch = 21,
         cex.points =10*sqrt(n.trts/max(n.trts)),
         col.points = "#BFBFBF",
         bg.points = "#5C8286", 
         multiarm = FALSE,
         number.of.studies = TRUE)

# rewrite a function(结局连续变量) ------------------------------------------------------

mynetgraph <- function(i, xiyao){
  mydata <- readxl::read_xlsx(path = 'data/Outcome_indicators_new.xlsx', sheet = i)
  
  string <- unique(mydata$treatment) #看一下有多少不一样的干预
  pattern <- paste0("[A-Z]{0,}[_]?", xiyao) # xiyao的干预
  intervene <- str_subset(string, pattern) # 某一类xiyao的所有干预
  
  # 将提取的数据命名
  mydata_xiyao <- mydata[mydata$treatment %in% intervene,]
  
  
  p2 <- pairwise(treat = mydata_xiyao$treatment,
                 n = mydata_xiyao$sampleSize,
                 mean = mydata_xiyao$mean,
                 sd = mydata_xiyao$std.dev,
                 studlab = mydata_xiyao$study,
                 data = mydata_xiyao)
  IV2 <- netmeta(p2)
  
  netgraph(IV2, 
           seq = "optimal", 
           col = "#BFBFBF",
           plastic = F,
           points = TRUE,
           pch = 21,
           cex.points =10*sqrt(n.trts/max(n.trts)),
           col.points = "#BFBFBF",
           bg.points = "#5C8286", 
           multiarm = FALSE,
           number.of.studies = TRUE)
}

# -------------------------------------------------------------------------

mynetgraph(1, "ACEI")
mynetgraph(1, "ARB")
mynetgraph(1, "CCB")

# mynetgraph(2, "ACEI")# 跟1一样

# 3降压有效率和9不良反应事件是二分类结局，计算方法不一样

# mynetgraph(4, "ACEI")#没有ACEI
mynetgraph(4, "ARB")
mynetgraph(4, "CCB")

mynetgraph(5, "ACEI")
mynetgraph(5, "ARB") #not network
mynetgraph(5, "CCB")

#6跟5一样

mynetgraph(7, "ACEI")
mynetgraph(7, "ARB") #not network
mynetgraph(7, "CCB")

# 8 not network

mynetgraph(10, "ACEI")#无ACEI
mynetgraph(10, "ARB")
mynetgraph(10, "CCB")#not network

mynetgraph(11, "ACEI")#not network
mynetgraph(11, "ARB")#无ARB
mynetgraph(11, "CCB")

# 12 13 14没有

mynetgraph(15, "ACEI")#无
mynetgraph(15, "ARB")#无
mynetgraph(15, "CCB")

# 16与15一样

mynetgraph(17, "ACEI")
mynetgraph(17, "ARB")#not network
mynetgraph(17, "CCB")

# 二分类结局函数(3,9) -----------------------------------------------------------------

mynetgraph_binary <- function(i, xiyao){
  mydata <- readxl::read_xlsx(path = 'data/Outcome_indicators_new.xlsx', sheet = i)
  
  string <- unique(mydata$treatment) #看一下有多少不一样的干预
  pattern <- paste0("[A-Z]{0,}[_]?", xiyao) # xiyao的干预
  intervene <- str_subset(string, pattern) # 某一类xiyao的所有干预
  
  # 将提取的数据命名
  mydata_xiyao <- mydata[mydata$treatment %in% intervene,]
  
  
  p2 <- pairwise(treat = mydata_xiyao$treatment,
                 n = mydata_xiyao$sampleSize,
                 event = mydata_xiyao$responders,
                 studlab = mydata_xiyao$study,
                 data = mydata_xiyao)
  IV2 <- netmeta(p2)
  
  netgraph(IV2, 
           seq = "optimal", 
           col = "#BFBFBF",
           plastic = F,
           points = TRUE,
           pch = 21,
           cex.points =10*sqrt(n.trts/max(n.trts)),
           col.points = "#BFBFBF",
           bg.points = "#5C8286", 
           multiarm = FALSE,
           number.of.studies = TRUE)
}


mynetgraph_binary(3, "ACEI")
mynetgraph_binary(3, "ARB")
mynetgraph_binary(3, "CCB")

mynetgraph_binary(9, "ACEI")
mynetgraph_binary(9, "ARB")
mynetgraph_binary(9, "CCB")
