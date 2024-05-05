library("TreeAndLeaf")
library("RedeR")
library("igraph")
library("RColorBrewer")
library(tidyverse)
df <- readxl::read_xlsx(path = 'Outcome_indicators.xlsx', sheet = 1)
df$ID <- paste(df$study, df$treatment, sep = '|')
df <- df[,c(-1,-2)]
df <- column_to_rownames(df, var = 'ID')
head(df)
dim(df)
#先构建hclust对象并查看进化树：
hc <- hclust(dist(df), "ave")# 先对df求距离矩阵dist(),默认欧几里得距离，然后层次聚类hclust
plot(hc, main="Dendrogram for the 'SBP' dataset", xlab="", sub="")
##1.将hc(hclust)对象转换为tree-and-leaf对象：
tal <- treeAndLeaf(hc)

##2.将属性映射到tree和leaf对象顶点(添加外部注释)：
tal2 <- att.mapv(g = tal,#igraph object
                 dat = df,#要设置的数据框
                 refcol = 0) #使用df的行名作为映射ID

##3.使用包装函数att.setv设置图形顶点属性：
pal <- brewer.pal(9, "Greens")#设置颜色
tal2 <- att.setv(g = tal2,
                 from = "mean",
                 to = "nodeColor",
                 cols = pal,
                 nquant = 5)
tal2 <- att.setv(g = tal2,
                 from = "sampleSize",
                 to = "nodeSize",
                 xlim = c(10, 50, 5),
                 nquant = 5)

##4.使用att.addv和att.adde函数设置图形属性：
tal2 <- att.addv(tal2, "nodeFontSize",
                 value = 15,
                 index = V(tal)$isLeaf)
tal2 <- att.adde(tal2,
                 "edgeWidth",
                 value = 3)

##5.调用RedeR（注意，电脑需要有java环境）：
rdp <- RedPort()
calld(rdp, checkcalls=TRUE)
resetd(rdp)
##6.在交互式 R/Java 界面中显示树形图：
addGraph(obj = rdp,
         g = tal2,
         gzoom=75)
##7.调用relax来微调叶节点（可在交互中调整到最适布局）：
relax(rdp,
      p1=25,
      p2=200,
      p3=5,
      p5=5,
      ps=TRUE)
##8.添加图例:
addLegend.color(obj = rdp,
                tal2,
                title = "Mean",
                position = "topright")
addLegend.size(obj = rdp,
               tal2, title = "Samplesize",
               position = "bottomright")
