library(gemtc)
library(tidyverse)
library(readxl)

# 反思 ----------------------------------------------------------------------
# 其实可以把这个函数更加智能化，加上异常纠正机制
# 从第一个sheet到最后一个sheet搭配每种西药自动循环
# -------------------------------------------------------------------------

if(F){
  # 获取输出文件夹目录
  sheets <- excel_sheets("data/Outcome_indicators.xlsx")
  # 批量创建结局指标文件夹
  for (i in 1:length(sheets)) {
    name <- paste0('./output/', sheets[i])
    dir.create(name)
  }
}


netmeta_all <- function(i,xiyao){
  # 读取数据 --------------------------------------------------------------------
  mydata <- readxl::read_xlsx(path = 'data/Outcome_indicators_new.xlsx', sheet = i)
  
  # 获取输出文件夹目录
  sheets <- excel_sheets("data/Outcome_indicators_new.xlsx")
  
  string <- unique(mydata$treatment) #看一下有多少不一样的干预
  length(unique(mydata$treatment)) #不一样的干预个数

  pattern <- paste0("[A-Z]{0,}[_]?", xiyao) #ACEI的干预
  intervene <- str_subset(string, pattern)
  
  # 将提取的数据命名
  mydata_xiyao <- mydata[mydata$treatment %in% intervene,]
  # 创建网络
  network_xiyao <- mtc.network(data.ab = mydata_xiyao)
  # 画图保存
  name <- paste0('./output/', sheets[i], '/', xiyao, '_network.tiff')
  tiff(file = name, compression = 'none')
  plot(network_xiyao, dynamic.edge.width=TRUE, vertex.color = '#FD7013', 
       vertex.label.color = '#222831', vertex.frame.color = '#393E46', 
       vertex.label.family = 'serif', vertex.label.cex = 1, vertex.label.dist = 2, 
       vertex.label.degree = pi/2, edge.color = '#dddfe6')
  dev.off()
  
  # 异质性分析
  resultanohe <- mtc.anohe(network_xiyao)
  summary(resultanohe)
  # 画图保存
  name1 <- paste0('./output/', sheets[i], '/', xiyao, '_yizhi.tiff')
  tiff(file = name1, compression = 'none', width = 580, height = 880)
  plot(summary(resultanohe))
  dev.off()
  
  # 一致性分析
  # mtc.nodesplit.comparisons(network_SBP_ACEI) #没有可比较的结果,因为不存在闭合环
  # resultnodesplit <-mtc.nodesplit(network_SBP_ACEI) #所以跑不出来
  # plot(resultnodesplit)
  # plot(summary(resultnodesplit))
  
  # 建模，选择一致性模型（consistency），因为网络结构没有闭合
  set.seed(1234)
  model_xiyao <-mtc.model(network_xiyao, type="consistency", n.chain=4,likelihood=NULL,
                             link=NULL, linearModel="random") #随机还是固定
  # 运行模拟
  results_xiyao <- mtc.run(model_xiyao, n.adapt = 5000, n.iter = 20000, thin = 1)
  summary(results_xiyao)
  
  # 森林图
  name2 <- paste0('./output/', sheets[i], '/', xiyao, '_forest.tiff')
  tiff(file = name2, compression = 'none', width = 580, height = 580)
  forest(relative.effect(results_xiyao, paste0(xiyao)))
  dev.off()
  
  # 轨迹及密度图
  name3 <- paste0('./output/', sheets[i], '/', xiyao, '_trace&density.pdf')
  pdf(file = name3)
  plot(results_xiyao)
  dev.off()
  
  # name3.1 <- paste0('./output/', sheets[i], '/', xiyao, '_trace&density.tiff')
  # tiff(file = name3.1, compression = 'none', height = 750, width = 800)
  # par(mfrow = c(7,2))
  # plot(results_xiyao, auto.layout = FALSE)
  # dev.off()
  
  # 收敛性诊断图
  name4 <- paste0('./output/', sheets[i], '/', xiyao, '_shoulian.pdf')
  pdf(file = name4)
  gelman.plot(results_xiyao)
  dev.off()
  
  ranks <- rank.probability(results_xiyao, preferredDirection= 1) #-1越低越好
  print(ranks)
  sucra(ranks)
  
  # 排序图
  name5 <- paste0('./output/', sheets[i], '/', xiyao, '_ranks1.tiff')
  tiff(file = name5, compression = 'none', height = 580, width = 580)
  plot(ranks)
  dev.off()
  
  name6 <- paste0('./output/', sheets[i], '/', xiyao, '_ranks2.tiff')
  tiff(file = name6, compression = 'none', height = 580, width = 580)
  plot(ranks, beside = TRUE)
  dev.off()
  
  # a <- relative.effect.table(results_xiyao)
  a <- round(relative.effect.table(results_xiyao),2)
  print(a)
  write.csv(a, file = paste0('./output/', sheets[i], '/', xiyao, '.csv'))
}

# -------------------------------------------------------------------------

netmeta_all(1, 'ACEI')
netmeta_all(1, 'ARB')
netmeta_all(1, 'CCB')

netmeta_all(2, 'ACEI')
netmeta_all(2, 'ARB')
netmeta_all(2, 'CCB')

#越高越好
netmeta_all(3, 'ACEI')
netmeta_all(3, 'ARB')
netmeta_all(3, 'CCB')


netmeta_all(4, 'ACEI')#没有ACEI
netmeta_all(4, 'ARB')
netmeta_all(4, 'CCB')

netmeta_all(5, 'ACEI')
netmeta_all(5, 'ARB')#ARB不成网
netmeta_all(5, 'CCB')

netmeta_all(6, 'ACEI')
netmeta_all(6, 'ARB')#ARB不成网
netmeta_all(6, 'CCB')

netmeta_all(7, 'ACEI')
netmeta_all(7, 'ARB')#不成网
netmeta_all(7, 'CCB')

# netmeta_all(8, 'ACEI')#不成网
# netmeta_all(8, 'ARB')#不成网
# netmeta_all(8, 'CCB')#不成网

netmeta_all(9, 'ACEI')
netmeta_all(9, 'ARB')
netmeta_all(9, 'CCB')

#越高越好
netmeta_all(10, 'ACEI')#没有ACEI
netmeta_all(10, 'ARB')
netmeta_all(10, 'CCB')#不成网

#11分两个
netmeta_all(11, 'CCB')

netmeta_all(12, 'ACEI')#不成网
netmeta_all(12, 'ARB')#没有
netmeta_all(12, 'CCB')#不成网

#13分两层，不成网

#14分三层，不成网

netmeta_all(15, 'CCB')#只有这一个

netmeta_all(16, 'CCB')#只有这一个

netmeta_all(17, 'ACEI')#不成网
netmeta_all(17, 'ARB')#不成网
netmeta_all(17, 'CCB')

netmeta_all(18, 'CCB')#其它两个太少不成网


# trace&density -----------------------------------------------------------

trace_density <- function(i,xiyao,h,r){
  # 读取数据 --------------------------------------------------------------------
  mydata <- readxl::read_xlsx(path = 'data/Outcome_indicators_new.xlsx', sheet = i)
  
  # 获取输出文件夹目录
  sheets <- excel_sheets("data/Outcome_indicators_new.xlsx")
  
  string <- unique(mydata$treatment) #看一下有多少不一样的干预
  length(unique(mydata$treatment)) #不一样的干预个数
  
  pattern <- paste0("[A-Z]{0,}[_]?", xiyao) #ACEI的干预
  intervene <- str_subset(string, pattern)
  
  # 将提取的数据命名
  mydata_xiyao <- mydata[mydata$treatment %in% intervene,]
  # 创建网络
  network_xiyao <- mtc.network(data.ab = mydata_xiyao)
  
  # 建模，选择一致性模型（consistency），因为网络结构没有闭合
  set.seed(1234)
  model_xiyao <-mtc.model(network_xiyao, type="consistency", n.chain=4,likelihood=NULL,
                          link=NULL, linearModel="random") #随机还是固定
  # 运行模拟
  results_xiyao <- mtc.run(model_xiyao, n.adapt = 5000, n.iter = 20000, thin = 1)
 
  # 轨迹及密度图
  name3.1 <- paste0('./output/', sheets[i], '/', xiyao, '_trace&density.tiff')
  tiff(file = name3.1, compression = 'none', height = h, width = 800)
  par(mfrow = c(r,2))
  plot(results_xiyao, auto.layout = FALSE)
  dev.off()
}

trace_density(1,'ACEI',450,4)
trace_density(1,'ARB',550,5)
trace_density(1,'CCB',750,7)

trace_density(2,'ACEI',450,4)
trace_density(2,'ARB',550,5)
trace_density(2,'CCB',750,7)

trace_density(3,'ACEI',450,4)
trace_density(3,'ARB',550,5)
trace_density(3,'CCB',750,7)

trace_density(9,'ACEI',450,4)
trace_density(9,'ARB',550,5)
trace_density(9,'CCB',750,7)

trace_density(17,'CCB',450,4)
