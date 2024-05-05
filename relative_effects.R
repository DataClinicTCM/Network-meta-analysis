library(gemtc)
library(tidyverse)
library(readxl)

########只为降压有效率OR准备的比较表和森林图

relative_effects <- function(i,xiyao){
  # 读取数据 --------------------------------------------------------------------
  mydata <- readxl::read_xlsx(path = 'data/Outcome_indicators.xlsx', sheet = i)
  
  # 获取输出文件夹目录
  sheets <- excel_sheets("data/Outcome_indicators.xlsx")
  
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
  set.seed(5678)
  # 运行模拟
  results_xiyao <- mtc.run(model_xiyao, n.adapt = 5000, n.iter = 20000, thin = 1)
  
  set.seed(123)
  # 森林图
  name2 <- paste0('./output/', sheets[i], '/', xiyao, '_forest.tiff')
  tiff(file = name2, compression = 'none', width = 580, height = 580)
  forest(relative.effect(results_xiyao, paste0(xiyao)))
  dev.off()
  
  set.seed(147)
  relative.effect.table(results_xiyao)
  # round(relative.effect.table(results_xiyao),2)
  # summary(relative.effect(results_xiyao, "ACEI", c("SLXM_ACEI","TMGT_ACEI","XMT_ACEI")))
  # a <- round(exp(relative.effect.table(results_xiyao)), 2) #有效率的OR是取log的,所以要指数倒回来，其他的MD正常
  # write.csv(a, file = paste0('./output/', sheets[i], '/', xiyao, '.csv'))
}


relative_effects(1, 'ACEI')

relative_effects(3, 'ACEI')
relative_effects(3, 'ARB')
relative_effects(3, 'CCB')
