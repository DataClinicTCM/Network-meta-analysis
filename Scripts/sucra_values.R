library(gemtc)
library(tidyverse)
library(readxl)

sucra_all <- function(i,xiyao){
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
  
  ranks <- rank.probability(results_xiyao, preferredDirection= 1) # -1越低越好
  
  a <- sucra(ranks)

  write.csv(a, file = paste0('./output/sucra/',sheets[i],'_', xiyao, '.csv'))
}


sucra_all(1, 'ACEI')
sucra_all(1, 'ARB')
sucra_all(1, 'CCB')

sucra_all(2, 'ACEI')
sucra_all(2, 'ARB')
sucra_all(2, 'CCB')

#越高越好
sucra_all(3, 'ACEI')
sucra_all(3, 'ARB')
sucra_all(3, 'CCB')


sucra_all(4, 'ACEI')#没有ACEI
sucra_all(4, 'ARB')
sucra_all(4, 'CCB')

sucra_all(5, 'ACEI')
sucra_all(5, 'ARB')#ARB不成网
sucra_all(5, 'CCB')

sucra_all(6, 'ACEI')
sucra_all(6, 'ARB')#ARB不成网
sucra_all(6, 'CCB')

sucra_all(7, 'ACEI')
sucra_all(7, 'ARB')#不成网
sucra_all(7, 'CCB')

# sucra_all(8, 'ACEI')#不成网
# sucra_all(8, 'ARB')#不成网
# sucra_all(8, 'CCB')#不成网

sucra_all(9, 'ACEI')
sucra_all(9, 'ARB')
sucra_all(9, 'CCB')

#越高越好
sucra_all(10, 'ACEI')#没有ACEI
sucra_all(10, 'ARB')
sucra_all(10, 'CCB')#不成网

#11分两个
sucra_all(11, 'CCB')
sucra_all(19, 'CCB')

# sucra_all(12, 'ACEI')#不成网
# sucra_all(12, 'ARB')#没有
# sucra_all(12, 'CCB')#不成网

#13分两层，不成网

#14分三层，不成网

sucra_all(15, 'CCB')#只有这一个

sucra_all(16, 'CCB')#只有这一个

sucra_all(17, 'ACEI')
sucra_all(17, 'ARB')#不成网
sucra_all(17, 'CCB')

sucra_all(18, 'CCB')#删除，从数据来看，NO不知道高低好
