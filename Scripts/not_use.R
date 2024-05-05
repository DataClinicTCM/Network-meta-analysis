library(tidyverse)
library(readxl)

tiqu <- function(i,xiyao){
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
}

tiqu(3, 'ACEI')

mydata <- readxl::read_xlsx(path = 'data/Outcome_indicators.xlsx', sheet = 3)

# 获取输出文件夹目录
sheets <- excel_sheets("data/Outcome_indicators.xlsx")

string <- unique(mydata$treatment) #看一下有多少不一样的干预
length(unique(mydata$treatment)) #不一样的干预个数

pattern <- paste0("[A-Z]{0,}[_]?", 'CCB') #ACEI的干预
intervene <- str_subset(string, pattern)

# 将提取的数据命名
mydata_xiyao <- mydata[mydata$treatment %in% intervene,]


