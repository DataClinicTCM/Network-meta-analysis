
# 加载包 ---------------------------------------------------------------------
library(gemtc)
library(tidyverse)
library(readxl)
library(igraph)
# 读取数据 --------------------------------------------------------------------
mydata_1 <- readxl::read_xlsx(path = 'data/Outcome_indicators.xlsx', sheet = 1)
# 获取输出文件夹目录
sheets <- excel_sheets("data/Outcome_indicators.xlsx")
# 批量创建结局指标文件夹
# for (i in 1:length(sheets)) {
# name <- paste0('./output/', sheets[i])
# dir.create(name)
# }

#  --------------------------------------------------------------------
string <- unique(mydata_1$treatment) #看一下有多少不一样的干预
length(unique(mydata_1$treatment)) #不一样的干预个数
pattern <- "[A-Z]{0,}[_]?ACEI" #ACEI的干预
intervene <- str_subset(string, pattern)
mydata_1[mydata_1$treatment %in% intervene,] #提取
# 将提取的数据命名
mydata_1_ACEI <- mydata_1[mydata_1$treatment %in% intervene,]

# 創建整体网络
network3 <- mtc.network(data.ab = mydata_3)
plot(network3, dynamic.edge.width=TRUE, vertex.color = '#FD1013', 
     vertex.label.color = '#222831', vertex.frame.color = '#393E46', 
     vertex.label.family = 'serif', vertex.label.cex = 1, vertex.label.dist = 2, 
     vertex.label.degree = pi/2, edge.color = '#dddfe6', layout = layout_in_circle, vertex.size=seq(1:16))#点的大小可以改，提取数据赋值即可
# 创建网络
network_1_ACEI <- mtc.network(data.ab = mydata_1_ACEI)
# 画图保存
name <- paste0('./output/', sheets[1], '/ACEI_network.tiff')
tiff(file = name, compression = 'none')
plot(network_3_CCB, dynamic.edge.width=TRUE, vertex.color = '#FD1013', 
     vertex.label.color = '#222831', vertex.frame.color = '#393E46', 
     vertex.label.family = 'serif', vertex.label.cex = 1, vertex.label.dist = 2, 
     vertex.label.degree = pi/2, edge.color = '#dddfe6')
dev.off()

# 异质性分析
resultanohe <- mtc.anohe(network_1_ACEI)
summary(resultanohe)
# 画图保存
name1 <- paste0('./output/', sheets[1], '/ACEI_yizhi.tiff')
tiff(file = name1, compression = 'none', height = 800)
plot(summary(resultanohe))
dev.off()

# 一致性分析
# mtc.nodesplit.comparisons(network_1_ACEI) #没有可比较的结果,因为不存在闭合环
# resultnodesplit <-mtc.nodesplit(network_1_ACEI) #所以跑不出来
# plot(resultnodesplit)
# plot(summary(resultnodesplit))

# 建模，选择一致性模型（consistency），因为网络结构没有闭合
set.seed(1234)
model_1_ACEI <-mtc.model(network_1_ACEI, type="consistency", n.chain=4,likelihood=NULL,
                  link=NULL, linearModel="random") #随机还是固定
# 运行模拟
results_1_ACEI <- mtc.run(model_1_ACEI, n.adapt = 5000, n.iter = 20000, thin = 1)
summary(results_1_ACEI)

name2 <- paste0('./output/', sheets[1], '/ACEI_forest.tiff')
tiff(file = name2, compression = 'none')
forest(relative.effect(results_1_ACEI, "ACEI"))
dev.off()

name3 <- paste0('./output/', sheets[3], '/ARB_trace&density.tiff')
tiff(file = name3, compression = 'none',height = 1000, width = 1000)
# 轨迹及密度图
plot(results_3_CCB)
# topptx(filename = paste0('ARB', '.pptx'))
dev.off()


name4 <- paste0('./output/', sheets[1], '/ACEI_shoulian.tiff')
tiff(file = name4, compression = 'none', height = 480)
# 收敛性诊断图
gelman.plot(results_1_ACEI) 
dev.off()

gelman.diag(results_3_CCB)

ranks <- rank.probability(results_1_ACEI, preferredDirection= -1) #-1越低越好
print(ranks)
sucra(ranks)

name5 <- paste0('./output/', sheets[1], '/ACEI_ranks1.tiff')
tiff(file = name5, compression = 'none', height = 480)
plot(ranks)
dev.off()

name6 <- paste0('./output/', sheets[1], '/ACEI_ranks2.tiff')
tiff(file = name6, compression = 'none', height = 480)
plot(ranks, beside = TRUE)
dev.off()



a <- relative.effect.table(results_3_CCB)
b <- round(relative.effect.table(results_3_CCB),2)
print(a)
print(b)
write.csv(b, file = paste0( 'b','.csv'))
forest(b, "CCB")











