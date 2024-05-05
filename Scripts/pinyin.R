library(pinyin)
library(tidyverse)
library(readxl)

# test --------------------------------------------------------------------

testword <- c("兰红斌", "大大大", "师姐好的吧")

py('羌笛何须怨杨柳春风不度玉门关')
py('羌笛何须怨杨柳春风不度玉门关', sep = ' ')
py("羌笛何须怨杨柳春风不度玉门", dic = pydic(dic = c("pinyin2"))) %>% str_remove_all("\\d{1,}")

testpy <- py(testword, dic = pydic(dic = c("pinyin2"))) %>% str_remove_all("\\d{1,}")

# -------------------------------------------------------------------------

myname <- read_xlsx(path = 'data/每项RCT的偏倚风险评估.xlsx', sheet = 1)

nameword <- myname$`Author
Year`

# function-------------------------------------------------------------------------
split.string<-function(string){
  str2<-strsplit(string,"")[[1]]
  string.split<-NULL
  j<-1
  string.split[j]<-str2[1]
  find.type<-function(char){
    if(grepl("[[:alpha:]]",char))
      type<-"alphabet"
    else if(grepl("[[:digit:]]",char))
      type<-"digit"
    else type<-"chinese"
    type
  }
  type<-find.type(str2[1])
  for(i in 2:length(str2)){
    type2<-find.type(str2[i])
    if(type2==type) string.split[j]<-paste(string.split[j],str2[i],sep="")
    else{
      j<-j+1
      type<-type2
      string.split<-c(string.split,str2[i])
    }
  }
  string.split
}

split.string("兰红斌2015")

sapply(c('羌笛何须怨杨柳1234', '春风不度玉门关456',"是的跟45"), split.string)

# -------------------------------------------------------------------------

aa <- sapply(nameword, split.string)
class(aa)
aa <- t(aa)
aa <- as.data.frame(aa)
class(aa$V1)

bb <- str_split_fixed(aa$V1, pattern = "", n=3)
class(bb)
bb <- as.data.frame(bb)
name_py <- py(bb$V1, dic = pydic(dic = c("pinyin2"))) %>% str_remove_all("\\d{1,}")

str_to_title()
aa$V3 <- paste0(str_to_title(name_py), ". ", "et al", " ", aa$V2)
write.csv(aa, file = "data/namePY.csv")
