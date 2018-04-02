setwd('E:/DATA ANALYTICS JOURNEY/R Edvancer/PROJECT 3 MANUFACTURING')

wob_train=read.csv("product_train.csv",sep=",",header=T,nrows = 50000)
head(wob_train)
wob_test=read.csv("product_test.csv",sep=",",header=T)

wob_test$went_on_backorder=NA

wob_train$data='train'
wob_test$data='test'
wob_all=rbind(wob_train,wob_test)

apply(wob_all,2,function(x) length(unique(x)))

library(dplyr)
library(magrittr)
wob_all=wob_all %>% select(-sku)

glimpse(wob_all)

CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
} 
glimpse(wob_train)

head(wob_all)

wob_all=wob_all[!((is.na(wob_all$went_on_backorder)) & wob_all$data=='train'), ]
for(col in names(wob_all)){
  if(sum(is.na(wob_all[,col]))>0 & !(col %in% c("data","went_on_backorder"))){
    wob_all[is.na(wob_all[,col]),col]=mean(wob_all[wob_all$data=='train',col],na.rm=T)
  }
}

wob_train = wob_all %>% filter(data == 'train') %>% select(-data) 
wob_test= wob_all %>% filter(data == 'test') %>% select(-data-went_on_backorder) 


any(is.na(wob_train))
any(is.na(wob_test))

library(randomForest)
fit = randomForest(as.factor(went_on_backorder)~., data = wob_train)

### Make predictions on test and submit 
test.predictions = predict(fit, newdata = wob_test)
write.csv(test.predictions,file = "Omkar_Sawant_P3_part2.csv", row.names = F)

