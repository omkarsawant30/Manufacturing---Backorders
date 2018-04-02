#Set the working directory and read the files through R
setwd('E:/DATA ANALYTICS JOURNEY/R Edvancer/PROJECT 3 MANUFACTURING')

wob_train=read.csv("product_train.csv",sep=",",header=T,nrows = 50000)
head(wob_train)
wob_test=read.csv("product_test.csv",sep=",",header=T)

##You will need same set of vars on both train and test, its easier to manage that if you combine train and test
##in the beginning and then separate them once you are done with data preparation
##We'll fill test's response column with NAs.
wob_test$went_on_backorder=NA

wob_train$data='train'
wob_test$data='test'
wob_all=rbind(wob_train,wob_test)

apply(wob_all,2,function(x) length(unique(x)))

library(dplyr)
library(magrittr)
wob_all=wob_all %>% select(-sku)

glimpse(wob_all)

##Next we'll create dummy variables for remaining categorical variables
##using sapply for creating dummies
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

###we can go ahead and separate training and test data BUT first we check NA values
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

#Using Random forest model to predict
library(randomForest)
fit = randomForest(as.factor(went_on_backorder)~., data = wob_train)

### Make predictions on test and submit 
test.predictions = predict(fit, newdata = wob_test)
write.csv(test.predictions,file = "manuf.csv", row.names = F)

#--------------------------------QUIZ-------------------------------------------------------------------------------
#(1) Find the difference between median of variables perf_6_month_avg and perf_12_month_avg when your target values is Yes
library(magrittr)
pro_train %>%
filter(went_on_backorder=="Yes") %>%
ok = median(pro_train$perf_6_month_avg)-median(pro_train$perf_12_month_avg)
ok
#Ans - 0
      
#(2) - Do a chisq test to see whether variable deck_risk affects your target variable. Write down p-value only, 
#for the test here. Round off your answer to 4 decimal digits.
chisq.test(pro_train$deck_risk,pro_train$went_on_backorder)

#Ans- 0.0012
      
#(3) - Find out the % zeros in the variable pieces_past_due . Round off your answer to two decimal digits
pro_train$pieces_past_due %>%
count(0)
#Ans - 99.13
      
#(4) - For building a classification model with random forest, what should be the type of your target variable?
#Ans - NA
      
#(5) - If there are 30 predictor variables in the data being considered for classification with function randomForest , 
#what would be default value of argument mtry.

#Ans - 5
      
#(6) - Which function can be used to remove objects from memory?
#Ans - rm
      
#(7) - Find the correlation coefficient between forecast_9_month and sales_9_month. Round it off to two decimal digits.
cor(pro_train$forecast_9_month,pro_train$sales_9_month)

#Ans - NA
      
#(8) - Is average min_bank significantly different across two target categories. Do a t-test and conclude
# according to p-value(answer should be the p-value only),round it off to 2 decimal places. Consider alpha=0.05.

#Ans - NA
      
#(9) - Which geometry function from package ggplot2 can be used to make histograms.
#Ans  - geom_histogram
      
#(10) - Which among these aesthetics ( shape , size, color) can be mapped to both categorical and numerical data?
#Ans - color
