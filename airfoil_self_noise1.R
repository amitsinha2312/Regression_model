#define path
path<-"G:/UCI/airfoil_self_noise_logistice"

#fix the path to source data
setwd(path)

#new data is nameed as "mydata" 
mydata <- read.csv("airfoil_self_noise.csv")

#finding the data tyoe of whole column 
str(mydata)

#checking for missing value
colSums(is.na(mydata))

#finding correlation relation
cor(mydata)

#lm is inbuilt function in R to define linear moodel
regmodel<-lm(Sound_pressure_level ~ .,data=mydata)

#checking for accuracy by term R^2(used to find the accuracy of model)
summary(regmodel)
#library

#fix the target place where diagram can be shown
par('mar')
par(mar=c(2,2,2,2))
#par(mfrow=c(1,1))
plot(regmodel)

#to update and find more accuracy of data,use log of target data
reregmodel<-update(regmodel,log(Sound_pressure_level)~.)
#clear(plot)
#set.seed(1)
summary(regmodel)

#let take 70% of training data use as for train and remaining use for testing the moel
set.seed(1)
d<-sample(x=nrow(mydata),size=nrow(mydata)*0.7)
train_new<-mydata[d,]
test_new<-mydata[-d,]

#train model
regmodel<-lm(log(Sound_pressure_level)~.,data=train_new)
summary(regmodel)

#test model
regpred<-predict(regmodel,test_new)
summary(regpred)

#convert back to original value
regpred<-exp(regpred)
summary(regpred)

#to find more accurate ,use boxplot functionto check outlier value
library(Metrics)
rmse(actual=test_new$Sound_pressure_level,predicted=regpred)
d<-boxplot(train_new$Displacement,varwidth = T,outline=T,border=T,plot=T)
d$out
