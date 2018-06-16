path<-"G:/UCI/titanic"
setwd(path)
install.pckages("data.table")
#load libraries and data
library(data.table)
library(plyr)
library(stringr)
train_titanic<-fread("train_titanic.csv",na.string=c(""," ",NA,"NA"))
test_titanic<-fread("test_titanic.csv",na.string=c(""," ",NA,"NA"))

head(train_titanic)
head(test_titanic)
str(train_titanic)

#check missing value
colSums(is.na(train_titanic))
colSums(is.na(test_titanic))

#quick data exploration
summary(train_titanic$Age)
summary(train_titanic)
summary(test_titanic$Age)

train_titanic[,.N/nrow(train_titanic),Pclass]
test_titanic[,.N/nrow(test_titanic),Pclass]

test_titanic[,.N/nrow(test_titanic),Sex]
train_titanic[,.N/nrow(train_titanic),Sex]

train_titanic[,.N/nrow(train_titanic),SibSp]
test_titanic[,.N/nrow(test_titanic),SibSp]

train_titanic[,.N/nrow(train_titanic),Parch]
test_titanic[,.N/nrow(test_titanic),Parch]

summary(train_titanic$Fare)
summary(test_titanic$Fare)

train_titanic[,.N/nrow(train_titanic),Cabin]
test_titanic[,.N/nrow(test_titanic),Cabin]

train_titanic[,.N/nrow(train_titanic),Embarked]
test_titanic[,.N/nrow(test_titanic),Embarked]

alldata<-rbind(train_titanic,test_titanic,fill=TRUE)

#New Variable
#Extract passengers title
alldata[,title := strsplit(Name,split ="[,.]")]
alldata[,title :=ldply(.data = title,.fun =function(x) x[2])]
alldata [,title :=str_trim(title,side="left")]

#combine titles
alldata[,title := replace(title,which(title %in% c("Capt","Col","Don","Jonkheer","Major","Rev","Sir")),"Mr"),by=title]
alldata[,title := replace(title,which(title %in% c("Lady","Mlle","Mme","Ms","the Countess","Dr","Dona")),"Mrs"),by=title]


#dealing with missing value
#ticket binary coding
alldata [,abs_col :=strsplit(x=Ticket,split =" ")]
alldata[,abs_col :=ldply(.data=abs_col,.fun=function(x)length(x))]
alldata[,abs_col :=ifelse(abs_col>1,1,0)]

#imput age with median whose value is not defined
for(i in "Age")
      set(alldata,i=which(is.na(alldata[[i]])),j=i,value=median(alldata$Age,na.rm=T))

#remove rows contating NA from embarked
alldata<-alldata[!is.na(Embarked)]

#impute Fare with Median
for(i in "Fare")
    set(alldata,i=which(is.na(alldata[[i]])),j=i,value=median(alldata$Fare,na.rm=T))

#replace missing values in cabin with "Miss"
alldata[is.na(Cabin),Cabin:="Miss"]

#LOg Transform fare
alldata$Fare<-log(alldata$Fare+1)

#Impute Parch 9 to 0
alldata[Parch==9L,Parch:=0]




#collect train and test
train<-alldata[!(is.na(Survived))]
train[,Survived:=as.factor(Survived)]

test<-alldata[is.na(Survived)]
test[,Survived:=NULL]

#logistic regression
model<-glm(Survived~ .,family=binomial(link='logit'),data=train[,-c("PassengerId","Name","Ticket")])
summary(model)

#run anova
anova(model,test='Chisq')

model2<-glm(Survived~Pclass+Sex+Age+SibSp+Fare+title,data=train,family = binomial(link="logit"))
summary(model2)

#compare two model
anova(model,model2,test="Chisq")

#partition and create training ,testing data
library(caret)
split<-createDataPartition(y=train$Survived,p=0.6,list=FALSE)
new_train<-train[split]
new_test<-train[-split]

#model training and prediction
log_model<-glm(Survived~ Pclass+Sex+Age+SibSp+Fare+title,data=new_train[,-c("PassengerId","Name","Ticket")],family=binomial(link="logit"))
log_predict<-predict(log_model,newdata = new_test,type="response")
log_predict<-ifelse(log_predict>0.5,1,0)

#plot roc
library(ROCR)
library(Metrics)
pr<-prediction(log_predict,new_test$Survived)
pref<-performance(pr,measure = "tpr",x.measure="fpr")
plot(pref)
auc(new_test$Survived,log_predict)

log_predict <- predict(log_model,newdata = new_test,type = "response")
log_predict <- ifelse(log_predict > 0.6,1,0)
pr <- prediction(log_predict,new_test$Survived)
perf <- performance(pr,measure = "tpr",x.measure = "fpr")
plot(perf)
auc(new_test$Survived,log_predict)
