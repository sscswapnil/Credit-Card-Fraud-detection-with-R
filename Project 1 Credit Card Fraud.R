library(randomForest)
library(e1071)
library(rpart)
library(rpart.plot)
library(caTools)
card<-read.csv("creditcard.csv")
str(card)
card$Class<-factor(card$Class)
set.seed(1)
nrow(card)
split<-sample.split(card$Class,SplitRatio = 0.7)
train<-subset(card,split==T)
cv<-subset(card,split==F)
nrow(train)
nrow(cv)
table(cv$Class)
85265/(85265+148)
glm.model<-glm(Class~.,data=train,family = "binomial")
glm.predict<-predict(glm.model,cv,type = "response")
glm.predict
nrow(cv)
table(cv$Class,glm.predict>0.5)

cv$Class
glm.predict
cv$predict<-glm.predict
write.csv(cv,"checkcredit.csv")


getwd()



tree.model<-rpart(Class~.,data = train,method = "class",minbucket=20)
prp(tree.model)
tree.predict<-predict(tree.model,cv,type="class")
tree.predict
table(cv$Class,tree.predict)

