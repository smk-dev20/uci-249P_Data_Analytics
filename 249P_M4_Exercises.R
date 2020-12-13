#Sherlin Mary Koshy(smkoshy) UCI MSWE-249P
#Reference : An Introduction to Statistical Learning by Gareth James,
#Daniela Witten, Trevor Hastie and Robert Tibshirani

#Applied Exercise 7
library(MASS)
set.seed(1)
train=sample(1:nrow(Boston),nrow(Boston)/2)

set.seed (1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,
                       mtry=6, ntree=25, importance =TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
m25=mean((yhat.rf-boston.test)^2)

rf.boston=randomForest(medv~.,data=Boston,subset=train,
                       mtry=6, ntree=50, importance =TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
m50=mean((yhat.rf-boston.test)^2)

rf.boston=randomForest(medv~.,data=Boston,subset=train,
                       mtry=6, ntree=100, importance =TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
m100=mean((yhat.rf-boston.test)^2)

rf.boston=randomForest(medv~.,data=Boston,subset=train,
                       mtry=6, ntree=250, importance =TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
m250=mean((yhat.rf-boston.test)^2)

rf.boston=randomForest(medv~.,data=Boston,subset=train,
                       mtry=6, ntree=500, importance =TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
m500=mean((yhat.rf-boston.test)^2)

n_trees = c(25,50,100,250,500)
means = c(m25,m50,m100,m250,m500)
plot(n_trees,means,type="l")

rf.boston=randomForest(medv~.,data=Boston,subset=train,
                       mtry=2, ntree=50, importance =TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
mt2=mean((yhat.rf-boston.test)^2)

rf.boston=randomForest(medv~.,data=Boston,subset=train,
                       mtry=4, ntree=50, importance =TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
mt4=mean((yhat.rf-boston.test)^2)

rf.boston=randomForest(medv~.,data=Boston,subset=train,
                       mtry=6, ntree=50, importance =TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
mt6=mean((yhat.rf-boston.test)^2)

rf.boston=randomForest(medv~.,data=Boston,subset=train,
                       mtry=8, ntree=50, importance =TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
mt8=mean((yhat.rf-boston.test)^2)

rf.boston=randomForest(medv~.,data=Boston,subset=train,
                       mtry=10, ntree=50, importance =TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
mt10=mean((yhat.rf-boston.test)^2)

rf.boston=randomForest(medv~.,data=Boston,subset=train,
                       mtry=12, ntree=50, importance =TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
mt12=mean((yhat.rf-boston.test)^2)

m_tries = c(2,4,6,8,10,12)
mt_means = c(mt2,mt4,mt6,mt8,mt10,mt12)
plot(m_tries,mt_means,type="l")


#Applied Exercise 8
library(ISLR)
attach(Carseats)
set.seed (1)
#8(a)
train=sample(1:nrow(Carseats),nrow(Carseats)/2)
#8(b)
tree.carseats=tree(Sales~.,Carseats,subset=train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)

yhat=predict(tree.carseats,newdata=Carseats[-train,])
carseats.test=Carseats[-train,"Sales"]
plot(yhat,carseats.test)
abline(0,1)
mean((yhat-carseats.test)^2)
#8(c)
cv.carseats=cv.tree(tree.carseats)
cv.carseats
plot(cv.carseats$size,cv.carseats$dev,type="b")

prune.carseats =prune.tree(tree.carseats,best=12)
plot(prune.carseats)
text(prune.carseats,pretty=0)
yhat=predict(prune.carseats,newdata=Carseats[-train,])
carseats.prune.test=Carseats[-train,"Sales"]
plot(yhat,carseats.prune.test)
abline(0,1)
mean((yhat-carseats.prune.test)^2)

#names(Carseats)
#8(d)
library(randomForest)
set.seed(1)
bag.carseats=randomForest(Sales~.,data=Carseats,subset=train,
                           mtry=10, importance=TRUE)
bag.carseats

yhat.bag=predict(bag.carseats,newdata=Carseats[-train,])
plot(yhat.bag,carseats.test)

mean((yhat.bag-carseats.test)^2)
importance(bag.carseats)

#8(e)
set.seed(1)
rf.carseats=randomForest(Sales~.,data=Carseats,subset=train,
                          mtry=3,importance =TRUE)
yhat.rf = predict(rf.carseats,newdata=Carseats[-train,])
mean((yhat.rf-carseats.test)^2)

set.seed(1)
rf.carseats=randomForest(Sales~.,data=Carseats,subset=train,
                         mtry=5,importance =TRUE)
yhat.rf = predict(rf.carseats,newdata=Carseats[-train,])
mean((yhat.rf-carseats.test)^2)

set.seed(1)
rf.carseats=randomForest(Sales~.,data=Carseats,subset=train,
                         mtry=8,importance =TRUE)
yhat.rf = predict(rf.carseats,newdata=Carseats[-train,])
mean((yhat.rf-carseats.test)^2)

importance(rf.carseats)

#Applied Exercise 9
attach(OJ)
names(OJ)
dim(OJ)
set.seed(2)
#9(a)
train=sample(1: nrow(OJ),800)
OJ.test=OJ[-train,]
Purchase.test=OJ.test$Purchase
length(Purchase.test)

#9(b)
set.seed(1)
tree.OJ=tree(Purchase~.,OJ,subset=train)
summary(tree.OJ)
#9(c)
tree.OJ
#9(d)
plot(tree.OJ)
text(tree.OJ,pretty=0)
#9(e)
tree.pred=predict(tree.OJ,OJ.test,type ="class")
table(tree.pred,Purchase.test)
(37+15)/270

#9(f)
set.seed(3)
cv.OJ=cv.tree(tree.OJ ,FUN=prune.misclass)
names(cv.OJ) 
cv.OJ
#9(g)
plot(cv.OJ$size,cv.OJ$dev, type="b")
#9(i)
prune.OJ=prune.misclass(tree.OJ,best =4)
plot(prune.OJ)
text(prune.OJ,pretty=0)
#9(j)
summary(prune.OJ)
#9(k)
tree.pred=predict(prune.OJ,OJ.test,type="class")
table(tree.pred,Purchase.test)
(42+14)/270


#Applied Exercise 10
?Hitters
attach(Hitters)
#10(a)
dim(Hitters)
Hitters = na.omit(Hitters)
dim(Hitters)
names(Hitters)
Hitters$Salary<-log(Hitters$Salary)

#10(b)
train = c(1:200)
Hitters.train=Hitters[train, ]
Hitters.test=Hitters[-train, ]

#10(c&d)
set.seed(1)
shrinkages = c(0.001,0.1,0.2,0.25)
boost.Hitters001 =gbm(Salary~.,data=Hitters[train,], distribution=
                    "gaussian",n.trees=1000 , interaction.depth =4, 
                    shrinkage =0.001,verbose=F)
train.pred=predict(boost.Hitters001, Hitters.train, n.trees=1000)
mean001=mean((train.pred-Hitters.train$Salary)^2)
yhat.boost001=predict(boost.Hitters001,Hitters.test,
                    n.trees =1000)

tmean001 = mean((yhat.boost001-Hitters.test$Salary)^2)

set.seed(1)
boost.Hitters1 =gbm(Salary~.,data=Hitters[train,], distribution=
                        "gaussian",n.trees=1000 , interaction.depth =4, 
                      shrinkage =0.1,verbose=F)
train.pred=predict(boost.Hitters1, Hitters.train, n.trees=1000)
mean1=mean((train.pred-Hitters.train$Salary)^2)
yhat.boost1=predict(boost.Hitters1,Hitters.test,
                      n.trees =1000)
tmean1 = mean((yhat.boost1-Hitters.test$Salary)^2)

set.seed(1)
boost.Hitters2 =gbm(Salary~.,data=Hitters[train,], distribution=
                        "gaussian",n.trees=1000 , interaction.depth =4, 
                      shrinkage =0.2,verbose=F)
train.pred=predict(boost.Hitters2, Hitters.train, n.trees=1000)
mean2=mean((train.pred-Hitters.train$Salary)^2)
yhat.boost2=predict(boost.Hitters2,Hitters.test,
                      n.trees =1000)
tmean2 = mean((yhat.boost2-Hitters.test$Salary)^2)
set.seed(1)
boost.Hitters25 =gbm(Salary~.,data=Hitters[train,], distribution=
                        "gaussian",n.trees=1000 , interaction.depth =4, 
                      shrinkage =0.25,verbose=F)
train.pred=predict(boost.Hitters25, Hitters.train, n.trees=1000)
mean25=mean((train.pred-Hitters.train$Salary)^2)
yhat.boost25=predict(boost.Hitters25,Hitters.test,
                      n.trees =1000)
tmean25 = mean((yhat.boost25-Hitters.test$Salary)^2)

train.errors = c(mean001,mean1,mean2,mean25)
test.errors = c(tmean001,tmean1,tmean2,tmean25)
plot(shrinkages,train.errors,type="b")
plot(shrinkages,test.errors,type="b")

lm.fit =lm(Salary ~.,data=Hitters.train)
lm.pred =predict(lm.fit,Hitters.test)
mean((Hitters.test$Salary - lm.pred)^2)
test.errors

#10(f)
summary(boost.Hitters1)

#10(g)
library(randomForest)
set.seed(1)
bag.Hitters =randomForest(Salary~.,data=Hitters.train,
                           mtry=19, importance =TRUE)
yhat.bag = predict(bag.Hitters,Hitters.test)
mean(( yhat.bag-Hitters.test$Salary)^2)


#Applied Exercise 12
set.seed (1)
rf.Hitters =randomForest(Salary~.,data=Hitters.train,
                          mtry=6, importance =TRUE)
yhat.rf = predict(rf.Hitters,Hitters.test)
mean((yhat.rf -Hitters.test$Salary)^2)
savehistory()