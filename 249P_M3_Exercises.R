#Sherlin Mary Koshy(smkoshy) UCI MSWE-249P
#Reference : An Introduction to Statistical Learning by Gareth James,
#Daniela Witten, Trevor Hastie and Robert Tibshirani

#Applied Exercise 10
#Reference :https://stackoverflow.com/questions/48261387/how-to-conditionally-replace-values-in-r-data-frame-using-if-then-statement
#Reference used for determining how to split train and test for KNN on 1 predictor
library(ISLR)
names(Weekly)
dim(Weekly)
summary(Weekly)
pairs(Weekly)
cor(Weekly[,-9])
attach(Weekly)
plot(Volume)

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
             data=Weekly,family=binomial)
summary(glm.fits)
contrasts(Direction)

glm.probs=predict(glm.fits,type="response")
glm.pred=rep("Down",1089)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)
mean(glm.pred==Direction)

train=(Year<2009)
Weekly.0910=Weekly[!train,]
dim(Weekly.0910)
Direction.0910=Direction[!train]

glm.fits=glm(Direction~Lag2,data=Weekly,
             family=binomial,subset=train)
glm.probs=predict(glm.fits,Weekly.0910,type="response")
glm.pred=rep("Down",104)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.0910)
mean(glm.pred==Direction.0910)

glm.fits=glm(Direction~Lag1+Lag2,data=Weekly,
             family=binomial,subset=train)
glm.probs=predict(glm.fits,Weekly.0910,type="response")
glm.pred=rep("Down",104)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.0910)
mean(glm.pred==Direction.0910)

glm.fits=glm(Direction~Lag1+Lag2+Lag1:Lag2,data=Weekly,
             family=binomial,subset=train)
glm.probs=predict(glm.fits,Weekly.0910,type="response")
glm.pred=rep("Down",104)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.0910)
mean(glm.pred==Direction.0910)

glm.fits=glm(Direction~poly(Lag2,2),data=Weekly,
             family=binomial,subset=train)
glm.probs=predict(glm.fits,Weekly.0910,type="response")
glm.pred=rep("Down",104)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.0910)
mean(glm.pred==Direction.0910)

glm.fits=glm(Direction~log(Lag2),data=Weekly,
             family=binomial,subset=train)
glm.probs=predict(glm.fits,Weekly.0910,type="response")
glm.pred=rep("Down",104)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.0910)
mean(glm.pred==Direction.0910)

library(class)

train.X = Weekly[1:985,3, drop=FALSE]
test.X = Weekly[986:1089,3, drop=FALSE]
train.Direction=Direction[train]



set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.0910)
mean(knn.pred==Direction.0910)

knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.0910)
mean(knn.pred==Direction.0910)

#Applied Exercise 13
#Reference : https://rpubs.com/wru127/472376
library(MASS)
names(Boston)
attach(Boston)
crime01<-rep(0,length(crim))
crime01[crim > median(crim)] <- 1
Boston= data.frame(Boston,crime01)
cor(Boston)
train = 1:(dim(Boston)[1]/2)
test = (dim(Boston)[1]/2 + 1):dim(Boston)[1]
Boston.train = Boston[train, ]
Boston.test = Boston[test, ]
crime01.test = crime01[test]

set.seed(1)
Boston.fit <-glm(crime01~indus+nox+age+dis+rad+tax, data=Boston.train,family=binomial)
Boston.probs = predict(Boston.fit, Boston.test, type = "response")
Boston.pred = rep(0, length(Boston.probs))
Boston.pred[Boston.probs > 0.5] = 1
table(Boston.pred, crime01.test)

mean(Boston.pred != crime01.test)
summary(Boston.fit)

Boston.fit <-glm(crime01~lstat, data=Boston.train,family=binomial)
Boston.probs = predict(Boston.fit, Boston.test, type = "response")
Boston.pred = rep(0, length(Boston.probs))
Boston.pred[Boston.probs > 0.5] = 1
table(Boston.pred, crime01.test)

mean(Boston.pred != crime01.test)
summary(Boston.fit)

train.K=cbind(indus,nox,age,dis,rad,tax)[train,]
test.K=cbind(indus,nox,age,dis,rad,tax)[test,]
Bosknn.pred=knn(train.K, test.K, crime01.test, k=1)
table(Bosknn.pred,crime01.test)
mean(Bosknn.pred !=crime01.test)

train.K=cbind(indus,nox,age,dis,rad,tax)[train,]
test.K=cbind(indus,nox,age,dis,rad,tax)[test,]
Bosknn.pred=knn(train.K, test.K, crime01.test, k=50)
table(Bosknn.pred,crime01.test)
mean(Bosknn.pred !=crime01.test)