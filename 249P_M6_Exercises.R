#Sherlin Mary Koshy(smkoshy) UCI MSWE-249P
#Reference : An Introduction to Statistical Learning by Gareth James,
#Daniela Witten, Trevor Hastie and Robert Tibshirani

#Applied Exercise 5
?Default
library(ISLR)
names(Default)
glm.fit = glm(default~balance+income, data=Default, family="binomial")
summary(glm.fit)

dim(Default)
attach(Default)
set.seed(1)
train=sample(10000,5000)
glm.fit = glm(default~balance+income, data=Default, subset=train, family="binomial")
summary(glm.fit)

glm.probs=predict(glm.fit,Default[-train,],type="response")
preds=rep("No",5000)
preds[glm.probs>0.5]="Yes"
mean(preds!=Default[-train,]$default)

set.seed(2)
train=sample(10000,5000)
glm.fit = glm(default~balance+income, data=Default, subset=train, family="binomial")
glm.probs=predict(glm.fit,Default[-train,],type="response")
preds=rep("No",5000)
preds[glm.probs>0.5]="Yes"
mean(preds!=Default[-train,]$default)

set.seed(3)
train=sample(10000,5000)
glm.fit = glm(default~balance+income, data=Default, subset=train, family="binomial")
glm.probs=predict(glm.fit,Default[-train,],type="response")
preds=rep("No",5000)
preds[glm.probs>0.5]="Yes"
mean(preds!=Default[-train,]$default)

set.seed(4)
train=sample(10000,5000)
glm.fit = glm(default~balance+income, data=Default, subset=train, family="binomial")
glm.probs=predict(glm.fit,Default[-train,],type="response")
preds=rep("No",5000)
preds[glm.probs>0.5]="Yes"
mean(preds!=Default[-train,]$default)

set.seed(1)
train=sample(10000,5000)
glm.fit = glm(default~balance+income+student, data=Default, subset=train, family="binomial")
summary(glm.fit)

glm.probs=predict(glm.fit,Default[-train,],type="response")
preds=rep("No",5000)
preds[glm.probs>0.5]="Yes"
mean(preds!=Default[-train,]$default)


#Applied Exercise 6
set.seed(1)
train=sample(10000,5000)
glm.fit = glm(default~balance+income, data=Default, family="binomial")
summary(glm.fit)

library(boot)
boot.fn=function (data ,index )
  return (coef(glm(default~balance+income,data=data,subset=index,family="binomial")))

boot(Default,boot.fn,1000)

#Applied Exercise 8 
set.seed (1)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
plot(x,y)

set.seed(1)
data=data.frame(x,y)
cv.error=rep(0,4)
for(i in 1:4){
  glm.fit=glm(y~poly(x,i))
  cv.error[i]=cv.glm(data,glm.fit)$delta[1]
}
cv.error

set.seed(17)
data=data.frame(x,y)
cv.error=rep(0,4)
for(i in 1:4){
  glm.fit=glm(y~poly(x,i))
  cv.error[i]=cv.glm(data,glm.fit)$delta[1]
}
cv.error

glm.fit=glm(y~poly(x,4))
summary(glm.fit)
