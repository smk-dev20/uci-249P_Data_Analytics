#Sherlin Mary Koshy(smkoshy) UCI MSWE-249P
#Reference : An Introduction to Statistical Learning by Gareth James,
#Daniela Witten, Trevor Hastie and Robert Tibshirani

#Conceptual Exercises

#2(a)
?plot
?symbols
?text
plot(NA,NA,type="n",xlim=c(-4,2),ylim=c(-1,5),asp=1,xlab ="X1", ylab ="X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)
#2(b)
text(c(-1), c(2), "< 4")
text(c(-4), c(2), "> 4")

#Applied Exercises
#4
set.seed(1)
x=matrix(rnorm(100*2),ncol=2)
x[1:50,]=x[1:50,]+2
x[51:75,]=x[51:75,]-2
y=c(rep(1,75),rep(2,25))
dat=data.frame(x=x,y=as.factor(y))
plot(x,col=y)
train=sample(100,75)

set.seed(1)
tune.out=tune(svm,y~.,data=dat[train,],kernel="linear",
              ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
bestlinear=tune.out$best.model
summary(bestlinear)
plot(bestlinear,dat[train,])
table(true=dat[train,"y"], pred=predict(bestlinear,newdata=dat[train,]))

set.seed(1)
tune.out=tune(svm,y~.,data=dat[train,],kernel="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),
                          gamma=c(0.5,1,2,3,4)))
bestkernel=tune.out$best.model
summary(bestkernel)
plot(bestkernel,dat[train,])
table(true=dat[train,"y"], pred=predict(bestkernel,newdata=dat[train,]))

table(true=dat[-train,"y"], pred=predict(bestlinear,
                                         newdata=dat[-train ,]))
table(true=dat[-train ,"y"], pred=predict(bestkernel,
                                          newdata =dat[-train ,]))

#5
x1=runif(500) -0.5
x2=runif(500) -0.5
y=1*( x1^2-x2^2 > 0)

plot(x1,x2,xlab ="X1",ylab ="X2",col=(4-y),pch=(3-y))
glm.fit=glm(y~x1+x2, family="binomial")
summary(glm.fit)
data=data.frame(x1=x1,x2=x2,y=y)
probs=predict(glm.fit, data, type ="response")
preds=rep(0,500)
preds[probs>0.47]= 1
plot(data[preds== 1,]$x1,data[preds==1,]$x2,col=(4 - 1),pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[preds == 0,]$x1,data[preds==0,]$x2, col = (4 - 0), pch = (3 - 0))

glmnl.fit=glm(y~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2), family = "binomial")
summary(glmnl.fit)

probs=predict(glmnl.fit, data, type ="response")
preds=rep(0, 500)
preds[probs > 0.47]=1
plot(data[preds==1,]$x1, data[preds==1,]$x2, col = (4 - 1), pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[preds == 0,]$x1,data[preds == 0,]$x2, col = (4 - 0), pch = (3 - 0))

data$y=as.factor(data$y)
svm.fit =svm(y ~ x1 + x2, data, kernel = "linear", cost = 0.01)
preds = predict(svm.fit, data)
plot(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0), xlab = "X1", ylab = "X2")
points(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1))

data$y= as.factor(data$y)
svmnl.fit = svm(y ~ x1 + x2, data, kernel = "radial", gamma = 1)
preds = predict(svmnl.fit, data)
plot(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0), xlab = "X1", ylab = "X2")
points(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1))

#7
library(ISLR)
mileage=ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
Auto$mpglevel = as.factor(mileage)

set.seed(1)
tune.out=tune(svm,mpglevel~.,data=Auto ,kernel ="linear",
              ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100)))
summary(tune.out)

set.seed (1)
tune.out=tune(svm,mpglevel~., data=Auto, kernel="radial",
              ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),
                           gamma=c(0.5,1,2,3,4) ))
summary(tune.out)

set.seed (1)
tune.out=tune(svm,mpglevel~., data=Auto, kernel="polynomial",
              ranges =list(cost=c(0.1 ,1 ,10 ,100 ,1000),
                           degree=c(1,2,3,4) ))
summary(tune.out)

?plot.svm

svm.linear=svm(mpglevel~., data = Auto, kernel = "linear", cost = 1)
svm.poly = svm(mpglevel~., data = Auto, kernel = "polynomial", cost = 1000, degree = 2)
svm.radial = svm(mpglevel~., data = Auto, kernel = "radial", cost = 10, gamma = 0.5)
plot(svm.linear,Auto, mpg~cylinders)
plot(svm.linear,Auto, mpg~horsepower)
plot(svm.linear,Auto, mpg~weight)