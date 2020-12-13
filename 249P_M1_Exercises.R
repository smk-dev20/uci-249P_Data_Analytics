#Sherlin Mary Koshy (smkoshy)
#University of California, Irvine
#Reference : An Introduction to Statistical Learning by Gareth James, Daniela Witten,
#Trevor Hastie and Robert Tibshirani

college = read.csv("College.csv")
fix(college)
rownames(college) = college[,1]
fix(college)
college = college[,-1]
fix(college)
summary(college)
?pairs
pairs(college[,2:11])
names(college)

pr = as.factor(Private)
attach(college)
plot(pr,Outstate, xlab="Private",ylab="Outstate Tuition")

Elite =rep ("No",nrow(college ))
Elite [college$Top10perc >50]=" Yes"
Elite =as.factor(Elite)
college =data.frame(college ,Elite)
summary(Elite)
plot(Elite,Outstate, xlab="Elite",ylab="Outstate Tuition")
par(mfrow=c(2,2)) 
hist(college$S.F.Ratio,breaks = 10)
hist(college$PhD,breaks = 5)
hist(college$Accept,breaks = 15)
hist(college$Grad.Rate,breaks = 25)

Auto = read.csv("Auto.csv", header = T, na.strings = "?")
Auto = na.omit(Auto)
dim(Auto)
fix(Auto)
names(Auto)
range(Auto$mpg)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)

mean(Auto$mpg)
sd(Auto$mpg)

mean(Auto$displacement)
sd(Auto$displacement)

mean(Auto$horsepower)
sd(Auto$horsepower)

mean(Auto$weight)
sd(Auto$weight)

mean(Auto$acceleration)
sd(Auto$acceleration)


dim(Auto)
Auto1 = Auto
dim(Auto1)


res <- Auto1[-c(seq(10,85)),]
dim(res)

range(res$mpg)
mean(res$mpg)
sd(res$mpg)

range(res$displacement)
mean(res$displacement)
sd(res$displacement)

range(res$horsepower)
mean(res$horsepower)
sd(res$horsepower)

range(res$weight)
mean(res$weight)
sd(res$weight)

range(res$acceleration)
mean(res$acceleration)
sd(res$acceleration)

cylinders = as.factor(Auto$cylinders)
plot(cylinders,Auto$horsepower,xlab="Cylinders",ylab="Horsepower",main="Cylinders vs Horsepower")
plot(Auto$horsepower,Auto$mpg,xlab="Horsepower",ylab="mpg",main="Horsepower vs mpg")
hist(Auto$year)
plot(Auto$horsepower,Auto$acceleration,xlab="Horsepower", ylab="Acceleration", main="Horsepower vs Acceleration")
plot(Auto$weight,Auto$acceleration,xlab="Weight", ylab="Acceleration", main="Weight vs Acceleration")
plot(cylinders,Auto$mpg,xlab="Cylinders", ylab="mpg", main="Cylinders v mpg")

library (MASS)
Boston
?Boston


plot(Boston$ptratio, Boston$medv, xlab = "Pupil-Teacher ratio", ylab="Median home value", main="Pupil-Teacher ratio vs Home Price")
plot(Boston$rm, Boston$medv, xlab = "Rooms per house", ylab="Median home value", main="Rooms per house vs Home Price")
plot(Boston$lstat, Boston$crim, xlab = "Lower Status of population", ylab="Crime Rate", main="Proportion of lower status in population vs Crime Rate")
plot(Boston$rad, Boston$tax, xlab = "Accessibility to highway", ylab="Tax Rate", main="Access to highway vs Property Tax")
plot(Boston$dis, Boston$medv, xlab = "Dist to employment centers", ylab="Median home price", main="Distance to employment centers vs Home Prices")
plot(Boston$crim, Boston$medv, xlab = "Crime Rate", ylab="Home Prices", main="Crime Rate vs Home Prices")

range(Boston$crim)
range(Boston$tax)
range(Boston$ptratio)

fix(Boston)
range(Boston$chas)
median(Boston$ptratio)

range(Boston$medv)
Boston[Boston$medv==5,]
range(Boston$age)
range(Boston$lstat)
range(Boston$nox)
range(Boston$dis)
range(Boston$black)

dim(Boston[Boston$rm>7,])
dim(Boston[Boston$rm>8,])
Boston[Boston$rm>8,]