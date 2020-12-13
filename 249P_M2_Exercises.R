#Sherlin Mary Koshy(smkoshy) UCI MSWE-249P
#Reference : An Introduction to Statistical Learning by Gareth James, Daniela Witten, Trevor Hastie and Robert Tibshirani

#Applied Exercise 8
Auto=read.csv("Auto.csv",header=T,na.strings="?")
fix(Auto)
dim(Auto)
Auto=na.omit(Auto)
dim(Auto)
names(Auto)
lm.fit=lm(mpg~horsepower, data=Auto)
summary(lm.fit)
coef(lm.fit)
predict(lm.fit, data.frame(horsepower=98),interval = "confidence")
predict(lm.fit, data.frame(horsepower=98),interval = "prediction")
attach(Auto)
lm.fit=lm(mpg~horsepower, data=Auto)
plot(horsepower,mpg)
abline(lm.fit ,lwd =3, col ="red")
par(mfrow=c(2,2))
plot(lm.fit)


#Applied Exercise 9
pairs(Auto[,1:8])
cor(Auto[,1:8])
lm.fit=lm(mpg~.-name,data=Auto)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)
summary(lm(mpg~weight*displacement))
summary(lm(mpg~horsepower+acceleration+horsepower:acceleration))
summary(lm(mpg~horsepower*weight))

summary(lm(formula = mpg ~ . - name + I(horsepower^2), data = Auto))
summary(lm(formula = mpg ~ . - name + log(horsepower), data = Auto))
summary(lm(formula = mpg ~ . - name + sqrt(horsepower), data = Auto))

#Applied Exercise 10
fix(Carseats)
names(Carseats)
?Carseats
lm.fit = lm(Sales~Price+Urban+US, data=Carseats)
summary(lm.fit)
lm.fit1 = lm(Sales~Price+US)
summary(lm.fit1)
confint(lm.fit1)
par(mfrow=c(2,2))
plot(lm.fit1)

#Applied Exercise 15
fix(Boston)
?Boston
names(Boston)
dim(Boston)

lm.fitz = lm(crim~zn, data=Boston)
lm.fiti = lm(crim~indus, data=Boston)
lm.fitc = lm(crim~chas, data=Boston)
lm.fitn = lm(crim~nox, data=Boston)
lm.fitr = lm(crim~rm, data=Boston)
lm.fita = lm(crim~age, data=Boston)
lm.fitd = lm(crim~dis, data=Boston)
lm.fitrd = lm(crim~rad, data=Boston)
lm.fitt = lm(crim~tax, data=Boston)
lm.fitptr = lm(crim~ptratio, data=Boston)
lm.fitb = lm(crim~black, data=Boston)
lm.fitl = lm(crim~lstat, data=Boston)
lm.fitm = lm(crim~medv, data=Boston)

summary(lm.fitz)
summary(lm.fiti)
summary(lm.fitc)
summary(lm.fitn)
summary(lm.fitr)
summary(lm.fita)
summary(lm.fitd)
summary(lm.fitrd)
summary(lm.fitt)
summary(lm.fitptr)
summary(lm.fitb)
summary(lm.fitl)
summary(lm.fitm)

X = c(coef(lm.fitz)[2],coef(lm.fiti)[2],coef(lm.fitc)[2],coef(lm.fitn)[2],
      coef(lm.fitr)[2],coef(lm.fita)[2],coef(lm.fitd)[2],coef(lm.fitrd)[2],
      coef(lm.fitt)[2],coef(lm.fitptr)[2],coef(lm.fitb)[2],coef(lm.fitl)[2],coef(lm.fitm)[2])

names(Boston)

plot(medv,crim)
abline(lm.fitm,lwd =3, col ="red")
plot(lstat,crim)
abline(lm.fitl,lwd =3, col ="red")
plot(age,crim)
abline(lm.fita,lwd =3, col ="red")

lm.fit = lm(crim~.,data=Boston)
summary(lm.fit)
Y = coef(lm.fit)[2:14]
X
Y
plot(X,Y)

lm.nfitz = lm(crim~poly(zn,3), data=Boston)
lm.nfiti = lm(crim~poly(indus,3), data=Boston)
lm.nfitn = lm(crim~poly(nox,3), data=Boston)
lm.nfitr = lm(crim~poly(rm,3), data=Boston)
lm.nfita = lm(crim~poly(age,3), data=Boston)
lm.nfitd = lm(crim~poly(dis,3), data=Boston)
lm.nfitrd = lm(crim~poly(rad,3), data=Boston)
lm.nfitt = lm(crim~poly(tax,3), data=Boston)
lm.nfitptr = lm(crim~poly(ptratio,3), data=Boston)
lm.nfitb = lm(crim~poly(black,3), data=Boston)
lm.nfitl = lm(crim~poly(lstat,3), data=Boston)
lm.nfitm = lm(crim~poly(medv,3), data=Boston)

summary(lm.nfitz)
summary(lm.nfiti)
summary(lm.nfitn)
summary(lm.nfitr)
summary(lm.nfita)
summary(lm.nfitd)
summary(lm.nfitrd)
summary(lm.nfitt)
summary(lm.nfitptr)
summary(lm.nfitb)
summary(lm.nfitl)
summary(lm.nfitm)