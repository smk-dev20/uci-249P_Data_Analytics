#Sherlin Mary Koshy(smkoshy) UCI MSWE-249P
#Reference : An Introduction to Statistical Learning by Gareth James,
#Daniela Witten, Trevor Hastie and Robert Tibshirani

#Applied Exercise 7
library(ISLR)
set.seed(1)
scaled.USA = scale(USArrests)
x.dist = c(dist(t(scaled.USA)))
x.dist
cors=cor(scaled.USA)[lower.tri(diag(4))]
cors
x.dist^2/(1-cors)

#Applied Exercise 9
USArrests[1,]
hc.complete =hclust(dist(USArrests),method ="complete")
plot(hc.complete,main ="Complete Linkage", xlab="", sub ="",
     cex =.9)
hc.clusters=cutree(hc.complete,3)
hc.clusters

hc.complete =hclust(dist(scaled.USA),method ="complete")
plot(hc.complete,main ="Complete Linkage", xlab="", sub ="",
     cex =.9)
hc.clusters=cutree(hc.complete,3)
hc.clusters

#Applied Exercise 10
set.seed(2)
x=matrix(rnorm(20*3*50,mean=0,sd=0.001), ncol =50)
x[1:20, 1]=x[1:20, 1]+2
x[21:40, 1]=x[21:40, 1] -10
x[41:60, 2] = 1

pr.out=prcomp(x)

plot(pr.out$x[,1:2], col = 1:3)
labels=c(rep(1,20),rep(2,20),rep(3,20))

set.seed(2)
km.out =kmeans(x,3, nstart =20)
km.out$cluster
table(km.out$cluster,labels)

set.seed(2)
km.out =kmeans(x,2, nstart =20)
km.out$cluster
table(km.out$cluster,labels)

set.seed(2)
km.out =kmeans(x,4, nstart =20)
km.out$cluster
table(km.out$cluster,labels)

data = data.frame(pr.out$x[,1],pr.out$x[,2])
dim(data)
set.seed(2)
km.out =kmeans(data,3, nstart =20)
km.out$cluster

set.seed(2)
km.out =kmeans(scale(x),3, nstart =20)
km.out$cluster

