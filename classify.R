library(MASS)
set.seed(123)
library(ks)
#classify=function()
#{
dat <- read.table("/nethome/erichs/multivariate_analysis/T8_3_FOOTBALL.DAT")

gp=as.factor(dat[,1])
idx=sample(1:10,90,rep=T)

training=dat[idx!=1,-1]
test=dat[idx==1,-1]
resu=lda(training,gp[idx!=1])

#predict(resu,dat[idx==1,-1])$posterior
post.prob=matrix(rep(NA,3*90),ncol=3)

for(i in 1:10) post.prob[idx==i,]=predict(kda(dat[idx!=i,-1], eval.points=dat[idx!=i,-1], kde.flag=FALSE), dat[idx==i,-1])

pred.gp=apply(post.prob,1,which.max)
table(gp,pred.gp)

mis.rate=(90-sum(diag(table(gp,pred.gp))))/90

prob=post.prob[rbind(1,31,61),]
#prob=post.prob[c(1,31,61)] - changed to pull all three groups for each 1, 31, 61

#return(list(prob, mis.rate))
#}

#qda

## positive data example
#set.seed(8192)
#x <- 2^rnorm(100)

post.probq=matrix(rep(NA,3*90),ncol=3)

for(i in 1:10) post.probq[idx==i,]=predict(qda(dat[idx!=i,-1], gp[idx!=i]), dat[idx==i,-1])$posterior

predq.gp=apply(post.probq,1,which.max)
#table(gp,predq.gp)

mis.rateq=(90-sum(diag(table(gp,predq.gp))))/90

#probq=post.probq[c(1,31,61)]
probq=post.probq[rbind(1,31,61),]

#kde

post.probk=matrix(rep(NA,3*90),ncol=3)
require(mvtnorm) 
for(i in 1:10) 
  post.probk[idx==i,]=predict(kde(dat[idx!=i,-1], gp[idxk!=1], eval.points=dat[-1]), dat[idx==i,-1])

predk.gp=apply(post.probk,1,which.max)
#table(gp,predq.gp)

mis.ratek=(90-sum(diag(table(gp,predk.gp))))/90

#probk=post.probk[c(1,31,61)]
probk=post.probk[rbind(1,31,61),]

#--10fold CV with no groups.  Creates one column, 90 rows - estimate

dat <- read.table("http://www.ces.clemson.edu/~calvinw/MthSc807/MthSc807/data/T8_3_FOOTBALL.DAT")
set.seed(8193)
idx=sample(1:10,90,rep=T)
post.probkdenew=matrix(rep(NA,1*90),ncol=1)
gp=as.factor(dat[,1])

for (i in 1:10){
training=dat[idx!=i,-1]
test=dat[idx==i,-1]
gpstack <- matrix(dat[idx!=i, -c(2:7)])
gpstack6 <- matrix(c(gpstack, gpstack, gpstack, gpstack, gpstack, gpstack))
testx <- stack(test)
testxless <- testx[,1]
trainx <- stack(training)
trainxless <- trainx[,1]
#H.scv <- Hscv(trainxless)
kdah<- hkda(x=trainxless, x.group=gpstack6, bw="scv", binned=FALSE )
kda.gr <- kda(x=trainxless, kdah, gpstack6, eval.points=trainxless)
post.probkdenew[idx==i,] <- predict(kda.gr, x=testxless)
}

#---end


set.seed(8192)
library(ks)
post.probk1=matrix(rep(NA,1*90),ncol=1)
post.probk2=matrix(rep(NA,1*90),ncol=1)
post.probk3=matrix(rep(NA,1*90),ncol=1)
idxk=sample(1:10,30,rep=T)
dat <- read.table("/nethome/erichs/multivariate_analysis/T8_3_FOOTBALL.DAT")
dat1 <- dat[1:30,]
dat2 <- dat[31:60,]
dat3 <- dat[61:90,]
for(i in 1:10){
  H.scv <- Hscv(dat[idxk!=i, -1])
  fhat <- kde(x=dat1[idxk!=i,-1], H=H.scv, eval.points=dat1[idxk==i,-1])
  post.probk1[idxk==i,]=predict(fhat, x=dat1[idxk==i,-1])
}

for(i in 1:10){
  H.scv <- Hscv(dat[idxk!=i, -1])
  fhat <- kde(x=dat2[idxk!=i,-1], H=H.scv, eval.points=dat2[idxk==i,-1])
  post.probk2[idxk==i,]=predict(fhat, x=dat2[idxk==i,-1])
}

for(j in 1:10){
  H.scv <- Hscv(dat[idxk!=j, -1])
  fhat <- kde(x=dat3[idxk!=j,-1], H=H.scv, eval.points=dat3[idxk==j,-1])
  post.probk3[idxk==j,]=predict(fhat, x=dat3[idxk==j,-1])
}

post.proball <- cbind(post.probk1, post.probk2, post.probk3)

predk.gp=apply(post.proball,1,which.max)
#table(gp,predq.gp)

mis.ratek=(90-sum(diag(table(gp,predk.gp))))/90

#probk=post.probk[c(1,31,61)]
probk=post.probk[rbind(1,31,61),]

#--example of 4d iris kde

data(iris)
ir <- iris[,1:4][iris[,5]=="setosa",]
H.scv <- Hscv(ir)
fhat <- kde(ir, H.scv, eval.points=ir)