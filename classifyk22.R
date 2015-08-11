library(MASS)
set.seed(123)
library(ks)
library(hydroTSM)
#classify=function()
#{
dat <- read.table("/nethome/erichs/multivariate_analysis/T8_3_FOOTBALL.DAT")

gp=as.factor(dat[,1])
idx=sample(1:10,90,rep=T)

training=dat[idx!=1,-1]
resu=lda(training,gp[idx!=1])

#predict(resu,dat[idx==1,-1])$posterior
post.prob=matrix(rep(NA,3*90),ncol=3)

for (i in 1:10){
  test <- dat[idx==i,-1]
  training <- dat[idx!=i,-1]
  S <- cov(dat[idx!=i,-1])
  jto <- 90-length(test[,1])
  jto2 <- length(test[,1])
  resu=matrix(NA,ncol=jto2, nrow=jto)
  resugroup=matrix(NA,ncol=1, nrow=jto)
  assign(paste("resu", i, sep = ""), resu)
   for (j in 1:jto){   
  resucolumns <- mahalanobis(dat[idx==i,-1], as.numeric(dat[idx!=i,-1][j,]), S)
  colnames(resu) <- names(resucolumns)
  resu[j,]=mahalanobis(dat[idx==i,-1], as.numeric(dat[idx!=i,-1][j,]), S)
  resugroup= as.matrix(dat[idx!=i,1])
   }
  resu <- cbind(resu, resugroup)
  assign(paste("resu", i, sep=""), resu)
}

knnprob=t(matrix(NA,ncol=90, nrow=3))
for (i in 1:10) {
  jto3 <- nrow(dat[idx==i,-1])
    for (j in 1:jto3) {
      resuax <- data.frame(get(paste("resu", i, sep = "")))
      resua <- resuax[order(resuax[,j]) , ]
      vectornames <- as.matrix(names(resua))
      vectornames2 <- rm1stchar(vectornames)
      vectornames3 <- as.matrix(rm1stchar(vectornames[1,1]))
      vectornames4 <- as.numeric(c(vectornames3[1,1], vectornames2[-1]))
      vectornames5 <- as.matrix(vectornames4[1:jto3])
      resua1 <- subset(resua, select = j)
      resua2 <- subset(resua, select = jto3+1)
      resua <- cbind(resua1, resua2)
      #resua <- resua[,c(j,jto3+1)]
      #resua <- as.matrix(sort(resuax[,j], decreasing = FALSE))
      resua <- as.matrix(resua[1:9,])
        for (k in 1:3){
          xx <- vectornames5[j,]
          knnprob[xx,k] <- (length(which(resua[,2] == k)))/9
        }
      assign(paste("resua", j, sep=""), resua)
    }
}


pred.gp=apply(knnprob,1,which.max)
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


set.seed(8192)
x <- training
fhat <- kde(x=x, positive=TRUE, eval.points=x$eval.points)
plot(fhat)
points(c(0.5, 1), predict(fhat, x=c(0.5, 1)))

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

predk.gp=apply(post.probk,1,which.max)
#table(gp,predq.gp)

mis.ratek=(90-sum(diag(table(gp,predk.gp))))/90

#probk=post.probk[c(1,31,61)]
probk=post.probk[rbind(1,31,61),]

#--example of 4d iris kde

data(iris)
ir <- iris[,1:4][iris[,5]=="setosa",]
H.scv <- Hscv(ir)
fhat <- kde(ir, H.scv, eval.points=ir)