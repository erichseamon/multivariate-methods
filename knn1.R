library(MASS)
set.seed(123)
library(ks)
library(class)
#classify=function()
#{
dat <- read.table("/nethome/erichs/multivariate_analysis/T8_3_FOOTBALL.DAT")

dat1 <- dat[1:30, -1]
dat2 <- dat[31:60, -1]
dat3 <- dat[61:90, -1]


dat1mean1 <- mean(dat1[,1])
dat1mean2 <- mean(dat1[,2])
dat1mean3 <- mean(dat1[,3])
dat1mean4 <- mean(dat1[,4])
dat1mean5 <- mean(dat1[,5])
dat1mean6 <- mean(dat1[,6])


dat2mean1 <- mean(dat2[,1])
dat2mean2 <- mean(dat2[,2])
dat2mean3 <- mean(dat2[,3])
dat2mean4 <- mean(dat2[,4])
dat2mean5 <- mean(dat2[,5])
dat2mean6 <- mean(dat2[,6])

dat3mean1 <- mean(dat3[,1])
dat3mean2 <- mean(dat3[,2])
dat3mean3 <- mean(dat3[,3])
dat3mean4 <- mean(dat3[,4])
dat3mean5 <- mean(dat3[,5])
dat3mean6 <- mean(dat3[,6])

dat3mean1 <- mean(beetle2[,1])
dat3mean2 <- mean(beetle2[,2])
dat3mean3 <- mean(beetle2[,3])
dat3mean4 <- mean(beetle2[,4])

dat1mean <- matrix(c(dat1mean1, dat1mean2, dat1mean3, dat1mean4))
dat2mean <- matrix(c(dat2mean1, dat2mean2, dat2mean3, dat2mean4))
dat3mean <- matrix(c(dat2mean1, dat2mean2, dat2mean3, dat2mean4))




gp=as.factor(dat[,1])
idx=sample(1:10,90,rep=T)

training=dat[idx!=1,-1]
resu=lda(training,gp[idx!=1])

#predict(resu,dat[idx==1,-1])$posterior
post.prob=matrix(rep(NA,3*90),ncol=3)

#--S-pooled for knn

S1 <- cov(dat1)
S2 <- cov(dat2)
S3 <- cov(dat3)
Spooled <- 1/(30+30+30-3)
Spooled2 <- 29 * S1 + 29 * S2 + 29 * S3
Spooled <- Spooled * Spooled2
InverseSpooled <- solve(Spooled)

T21 <- (30 * 30 * 30/(30 + 30 + 30)) 
T22 <- (t(beetle1mean - beetle2mean))
T23 <- (beetle1mean - beetle2mean)
T2 <- T21 * T22 %*% InverseSpooled %*% T23

for(i in 1:10){ 
  m <- t(dat[idx!=i,-1] - dat[idx==i,-1]) %*% InverseSpooled %*%(dat[idx!=i,-1] - dat[idx==i,-1])
  #knnoutput <- knnMCN(dat[idx!=i,-1], gp[idx!=i], dat[idx==i,-1],  K = 9, ShowObs = TRUE)
  #post.prob[idx==i,]=predict(knnoutput["TstXIBelong"], dat[idx==i,-1])
}
pred.gp=apply(post.prob,1,which.max)
table(gp,pred.gp)

mis.rate=(90-sum(diag(table(gp,pred.gp))))/90

prob=post.prob[rbind(1,31,61),]