library(MASS)
set.seed(123)
library(ks)
#classify=function()
#{
dat <- read.table("/nethome/erichs/multivariate_analysis/T8_3_FOOTBALL.DAT")

gp=as.factor(dat[,1])
idx=sample(1:10,90,rep=T)

training=dat[idx!=1,-1]
resu=lda(training,gp[idx!=1])
evpts <- do.call(expand.grid,  lapply(dat[-1], quantile, prob=c(.15, .30, .45, .60, .75, .90)) )

#predict(resu,dat[idx==1,-1])$posterior
post.prob=matrix(rep(NA,3*90),ncol=3)

for(i in 1:10) post.prob[idx==i,]=predict(kde(dat[idx!=i,-1], eval.points=evpts), dat[idx==i,-1])$estimate

pred.gp=apply(post.prob,1,which.max)
table(gp,pred.gp)

mis.rate=(90-sum(diag(table(gp,pred.gp))))/90

probk=post.prob[rbind(1,31,61),]