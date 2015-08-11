dat <- read.table("/nethome/erichs/multivariate_analysis/T8_3_FOOTBALL.DAT")
#head(dat)
#dat <- dat[-1]
colnames(dat) <- c("Group", "WDIM", "CIRCUM", "FBEYE", "EYEHD", "EARHD", "JAW")
#pairs(dat, col=dat$Group+1, pch=dat$Group+1) # scatterplot


gp=as.factor(dat[,1])
idx=sample(1:10,90,rep=T)

training=dat[idx!=1,-1]
#resu=lda(training,gp[idx!=1])

#predict(resu,dat[idx==1,-1])$posterior
post.probtree=matrix(rep(NA,3*90),ncol=3)

for(i in 1:10) post.probtree[idx==i,]=predict(rpart(Group~., dat[idx!=i,], method="class"), dat[idx==i,-1])

predtree.gp=apply(post.probtree,1,which.max)
table(gp,predtree.gp)

mis.rate=(90-sum(diag(table(gp,predtree.gp))))/90

dat.cart<-rpart(Group~., data=dat, method="class")
#dat.cart #--cart results

attach(dat)
#table(EYEHD<10.35)
#table(Group[EYEHD<10.35])

plot(dat.cart,margin=.2)
text(dat.cart, use.n=FALSE, pretty=0)

dat.cart.pred <- predict(dat.cart, dat[,-1], type="prob")
table(Actual=dat$Group, Classified=dat.cart.pred)

