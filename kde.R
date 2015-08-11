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
  fhat1 <- kde(x=dat1[idxk!=i,-1], H=H.scv, eval.points=dat1[idxk==i,-1])
}
for(i in 1:10){
  datnew<- dat[,2:7][dat[,1]==1,]
  H.scv <- Hscv(datnew)
  fhat2 <- kde(x=datnew, H=H.scv, eval.points=datnew)
  post.probk2[idxk==i,]=predict(fhat2, x=dat2[idxk==i,-1])
}
#--iris 4d example emulation 
set.seed(2343) #-set the seed
dat <- read.table("/nethome/erichs/multivariate_analysis/T8_3_FOOTBALL.DAT")
post.probk1=matrix(rep(NA,1*90),ncol=1) #-create empty matrix for group 1
idxk=sample(1:10,90,rep=T) #--sample for 90 observations for 10 groups

for(i in 1:10){ #-loop for group 1
datnew1 <- dat[,2:7][dat[,1]=="1",] #-subset of data - group 1
H.scv <- Hscv(datnew1)
fhat1 <- kde(na.omit(datnew1[idxk!=i,]), H.scv, eval.points=na.omit(datnew1[idxk==i,])) #-kde - training as data, test as eval
post.probk1[idxk==i]=predict(fhat1, x=na.omit(datnew1[idxk==i,])) #-populating group 1 matrix/predict using test data
}

post.probk2=matrix(rep(NA,1*90),ncol=1) #-create empty matrix for group 2

for(i in 1:10){ #-loop for group 2
  datnew2 <- dat[,2:7][dat[,1]=="2",]
  H.scv <- Hscv(datnew2)
  fhat2 <- kde(datnew2[idxk!=i,], H.scv, eval.points=datnew[idxk==i,])
  post.probk2[idxk==i,]=predict(fhat2, x=datnew2[idxk==i,])
}

post.probk3=matrix(rep(NA,1*90),ncol=1) #-create empty matrix for group 2

for(i in 1:10){ #-loop for group 3
  datnew3 <- dat[,2:7][dat[,1]=="3",]
  H.scv <- Hscv(datnew3)
  fhat3 <- kde(datnew3[idxk!=i,], H.scv, eval.points=datnew[idxk==i,])
  post.probk3[idxk==i,]=predict(fhat3, x=datnew3[idxk==i,])
}

post.proball <- cbind(post.probk1, post.probk2, post.probk3) # bind the three groups

post.probfinal=matrix(rep(NA,3*90),ncol=3) #-create empty matrix for group 2

for (i in 1:90){
  post.probfinal[i,]=(post.proball[i,]*.33)/sum(post.proball[i,]/3)
}
                         
predk1.gp=apply(post.probfinal,1,which.max) 

mis.ratek2=(90-sum(diag(table(gp,predk1.gp))))/90 #-misclassifcation rate
#-----

for(j in 1:10){
  H.scv <- Hscv(dat[idxk!=j, -1])
  fhat3 <- kde(x=dat3[idxk!=j,-1], H=H.scv, eval.points=dat3[idxk==j,-1])
  post.probk3[idxk==j,]=predict(fhat3, x=dat3[idxk==j,-1])
}

post.proball <- cbind(post.probk1, post.probk2, post.probk3)

predk.gp=apply(post.proball,1,which.max)
table(gp,predk.gp)

mis.ratek=(90-sum(diag(table(gp,predk.gp))))/90

#probk=post.probk[c(1,31,61)]
probk=post.probk[rbind(1,31,61),]