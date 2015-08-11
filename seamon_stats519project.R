#------------------------------------------------------------------------#
# TITLE:        stat519project_seamon.R
#
# AUTHOR:       Erich Seamon
#
# INSTITUITON:  College of Natural Resources
#               University of Idaho
#
# DATE:         May 2015
#
# 
#
# COMMENTS:     This script generates statistical outputs for a  
#               dataset of climate variables -for a portion of  
#               Westrern WA and Eastern ID.  The script generates
#               a series of plots.
#               
#                
#               More on the agmesh design: reacchapp.nkn.uidaho.edu 
#
#--Setting the working directory an d clearing the workspace-----------#

library(gridExtra)
#install.packages('dendextend')
library(dendextend)
library(mvnormtest)
library(ks)

#--subset data for data reduction purposes

#colnames(fclimatematrix1) <- c("TMMX", "TMMN", "RMIN", "RMAX", "SRAD", "VS", "ET", "YEAR", "MLRA", "LAT", "LON", "DAY")
scen <- "scenario_72698"
setwd(paste("/agmesh-scenarios/", scen, sep="")) 
#write.matrix(fclimatematrix1, file="")


#------start with reduced file read from server

fclimatematrix1 <- read.csv("http://reacchapp.nkn.uidaho.edu/agmesh-scenarios/scenario_72698/analysis/datatable.csv")

nrowz <- nrow(fclimatematrix1)
idx=sample(1:nrowz,nrowz,rep=T)

#-For multvariate normality test
Ca <- t(fclimatematrix1[1:nrowz,1:6])
mshapiro.test(Ca)


gp=as.factor(fclimatematrix1[,9])

fclimatemlrafactor <- factor(fclimatematrix1[,9])
fclimateyearfactor <- factor(fclimatematrix1[,8])
fclimatematrix2 <- cbind(fclimatematrix1[,1:6])
fclimatematrix2 <- as.matrix(fclimatematrix2)
#method <- factor(gl(3,18,36,labels=c("M1","M2","M3")))
#--Run manovas
manova_fclimatematrix2 <- summary(manova(fclimatematrix2 ~ fclimatemlrafactor))
manova_fclimatematrix3 <- summary(manova(fclimatematrix2 ~ fclimateyearfactor))
manova_fclimatematrix4 <- summary(manova(fclimatematrix2 ~ fclimatematrix1[,7]))

xx <- round(fclimatematrix1, digits=2)

#--KDE 10 fold CV

misratekde10_20 <- matrix(NA, nrow=20, ncol=1)

for (m in 1:20) {

dat <- fclimatematrix1
nrowz <- nrow(dat)
#set.seed(8193)
idx=sample(1:10,nrowz,rep=T)
#gp=as.factor(dat[,1])
post.probkdenew=matrix(rep(NA,1*nrowz),ncol=1)

post.probkdenew1=matrix(rep(NA,1*nrowz),ncol=1) #empty matrix for loop population
post.probkdenew2=matrix(rep(NA,1*nrowz),ncol=1) #empty matrix for loop population
post.probkdenew3=matrix(rep(NA,1*nrowz),ncol=1) #empty matrix for loop population

for (i in 1:10){ #CV loop for kde.  Requires three kde for training gp1, 2, and 3
  training=dat[idx!=i,]
  test=dat[idx==i,] 
  #H.scv <- Hscv(training)
  #--pull out 1 2 and 3 from training
  training1 <- subset(training, training[,9]=="11")
  training1 <- training1[,1:6]
  #H.scv <- Hscv(training1)
  training2 <- subset(training, training[,9]=="59")
  training2 <- training2[,1:6]
  #H.scv <- Hscv(training2)
  training3 <- subset(training, training[,9]=="10")
  training3 <- training3[,1:6]
  #H.scv <- Hscv(training3)
  #run kde for each of the three grouped training datasets
  post.probkdenew1[idx==i,] <- kde(x=training1, eval.points=test[,1:6])$estimate
  post.probkdenew2[idx==i,] <- kde(x=training2, eval.points=test[,1:6])$estimate
  post.probkdenew3[idx==i,] <- kde(x=training3, eval.points=test[,1:6])$estimate
  #post.probkdenew[idx==i,] <- predict(fhat, x=test)
}
#combine the estimates for each of the groups
post.probktotal <- cbind(post.probkdenew1, post.probkdenew2, post.probkdenew3)

post.probfinal1=matrix(rep(NA,1*nrowz),ncol=1) #-create empty matrix for group 1
post.probfinal2=matrix(rep(NA,1*nrowz),ncol=1) #-create empty matrix for group 2
post.probfinal3=matrix(rep(NA,1*nrowz),ncol=1) #-create empty matrix for group 3


for (i in 1:nrowz){ #-generatetes posterior probabilities from estimates - gp1
  post.probfinal1[i,]=round((post.probkdenew1[i,]*.33)/sum(post.probktotal[i,]/3), 
                            digits=5)
}

for (i in 1:nrowz){#-generatetes posterior probabilities from estimates - gp2
  post.probfinal2[i,]=round((post.probkdenew2[i,]*.33)/sum(post.probktotal[i,]/3), 
                            digits=5)
}

for (i in 1:nrowz){#-generatetes posterior probabilities from estimates - gp3
  post.probfinal3[i,]=round((post.probkdenew3[i,]*.33)/sum(post.probktotal[i,]/3), 
                            digits=5)
}
#combined posterior probabilities
post.probfinalzkde10 <- cbind(post.probfinal1, post.probfinal2, post.probfinal3)
gp=as.factor(fclimatematrix1[,9])

predkde1.gp=apply(post.probfinalzkde10,1,which.max)  #predicted groups
predkde1.gp[predkde1.gp == 1] <- 10
predkde1.gp[predkde1.gp == 2] <- 11
predkde1.gp[predkde1.gp == 3] <- 59

predkde1.gp <- unlist(predkde1.gp, recursive=TRUE)
#predkde1.gp <- rbind (predkde1.gp, predkde1.gp[2304])
predkde1.gp <- c(predkde1.gp, 11)

xx <- table(gp,predkde1.gp)
xx_sorted <- apply(xx,1,which.max)
xx <- xx[,xx_sorted]

#table(gp,predkde1.gp)#--original vs predicted groups

misratekde10_20[m] = (nrowz-sum(diag(xx)))/nrowz #-misclassifcation rate

}

grid.table(round(misratekde10_20, digits=3))

misratekde10_20avg <- mean(misratekde10_20)

library(ggplot2)

#kde plots

plot(post.probktotal[,3])
plot(post.probktotal[,1])

abline(lm(post.probktotal[,1]~post.probktotal[,1]), col="red") # regression line (y~x) 
lines(lowess(post.probktotal[,3]~post.probktotal[,3]), col="blue") # lowess line (x,y)


#setting up KDE density plots

DP <- data.frame(post.probktotal)

require(ggplot2)

p.dp.1 <- ggplot(data = DP,
                 aes(x = c(1:nrowz),
                     y = post.probktotal[,1])) + 
  ylab("Kernel Density") + 
  xlab("Observations") +
  geom_point() + 
  theme_bw(base_size = 16) +
  geom_smooth(method = "gam", formula = y ~ s(x), size = 1,
              aes(colour = "Linear")) +
  scale_color_manual(name = "Fits",
                     breaks = c("Linear"),
                     values = c("blue"))

p.dp.2 <- ggplot(data = DP,
                 aes(x = c(1:nrowz),
                     y = post.probktotal[,2])) + 
  ylab("Kernel Density") + 
  xlab("Observations") +
  geom_point() + 
  theme_bw(base_size = 16) +
  geom_smooth(method = "gam", formula = y ~ s(x), size = 1,
              aes(colour = "Linear")) +
  scale_color_manual(name = "Fits",
                     breaks = c("Linear"),
                     values = c("blue"))


p.dp.3 <- ggplot(data = DP,
                 aes(x = c(1:nrowz),
                     y = post.probktotal[,3])) + 
  ylab("Kernel Density") + 
  xlab("Observations") +
  geom_point() + 
  theme_bw(base_size = 16) +
  geom_smooth(method = "gam", formula = y ~ s(x), size = 1,
              aes(colour = "Linear")) +
  scale_color_manual(name = "Fits",
                     breaks = c("Linear"),
                     values = c("blue"))


p.dp.4 <- ggplot(data = DP,
                 aes(x = c(1:nrowz),
                     y = post.probktotal[,1])) + 
  ylab("Kernel Density") + 
  xlab("Observations") +
  geom_point(data = DP,
              aes(x = c(1:nrowz),
                  y = post.probktotal[,3])) +
  theme_bw(base_size = 16) +
  geom_smooth(method = "gam", formula = y ~ s(x), size = 1,
              aes(colour = "Linear")) +
  scale_color_manual(name = "Fits",
                     breaks = c("Linear"),
                     values = c("blue"))

#---Nearest Neighbor 10 fold CV
misrateknn10_20 <- matrix(NA, nrow=20, ncol=1)

for (m in 1:20) {
  
gp=as.factor(dat[,1])

post.probknn1=matrix(rep(NA,3*nrowz),ncol=3) #-empty matrix for loop population

for (i in 1:10){#-loop for 10 fold CV for knn
  training=dat[idx!=1,1:6]#-training
  test <- dat[idx==i,1:6]#-test
  S <- cov(dat[idx!=i,1:6]) 
  jto <- nrowz-length(test[,1])
  jto2 <- length(test[,1])
  resu=matrix(NA,ncol=jto2, nrow=jto)
  resugroup=matrix(NA,ncol=1, nrow=jto)
  assign(paste("resu", i, sep = ""), resu)
  for (j in 1:jto){   
    resucolumns <- mahalanobis(dat[idx==i,1:6], as.numeric(dat[idx!=i,1:6][j,]), S)
    colnames(resu) <- names(resucolumns)
    resu[j,]=mahalanobis(dat[idx==i,1:6], as.numeric(dat[idx!=i,1:6][j,]), S)
    resugroup= as.matrix(dat[idx!=i,9])
  }
  resu <- cbind(resu, resugroup)
  assign(paste("resu", i, sep=""), resu)
}
#loop to restructure distances and calculate probabilities
knnprob10=t(matrix(NA,ncol=nrowz, nrow=3))
for (i in 1:10) {
  jto3 <- nrow(dat[idx==i,1:6])
  for (j in 1:jto3) {
    resuax <- data.frame(get(paste("resu", i, sep = "")))
    resua <- cbind(resuax[j], resuax[jto3+1])
    resua$rows <- rownames(resua)
    resua <- resua[order(resua[,1], decreasing=F),]

    resua[, 2][resua[, 2] == 10] <- 1
    resua[, 2][resua[, 2] == 11] <- 2
    resua[, 2][resua[, 2] == 59] <- 3
    resuaall <- resua
    resua <- resua[1:9,]
    
    for (k in 1:3){ #sub-loop that takes lowest 9 distances, and creates probabilities
      xx <- as.numeric(resuaall[j,3])
      knnprob10[xx,k] <- (length(which(resua[,2] == k)))/9
    }
    assign(paste("resuaa", j, sep=""), resua)
  }
}

knnprob10[is.na(knnprob10)] <- 0

predknn10.gp=apply(knnprob10,1,which.max) #-predicted groups
predknn10.gp[predknn10.gp == 1] <- 10
predknn10.gp[predknn10.gp == 2] <- 11
predknn10.gp[predknn10.gp == 3] <- 59

predknn10.gp <- unlist(predknn10.gp, recursive=TRUE)
#predkde1.gp <- rbind (predkde1.gp, predkde1.gp[2304])
#predknn10.gp <- c(predknn.gp, 11)

gpknn=as.factor(dat[,9])
xx <- table(gpknn,predknn10.gp)
xx_sorted <- apply(xx,1,which.max)
xx <- xx[,xx_sorted]

gpknn=as.factor(dat[idx==i,9])
table(gpknn,predknn10.gp) #-original vs predicted groups

misrateknn10_20[m] = (nrowz-sum(diag(table(gpknn,predknn10.gp))))/nrowz #-misclassification rate
}

#--Tree classifcation

for (m in 1:20) {
  
  nrowz <- nrow(fclimatematrix2)
  gplevels <- nlevels(gpx3)
    
post.probtree10=matrix(rep(NA,gplevels*nrowz),ncol=gplevels)

df.fc <- data.frame(fclimatematrix1)

fc.1 <- cbind(df.fc[,1:6], df.fc[9])
fc.2 <- cbind(df.fc[,1:6])

attach(fc.1)


library(ggdendro)
#for(i in 1:10) {

fc.cart <- rpart(MLRA~., fc.2, 
      method="class")

plot(fc.cart, margin=.2)
text(fc.cart, use.n=FALSE, pretty=0)
  
  treetrain <- data.frame(fclimatematrix1[idx!=i,])
  treetest <- data.frame(fclimatematrix1[idx==i,])
  post.probtree10[idx==i,]=predict(rpart(MLRA~., treetrain, 
                                                      method="class"), treetest, type="prob")
}

predtree10.gp=apply(post.probtree10,1,which.max)
table(gpx3,predtree10.gp)

misratetree10_20[m] =(nrowz-sum(diag(table(gpx3,predtree10.gp))))/nrowz

#-HClustering

misratehc10_20=c()
m=10

for(j in 1:20){
  post.prob.hc=matrix(rep(NA,3*nrowz),ncol=3)
  idx=sample(1:m,nrowz,rep=TRUE)
  #dat.new=cbind(fc.1,idx)
  for(i in 1:m){
    training=fc.1[idx!=i,-7]
    test=fc.1[idx==i,-7]
    dat_hc_w=cutree(hclust(dist(scale(training)),method="ward.D"),3)
    for(g in 1:3){
      centroid=colMeans(fc.1[dat_hc_w==g,-7])
      post.prob.hc[idx==i,g]=apply(sweep(fc.1[idx==i,-7],2,centroid)^2,1,sum)
    }}
  
  pred_gp_hc=apply(post.prob.hc,1,which.min)
  misratehc10_20[j]=round(((nrowz-(sum(apply(table(gp,pred_gp_hc),2,max))))/nrowz),2)
}

dat_hc_w=hclust(dist(scale(fc.1)),method="ward.D")


#-dendrogram creation for Hclustering

dend <- as.dendrogram(dat_hc_w)

#---PCA

fclim.pc <- prcomp(fclimatematrix1[,1:6], scale=T)  
U <- fclim.pc$rotation #---U
X <- fclimatematrix1[,1:6]
X2 <- fclimatematrix1[,9]
X3<- cbind(X,X2)
Z <- fclimatematrix1[,1:6] %*% fclim.pc$rotation
#Xsample <- X3[sample(nrow(X3),size=1000,replace=TRUE),]
#Zsample <- Z[sample(nrow(Z),size=1000,replace=TRUE),]

#-removing outliers - es050315
fclim.pc2 <- fclim.pc
fclim.pc2$x[337,] <-  0
fclim.pc2$x[154,] <-  0

#--remove any previously created plots in this scenario
#--before creating new versions below

system(paste("rm -r ", "/agmesh-scenarios/", scen, "/analysis/", "*.jpg", sep=""))


#--Plotting of all results

#-EDA plots

grid.table(xx[1:10,1:11])

boxplot(fclimatematrix1[,1:7], main="Boxplot of all variables - Day 1 2000")

#--Dendrogram plotting

labels_colors(dend) <- fc.1[,7] 
labels_colors(dend)

plot(dend)

plot(cut(dend, h = 70)$lower[[2]], cex=10)


#-pairwise plot of initial data

gpx3=as.factor(X3[,7])
pairs(X3,pch=20,col=unclass(gpx3))

#-correlation
grid.table(round(cor(fclimatematrix1[,1:6]), digits=3))
(cor(fclimatematrix1[,1:6]))

#-box plot of Z
boxplot(Z)

#-pairwise plot of Z
pairs(Z)

#--correlation of each variable to PC1
cor(as.matrix(scale(fclimatematrix1[,1:6])), fclim.pc2$x[,1])

#-loadings
fclim.pc.loadings <- cor(as.matrix((fclimatematrix1[,1:6])), fclim.pc2$x)

#-sum of loadings
fclim.pc.loadings.sum <- apply(fclim.pc.loadings[,1:3]^2,2,sum)

#-variances of PC
fclim.pc.variances <- (fclim.pc2$sdev^2)

#pairwise plot of first 6 PCs
pairs(fclim.pc2$x[,1:6], pch=20,col=unclass(gpx3))

#-biplot of PC results with 337 removed
biplot(fclim.pc2, xlabs=fclimatematrix1[,9], cex=c(.7,1))

#-zoomed in biplot of PC results with 337 removed
biplot(fclim.pc2, xlabs=fclimatematrix1[,9], cex=c(.7,1), expand=20, xlim=c(-0.05, 0.05), ylim=c(-0.05, 0.05))

#plotting PC1 vs PC2
plot(fclim.pc2$x[,1:2], type='p', col=c("blue", "red", "green")[unclass(gpx3)])

#-screeplot 
screeplot(fclim.pc2)
