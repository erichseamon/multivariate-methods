> library(mvtnorm)
> Tibet<-read.table("http://www.uidaho.edu/~stevel/519/R.CMA/chap7tibetskull.txt")
Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
  In file(file, "rt") : cannot open: HTTP status was '404 Not Found'
> Tibet<-read.table("http://www.webpages.uidaho.edu/~stevel/519/R.CMA/chap7tibetskull.txt")
> Tibet<-read.table("http://www.uidaho.edu/~stevel/519/R.CMA/chap7tibetskull.txt")
Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
  In file(file, "rt") : cannot open: HTTP status was '404 Not Found'
> #
  > attach(Tibet)
> #
  > m1<-apply(Tibet[Type==1,-6],2,mean)
> m2<-apply(Tibet[Type==2,-6],2,mean)
> l1<-length(Type[Type==1])
> l2<-length(Type[Type==2])
> x1<-Tibet[Type==1,-6]
> x2<-Tibet[Type==2,-6]
> S123<-((l1-1)*var(x1)+(l2-1)*var(x2))/(l1+l2-2)
> T2<-t(m1-m2)%*%solve(S123)%*%(m1-m2)
> #
  > Fstat<-(l1+l2-5-1)*T2/(l1+l2-2)*5
> pvalue<-1-pf(Fstat,5,26)
> #
  > Fstat
[,1]
[1,] 15.17291
> pvalue
[,1]
[1,] 5.261755e-07
> #
  > m1<-apply(Tibet[Type==1,-6],2,mean)
> m2<-apply(Tibet[Type==2,-6],2,mean)
> l1<-length(Type[Type==1])
> l2<-length(Type[Type==2])
> x1<-Tibet[Type==1,-6]
> x2<-Tibet[Type==2,-6]
> S123<-((l1-1)*var(x1)+(l2-1)*var(x2))/(l1+l2-2)
> a<-solve(S123)%*%(m1-m2)
> z12<-(m1%*%a+m2%*%a)/2
> z1<-m1%*%a
> z2<-m2%*%a
> z12
[,1]
[1,] -30.46349
> z1
[,1]
[1,] -28.71277
> z2
[,1]
[1,] -32.21421
> #
  > a
[,1]
Length   -0.089306662
Breadth   0.155774683
Height    0.005231617
Fheight  -0.177194601
Fbreadth -0.177408670
> z12
[,1]
[1,] -30.46349
> #
  > 
  > #
  > library(MASS)
> dis<-lda(Type~Length+Breadth+Height+Fheight+Fbreadth,data=Tibet,prior=c(0.5,0.5))
> #
  > #
  > newdata<-rbind(c(171,140.5,127.0,69.5,137.0),c(179.0,132.0,140.0,72.0,138.5))
> colnames(newdata)<-colnames(Tibet)[-6]
> #
  > newdata<-data.frame(newdata)
> predict(dis,newdata=newdata)
$class
[1] 1 2
Levels: 1 2

$posterior
1         2
1 0.7545066 0.2454934
2 0.1741016 0.8258984

$x
LD1
1 -0.6000350
2  0.8319908

> #
  > #
  > group<-predict(dis,method="plug-in")$class
> #
  > table(group,Type)
Type
group  1  2
1 14  3
2  3 12
> #
  > #
  > #
  > #
  > skulls<-read.table("http://www.uidaho.edu/~stevel/519/R.CMA/chap5skulls.txt")
Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
  In file(file, "rt") : cannot open: HTTP status was '404 Not Found'
> #
  > attach(skulls)
Error in attach(skulls) : object 'skulls' not found
> skulls.manova<-manova(cbind(MB,BH,BL,NH)~EPOCH)
Error in cbind(MB, BH, BL, NH) : object 'MB' not found
> summary(skulls.manova,test="Pillai")
Error in summary(skulls.manova, test = "Pillai") : 
  error in evaluating the argument 'object' in selecting a method for function 'summary': Error: object 'skulls.manova' not found
> summary(skulls.manova,test="Wilks")
Error in summary(skulls.manova, test = "Wilks") : 
  error in evaluating the argument 'object' in selecting a method for function 'summary': Error: object 'skulls.manova' not found
> summary(skulls.manova,test="Hotelling")
Error in summary(skulls.manova, test = "Hotelling") : 
  error in evaluating the argument 'object' in selecting a method for function 'summary': Error: object 'skulls.manova' not found
> summary(skulls.manova,test="Roy")
Error in summary(skulls.manova, test = "Roy") : 
  error in evaluating the argument 'object' in selecting a method for function 'summary': Error: object 'skulls.manova' not found
> #
  > chisplot(residuals(skulls.manova))
Error: could not find function "chisplot"
> #
  > #
  > #
  > dsfs1<-c(0.13,-0.04,-0.15,0.08)%*%t(skulls[,-1])
Error in t(skulls[, -1]) : 
  error in evaluating the argument 'x' in selecting a method for function 't': Error: object 'skulls' not found
> dsfs2<-c(0.04,0.21,-0.068,-0.08)%*%t(skulls[,-1])
Error in t(skulls[, -1]) : 
  error in evaluating the argument 'x' in selecting a method for function 't': Error: object 'skulls' not found
> m1<-c(mean(dsfs1[1:30]),mean(dsfs1[31:60]),mean(dsfs1[61:90]),mean(dsfs1[91:120]),mean(dsfs1[121:150]))
Error in mean(dsfs1[1:30]) : 
  error in evaluating the argument 'x' in selecting a method for function 'mean': Error: object 'dsfs1' not found
> m2<-c(mean(dsfs2[1:30]),mean(dsfs2[31:60]),mean(dsfs2[61:90]),mean(dsfs2[91:120]),mean(dsfs2[121:150]))
Error in mean(dsfs2[1:30]) : 
  error in evaluating the argument 'x' in selecting a method for function 'mean': Error: object 'dsfs2' not found
> plot(m1,m2,type="n",xlab="CV1",ylab="CV2",xlim=c(0.5,3))
> text(m1,m2,labels=c("c4000BC","c3300BC","c1850BC","c200BC","cAD150"))
> 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > 
  > newdata<-rbind(c(171,140.5,127.0,69.5,137.0),c(179.0,132.0,140.0,72.0,138.5))
> colnames(newdata)<-colnames(Tibet)[-6]
> #
  > newdata
Length Breadth Height Fheight Fbreadth
[1,]    171   140.5    127    69.5    137.0
[2,]    179   132.0    140    72.0    138.5
> source('/svn/REACCH/trunk/reacch-r-development/r-scripts/STAT519/7.R')
The following objects are masked from Tibet (pos = 3):
  
  Breadth, Fbreadth, Fheight, Height, Length, Type

Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
  In file(file, "rt") : cannot open: HTTP status was '404 Not Found'
> Tibet<-read.table("http://www.webpages.uidaho.edu/~stevel/519/R.CMA/chap7tibetskull.txt")
> #
  > attach(Tibet)
The following objects are masked from Tibet (pos = 3):
  
  Breadth, Fbreadth, Fheight, Height, Length, Type

The following objects are masked from Tibet (pos = 4):
  
  Breadth, Fbreadth, Fheight, Height, Length, Type

> #
  > m1<-apply(Tibet[Type==1,-6],2,mean)
> m2<-apply(Tibet[Type==2,-6],2,mean)
> l1<-length(Type[Type==1])
> l2<-length(Type[Type==2])
> x1<-Tibet[Type==1,-6]
> x2<-Tibet[Type==2,-6]
> S123<-((l1-1)*var(x1)+(l2-1)*var(x2))/(l1+l2-2)
> T2<-t(m1-m2)%*%solve(S123)%*%(m1-m2)
> #
  > Fstat<-(l1+l2-5-1)*T2/(l1+l2-2)*5
> pvalue<-1-pf(Fstat,5,26)
> #
  > Fstat
[,1]
[1,] 15.17291
> pvalue
[,1]
[1,] 5.261755e-07
> #
  > m1<-apply(Tibet[Type==1,-6],2,mean)
> m2<-apply(Tibet[Type==2,-6],2,mean)
> l1<-length(Type[Type==1])
> l2<-length(Type[Type==2])
> x1<-Tibet[Type==1,-6]
> x2<-Tibet[Type==2,-6]
> S123<-((l1-1)*var(x1)+(l2-1)*var(x2))/(l1+l2-2)
> a<-solve(S123)%*%(m1-m2)
> z12<-(m1%*%a+m2%*%a)/2
> z1<-m1%*%a
> z2<-m2%*%a
> z12
[,1]
[1,] -30.46349
> z1
[,1]
[1,] -28.71277
> z2
[,1]
[1,] -32.21421
> #
  > t1=Tibet[Type==1, 1:5]
> t2=Tibet[Type==2, 1:5]
> m1(mean(t1))
Error: could not find function "m1"
> m1(mean(t1))
Error: could not find function "m1"
> m1=mean(m1)
> m1=colMeans(t1)
> m2(colMeans(t2))
Error: could not find function "m2"
> m2=colMeans(2)
Error in colMeans(2) : 'x' must be an array of at least two dimensions
> m2=colMeans(t2)
> s1=var(t1)
> s1
Length   Breadth   Height   Fheight Fbreadth
Length   45.52941 25.222426 12.39062 22.154412 27.97243
Breadth  25.22243 57.805147 11.87500  7.519301 48.05515
Height   12.39062 11.875000 36.09375 -0.312500  1.40625
Fheight  22.15441  7.519301 -0.31250 20.935662 16.76930
Fbreadth 27.97243 48.055147  1.40625 16.769301 66.21140
> s2(var(t2))
Error: could not find function "s2"
> s2=var(t2)
> s2
Length     Breadth     Height    Fheight  Fbreadth
Length   74.423810  -9.5226190  22.736905 17.7940476 11.125000
Breadth  -9.522619  37.3523810 -11.263095  0.7047619  9.464286
Height   22.736905 -11.2630952  36.316667 10.7238095  7.196429
Fheight  17.794048   0.7047619  10.723810 15.3023810  8.660714
Fbreadth 11.125000   9.4642857   7.196429  8.6607143 17.964286
>  d1 = sum((newdata[1,]-m1)^2)
>  d2 = sum((newdata[1,]-m2)^2)
> d1;d2
[1] 85.22318
[1] 329.2978
>  d1 = sum((newdata[2,]-m1)^2)
>  d2 = sum((newdata[2,]-m2)^2)
> d1;d2
[1] 206.6202
[1] 139.0144
> ?mahalanobis
> ma1 <- mahalanobis(newdata,center=m1,cov=s1)
> ma1
[1]  2.406343 16.247860
> ma1 <- mahalanobis(newdata,center=m2,cov=s2)
> ma1;ma2
[1] 5.344742 6.453361
Error: object 'ma2' not found
> ma1:ma2
Error: object 'ma2' not found
> ma1
[1] 5.344742 6.453361
> ma2
Error: object 'ma2' not found
> ma1 <- mahalanobis(newdata,center=m1,cov=s1)
> ma2 <- mahalanobis(newdata,center=m2,cov=s2)
> ma1
[1]  2.406343 16.247860
> ma2
[1] 5.344742 6.453361
> d1/d2
[1] 1.486322
> ma1
[1]  2.406343 16.247860
> ma1(1)
Error: could not find function "ma1"
> ma1[1]
[1] 2.406343
> ma1[1]/ma1[2]
[1] 0.1481021
> d1/(d1+d2);d2/(d1+d2)
[1] 0.5977995
[1] 0.4022005
> ma1/(ma1+ma2)
[1] 0.3104524 0.7157263
> library(MASS)
> dis<-lda(Type~Length+Breadth+Height+Fheight+Fbreadth,data=Tibet,prior=c(0.5,0.5))
> #
  > #
  > newdata<-rbind(c(171,140.5,127.0,69.5,137.0),c(179.0,132.0,140.0,72.0,138.5))
> colnames(newdata)<-colnames(Tibet)[-6]
> #
  > newdata<-data.frame(newdata)
> predict(dis,newdata=newdata)
$class
[1] 1 2
Levels: 1 2

$posterior
1         2
1 0.7545066 0.2454934
2 0.1741016 0.8258984

$x
LD1
1 -0.6000350
2  0.8319908

> ?lda