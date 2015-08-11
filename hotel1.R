f.hotel1<-function(x,mu0)
{
  #  The arguments to this function are 
  #  the data matrix  x
  #  and the hypothesized mean vector  mu0
  
  #  Compute the dimensions of the array x
  dimx<-dim(x)
  
  #  Compute sample mean vector and sample 
  #  covariance matrix
  xmean<-apply(x,2,mean)
  s<-var(x)
  
  #  Compute the inverse of sample covariance matrix
  invs<-solve(s)
  
  #  Compute one sample Hotelling T-squared test
  HotelT2<-dimx[1]*t(as.matrix(xmean-mu0))%*%
    invs%*%as.matrix(xmean-mu0)
  
  #  Convert the T-squared value to an F-value 
  #  and compute df and the p-value
  
  fval<-(dimx[1]-dimx[2])*HotelT2/(dimx[2]*(dimx[1]-1))
  df1<-dimx[2]
  df2<-dimx[1]-dimx[2]
  pval<-1-pf(fval,df1,df2)
  cat("T-squared=",HotelT2,fill=TRUE)
  df<-cbind(df1,df2)
  cat("F-value=",fval," df=",df," p-value=",pval)
  
  return(HotelT2)
}