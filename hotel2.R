f.hotel2<-function(x1,x2)
{
  #  The arguments to this function are the 
  #  data matrices  x1  and  x2
  #  for the two treatment groups
  
  #  Compute the dimensions of the data 
  #  matrices
  p<-dim(x1)[2]
  n1<-dim(x1)[1]
  n2<-dim(x2)[1]
  
  #  Compute sample mean vector and sample 
  #  covariance matrix for each treatment group
  
  m1<-apply(x1,2,mean)
  s1<-var(x1)
  m2<-apply(x2,2,mean)
  s2<-var(x2)
  
  #  Compute the pooled estimate of covariance 
  #  matrix
  
  s<-((n1-1)*s1+(n2-1)*s2)/(n1+n2-2) 
  
  #  Compute the two sample Hotelling T-squared test
  
  T2<-((n1*n2)/(n1+n2))*t(m1-m2)%*%solve(s)%*%(m1-m2)
  
  #  Convert the T-squared value to an F-value and compute df
  #  and the p-value
  
  fval<-(n1+n2-p-1)*T2/(p*(n1+n2-2))
  df1<-p
  df2<-n1+n2-p-1
  pval<-1-pf(fval,df1,df2)
  cat("T-squared=",T2,fill=TRUE)
  df<-cbind(df1,df2)
  cat("F-value=",fval," df=",df," p-value=",pval,fill=T)
  
  #  Compute Bartlett's test for homogeneity 
  #  of covariance matrices.  First compute 
  #  eigenvalues for each covariance matrix.
  
  es<-eigen(s)
  es1<-eigen(s1)
  es2<-eigen(s2)
  
  M<-(n1+n2-2)*log(prod(es$values))
  M<-M-(n1-1)*log(prod(es1$values))
  -(n2-1)*log(prod(es2$values))
  CI<-1-((2*p*p+3*p-1)/(6*(p+1)))*((1/(n1-1))
                                   +(1/(n2-1))-(1/(n1+n2-2)))
  B<-CI*M
  df<-p*(p+1)/2
  pval<-1-pchisq(B,df)
  
  cat("Bartlett Test = ",B," df = ",df," p-value = ",pval,fill=T)
  
  #  Compute a Wald test when the covariance 
  #  matrices are not homogeneous
  
  W<-t(m1-m2)%*%solve(s1/n1+s2/n2)%*%(m1-m2)
  
  #  Compute p-value from large sample 
  #  chi-square approximation
  
  df<-p
  pval<-1-pchisq(W,df)
  cat("Large sample chi-square test for equal mean vectors ",fill=T)
  cat("when the covariance matrices are nonhomogeneous:",fill=T)
  cat("Wald test = ",W," df = ",df," p-value = ",pval,fill=T)
  
  #  Compute p-value from an F- approximation
  
  fval<-(n1+n2-p-1)*W/(p*(n1+n2-2))
  df1<-p
  df2<-n1+n2-p-1
  pval<-1-pf(fval,df1,df2)
  cat("Large sample F-test for equal mean vectors when the",fill=T)
  cat("covariance matrices are nonhomogeneous:",fill=T)
  df<-cbind(df1,df2)
  cat("F-value=",fval," df=",df," p-value=",pval,fill=T)
  
  return(T2) }

#  This function can be applied to the steel 
#  example done in lecture by first creating 
#  a data martix  x  from the data file 
#             steel.dat
#  The first column of this file contains values 
#  used to identify the two treatment groups, 
#  the second column contains the yield point 
#  measurements and the third column contains   
#  the ultimate strength measurements.
#
#     x<-matrix(scan("steel.dat"), ncol=3,byrow=T)
#     x
#
#  Then create separate data matrices for the 
#  two treatment groups.
#
#      p<-dim(x)[2]  	
#      x1<-x[x[,1]=="1",2:p]
#      x2<-x[x[,1]=="2",2:p]
#
#  Source the function into R
#
#    source("hotel2.R")
#
#  Execute the function
#
#    f.hotel2(x1,x2)

