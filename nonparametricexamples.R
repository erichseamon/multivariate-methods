sleep
extra group ID
1    0.7     1  1
2   -1.6     1  2
3   -0.2     1  3
4   -1.2     1  4
5   -0.1     1  5
6    3.4     1  6
7    3.7     1  7
8    0.8     1  8
9    0.0     1  9
10   2.0     1 10
11   1.9     2  1
12   0.8     2  2
13   1.1     2  3
14   0.1     2  4
15  -0.1     2  5
16   4.4     2  6
17   5.5     2  7
18   1.6     2  8
19   4.6     2  9
20   3.4     2 10
> sleep1 <- with(sleep, extra[group==2] - extra[group ==1])
> summary(sleep1)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.00    1.05    1.30    1.58    1.70    4.60 
> ?with
> with?
+ )
> ?with
> sample(c(-1,1),10,rep=T)
[1] -1 -1 -1  1  1 -1 -1  1  1 -1
> sample(c(-1,1),10,rep=T)
[1]  1 -1 -1  1 -1 -1  1 -1 -1 -1
> j <- sample(c(-1,1),10,rep=T)
> sleep1*j
[1] -1.2  2.4  1.3 -1.3  0.0  1.0  1.8 -0.8 -4.6 -1.4
> mean(sleep1*j)
[1] -0.28
> mean(sleep1*j)
[1] -0.28
> mean(sleep1*j)
[1] -0.28
> j <- sample(c(-1,1),10,rep=T)
> j
[1]  1 -1  1  1 -1 -1 -1 -1  1 -1
> mean(sleep1*j)
[1] 0.1
> mean(sleep1*sample(c(-1.1),10,rep=T))
[1] -1.738
> for (i : 1:B) cls[i]=mean(sleep1*sample(c(-1,1),10,rep=T))
> xls2
> B=10^3
> xls2=c()
> for (i : 1:B) cls[i]=mean(sleep1*sample(c(-1,1),10,rep=T))
> sls2
> xls2
NULL
> xls2
NULL
> xls2
NULL
> for (i : 1:B) cls[i]=mean(sleep1*sample(c(-1,1),10,rep=T))
> sum(xls2>=1.58 | xls2<=-1.58)/B
[1] 0
> B=10^3
> xls2=c()
> for (i : 1:B) xls2[i]=mean(sleep1*sample(c(-1,1),10,rep=T))
> xls2
NULL
> for (i : 1:B) xls2[i]=mean(sleep1*sample(c(-1,1),10,rep=T))
> xls2
NULL
> n1=10
> n2=20
> orig.gp=c(rep(1,n1),rep(2,n2))
> perm.ds1=sample(orig.gp)
> perm.ds1