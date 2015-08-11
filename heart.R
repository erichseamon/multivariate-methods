heart<-read.table("http://www.webpages.uidaho.edu/~stevel/519/Data/SAheart.txt")
head(heart)
pairs(heart, col=heart$chd+1, pch=heart$chd+1)
heart.cart<-rpart(chd~., data=heart, method="class")
heart.cart

attach(heart)
table(age<50.5)
table(chd[age<50.5])

plot(heart.cart,margin=.2)
text(heart.cart, use.n=FALSE, pretty=0)
