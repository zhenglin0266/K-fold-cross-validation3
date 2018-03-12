##K-fold cross-validation##
X<- rnorm(1000)
X<-cbind(1,poly(X,df=5))
pa<-c(1, 5, 4, 3, 2, 1)
Y<- X%*%pa + rnorm(1000)
n<-100
nn<-100
res<-matrix(0, n, 5)
g<-matrix(0, n, 5)
gg<-c()
RS1<- matrix(0, nn, 5)
RS2<- matrix(0, nn, 5)
R1<-c()
R2<-c()
kkk<-1
for(k in c(5, 10, 20, 50, 100)){
  for(q in 1: n){
    numb <- sample(c(1: 1000), size=1000, replace=T)
    y <- Y[numb]
    x <- X[numb,]
    
    RS<-c()
    for(b in 2:50){
      RSS<- c()
      for(i in 1:k){
        kk<-1000/k
        xx <- x[-(((i-1)*kk+1):(i*kk)),2] 
        yy <- y[-(((i-1)*kk+1):(i*kk))]
        fit = smooth.spline(xx, yy, df=b)
        fity = predict(fit, x[((i-1)*kk+1):(i*kk),2])
        RSS[i]<-sum((y[((i-1)*kk+1):(i*kk)]-fity$y)^2)/kk
      }
      RS[b] <- mean(RSS)
    }
    g[q, kkk] <- which.min(RS)
    res[q, kkk]<-RS[g[q, kkk]]
  }
  gg[kkk] <- round(mean(g[,kkk]))
  
  ##Test#
  for(j in 1: nn){
    numb <- sample(c(1: 1000), size=1000, replace=T)
    y <- Y[numb]
    x <- X[numb,]
    RSS1<-c()
    RSS2<-c()
    for(i in 1: k){
      kk<-1000/k
      xx <- x[-(((i-1)*kk+1):(i*kk)),2] 
      yy <- y[-(((i-1)*kk+1):(i*kk))]
      fit1 = smooth.spline(xx, yy, df=gg[kkk])
      fity1 = predict(fit1, x[((i-1)*kk+1):(i*kk),2])
      RSS1[i]<-sum((y[((i-1)*kk+1):(i*kk)]-fity1$y)^2)/kk 
      fit2 = smooth.spline(xx, yy, cv=FALSE)
      fity2 = predict(fit2, x[((i-1)*kk+1):(i*kk),2])
      RSS2[i]<-sum((y[((i-1)*kk+1):(i*kk)]-fity2$y)^2)/kk
    }
    RS1[j, kkk] <- mean(RSS1)
    RS2[j, kkk] <- mean(RSS2)
  }
  
  R1[kkk]<-mean(RS1[, kkk])
  R2[kkk]<-mean(RS2[, kkk])
  kkk<-kkk+1
}
cat("For five K values, the means of errors of CV method are", R1, ",and the means of errors of GCV method are", R2)

##Firstly, we set degrees from 2 to 50, and get 100 optimal degrees after 100
##repeats. For each k, we calculate the average value of the 100 optimal drgrees and 
## regard it as the best degree for the k.

##Secondly, with the best degree, we use K-fold cross-validation to estimate the 
##model and get the errors, and then we use GCV method to estimate the model and get 
##the errors. We repeat this process for 100 times, and calculate the means of
##the errors of the two methods. Next, we compare the errors of CV method with
##the errors of GCV method.

##In summary, for the five K values, we see that the means of errors of CV method
##are about 1.0, and the means of errors of GCV method are about 1.05, so the errors
##of the two method are similar, and the errors of GCV method are a little bigger 
##than those of CV method. Thus, when we compare the two methods with errors, 
##CV method with the best degrees is a little better than GCV method.
