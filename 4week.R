#lfour
lag4$Week<-as.factor(lag4$Week)
f1<-lm(lfour~(driving+transit+walking)^2 ,data=lag4)
kable(tidy(f1))
plot(fitted(f1),resid(f1),xlab="predicted",ylab="fitted")
bf<-boxCox(lag4$lfour~(lag4$driving+lag4$transit+lag4$walking)^2, family="yjPower", plotit = TRUE)
lambdaf <- bf$x[which.max(bf$y)]
lag4$lfourbc<- yjPower(lag4$lfour, lambdaf)
f2<-lm(lfourbc~(driving+transit+walking)^2 ,data=lag4)
f3<-lm(lfourbc~driving+transit+walking+driving:walking+transit:walking,data=lag4)
kable(tidy(f3))

#edflfour
bf2<-boxcox(lag4$edf4~(lag4$driving+lag4$transit+lag4$walking)^2)
lambdaf2 <- bc$x[which.max(bc$y)]
g1<-lm(edf4 ~ (driving+transit+walking)^2 ,data=lag4)
plot(fitted(g1),resid(g1),xlab="predicted",ylab="fitted")
g2<-lm(((edf4^lambdaf2-1)/lambdaf2) ~ (driving+transit+walking)^2 ,data=lag4)
g3<-lm(((edf4^lambdaf2-1)/lambdaf2)~driving+transit+walking+driving:walking+transit:walking,data=lag4)
kable(tidy(g3))

#Gradient
library(pspline)
h1 <- smooth.Pspline(lag4$Week, lag4$lfour, method=3)
h00 <- predict(h1, lag4$Week, nderiv=0)
h01 <- predict(h1, lag4$Week, nderiv=1)
lag4$lfourgrad<-h01
k1<-lm(lfourgrad~(driving+transit+walking)^2 ,data=lag4)

#EDF Gradient
h2 <- smooth.Pspline(lag4$Week, lag4$edf4, method=3)
h02 <- predict(h1, lag4$Week, nderiv=0)
h03 <- predict(h1, lag4$Week, nderiv=1)
lag4$edf4grad<-h03
k2<-lm(edf4grad~(driving+transit+walking)^2 ,data=lag4)