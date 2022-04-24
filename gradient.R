gg<-as.data.frame(excessdeath_mobility)
gg$Week<-as.factor(gg$Week)

#Using Loess
g1<-loess(ed18~Week,data = gg,span=0.1)
g25<-loess(ed18~Week,data = gg,span=0.25)
g50<-loess(ed18~Week,data = gg,span=0.50)
g75<-loess(ed18~Week,data = gg,span=0.75)
plot(gg$ed18,x=gg$Week)
lines(g25,col="red")
lines(g50,col="green")

#Using Cubic Spline
library(pspline)
p1 <- smooth.Pspline(gg$Week, gg$ed18, method=3)
f0 <- predict(p1, gg$Week, nderiv=0)
f1 <- predict(p1, gg$Week, nderiv=1)
gg$grad<-f1
d1<-lm(grad~(driving+transit+walking)^2 ,data=gg)

#FOR EDF
p2 <- smooth.Pspline(gg$Week, gg$edf20, method=3)
h0<-predict(p2, gg$Week, nderiv=0)
h1 <- predict(p2, gg$Week, nderiv=1)
gg$edfgrad<-h1 
d2<-lm(edfgrad~(driving+transit+walking)^2 ,data=gg)
d3<-lm(edfgrad~driving+transit+walking+driving:transit+driving:walking, data=gg)


#BOX COX
library(MASS)
bc<-boxcox(gg$edfgrad~(gg$driving+gg$transit+gg$walking)^2)