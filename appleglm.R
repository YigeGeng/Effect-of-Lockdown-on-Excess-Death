
#For excess death
dd<-as.data.frame(excessdeath_mobility)
a1<-lm(ed18~(driving+transit+walking)^2 ,data=dd)
plot(fitted(a1),resid(a1),main="Residual Plot",xlab = "Predicted",ylab = "Residuals")
bc2<-boxCox(dd$ed18~(dd$driving+dd$transit+dd$walking)^2, family="yjPower", plotit = TRUE)
lambda2 <- bc2$x[which.max(bc2$y)]
dd$ed18bc<- yjPower(dd$ed18, lambda2)
a3<-lm(ed18bc~(driving+transit+walking)^2 ,data=dd)
kable(tidy(a3))
qqnorm(resid(a3))
qqline(resid(a3), col = "steelblue", lwd = 2)
a4<-lm(ed18bc~driving+transit+walking+driving:walking+transit:walking ,data=dd)

require(broom) # for tidy()bc3
require(knitr) # for kable()
aout<-tidy(a1)
kable(aout)
a2<-lm(ed18~driving+transit+walking+driving:walking+transit:walking,data=dd)
anova(a2,a1)
aout2<-tidy(a2)
kable(aout2)


#FOR EDF
library(MASS)
bc<-boxcox(dd$edf20~(dd$driving+dd$transit+dd$walking)^2)
lambda <- bc$x[which.max(bc$y)]
b3<-lm(((edf20^lambda-1)/lambda) ~ (driving+transit+walking)^2 ,data=dd)
b4<-lm(((edf20^lambda-1)/lambda)~driving+transit+walking+driving:walking+transit:walking,data=dd)
anova(b4,b3)
b4out<-tidy(b4)
kable(b4out)

dd$transedf<-(dd$edf20^lambda-1)/lambda
t1<-lm(transedf ~ (driving+transit+walking)^2 ,data=dd)


b1<-lm(edf20~(driving+transit+walking)^2 ,data=dd)
rb1<-resid(b1)
plot(fitted(b1),rb1)
bout<-tidy(b1)
kable(bout)
b2<-lm(edf20~driving+transit+walking+driving:walking+transit:walking,data=dd)
anova(b2,b1)
bout2<-tidy(b2)
kable(bout2)


#Using ln for edf
gl1<-glm(observed~(driving+transit+walking)^2 ,family=poisson(),offset = log(expected),data=dd)
gl2<-glm.nb(observed~(driving+transit+walking)^2+offset(log(expected)),data=dd)
gl3<-glm(observed~driving+transit+walking+driving:walking+transit:walking,family=poisson(),offset = log(expected),data=dd)
gl4<-glm.nb(observed~driving+transit+walking+driving:walking+transit:walking+offset(log(expected)),data=dd)