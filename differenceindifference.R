difdata<-as.data.frame(did)
difdata$week<-as.factor(difdata$week)
did1<-lm(formula = difdata$excess~difdata$treat*difdata$after,data=difdata)
did2<-lm(formula = excess~treat+treat:difdata$`after-1`+treat:difdata$`after-2`+treat:difdata$`after-3`+treat:difdata$after1+treat:difdata$after2+treat:difdata$after3+treat:difdata$after4 ,data=difdata)