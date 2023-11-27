#10.7
#(c)
data("chard")
mod3<-lm(xper~age+cap+lab,data=chard)
v<-mod3$residuals
mod4<-lm(q~xper+cap+lab+v,data=chard)
summary(mod4)

10.9
#(a)
library(PoEdata)
data(newbroiler)

time=c(11:50)
mod1<-lm(log(qprod[c(11:50)])~log(p[c(11:50)])+log(pf[c(11:50)])
         +time+log(qprod[c(10:49)]),data=newbroiler)
summary(mod1)

(b)
library(AER)
mod2<-ivreg(log(qprod[c(11:50)])~log(p[c(11:50)])+log(pf[c(11:50)])+time
            +log(qprod[c(10:49)])
            |log(y[c(11:50)])+log(pb[c(11:50)])+popgro[c(11:50)]+log(p[c(10:49)])+lexpts[c(11:50)]
            +log(pf[c(11:50)])+time+log(qprod[c(10:49)])
            ,data=newbroiler)
summary(mod2)

(c)
mod3<-lm(log(p[c(11:50)])~log(y[c(11:50)])+log(pb[c(11:50)])
         +popgro[c(11:50)]+log(p[c(10:49)])+lexpts[c(11:50)]
         +log(pf[c(11:50)])+time+log(qprod[c(10:49)])
         ,data=newbroiler)
summary(mod3)
mod4<-lm(log(qprod[c(11:50)])~mod3$residuals+log(p[c(11:50)])+log(pf[c(11:50)])
         +time+log(qprod[c(10:49)]),data=newbroiler)
summary(mod4)


(d)
mod9<-lm(log(p[c(11:50)])~log(p[c(10:49)])+log(y[c(11:50)])+log(pb[c(11:50)])
         +popgro[c(11:50)]++lexpts[c(11:50)]
         ,data=newbroiler)
summary(mod9)


(e)
mod8<-lm(mod2$residuals~log(y[c(11:50)])+log(pb[c(11:50)])
         +popgro[c(11:50)]+log(p[c(10:49)])+lexpts[c(11:50)]
         +log(pf[c(11:50)])+time+log(qprod[c(10:49)])
         ,data=newbroiler)
rs2<-summary(mod8)$r.squared
rs2
N<-40
q<-N*rs2
q
L<-5
B<-1
pvalue<-1-pchisq(q,L-B)
pvalue

11.6
data(truffles)
head(truffles)

modD<-lm(q~p+ps+di,data=truffles)
summary(modD)


modS<-lm(q~p+pf,data=truffles)
summary(modS)