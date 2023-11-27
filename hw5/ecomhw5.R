library(devtools)
library(PoEdata)
library(bookdown)
library(Knitr)
library(xtable)
library(printr)
library(stargazer)
library(rmarkdown)
setwd("C:\Users\User\Desktop\economtericsHw")

##13
sqft2=br2$sqft^2
age2=br2$age^2
m2=lm(price ~ sqft + sqft2 + age + age2+ sqft*age,data = br2)
summary(m2)
min(br2$age)
-30.73+0.044*66.2-0.93*20
-30.73+0.044*2300-0.93*20
-30.73+0.044*7897-0.93*20
51.87-2.483




##19d
educxexper = cps4_small$educ*cps4_small$exper
educ2=cps4_small$educ^2
exper2=cps4_small$exper^2
lnwage=log(cps4_small$wage)
model2=lm(lnwage ~ educ + exper + hrswk + educxexper + educ2 + exper2, data = cps4_small)

summary(model2)

#10-4a
data("ivreg1")
x=ivreg1$x
e=ivreg1$e
y=1+1*x+e
summary(x)
summary(y)
summary(e)
#10-4b
plot(x,y)
abline(a=1,b=1)
#沒有隨機分布在回歸線周圍 x<0時資料分布在回歸線下，x>0在線之上
#10-4C
model3=lm(y~x)
model3
#y=1.001+1.989x
#10-4d
abline(reg=model3,col="red")
#10-4e
e2=y-1.001-1.989*x
mat=cbind(x,e,e2)
cor(mat)

#10-6a
#corr=cov(x,e)/sqrt(x)*sqrt(e)=0.6364
#10-6b
data("ivreg2")
y=ivreg2$y
x=ivreg2$x
e2=y-3-x
cor(x,e2)
#10-6c
plot(x,y)
abline(3,1)
#10-6d
model7=lm(y~x)
y10=ivreg2$y[1:10]
x10=ivreg2$x[1:10]
y100=ivreg2$y[1:100]
x100=ivreg2$x[1:100]
y500=ivreg2$y[1:500]
x500=ivreg2$x[1:500]
model4=lm(y10~x10)
model5=lm(y100~x100)
model6=lm(y500~x500)
model4
model5
model6
model7

#10.6
data(ivreg2)

#(e)
cor(ivreg2$z1,ivreg2$x)
cor(ivreg2$z1,e2)
cor(ivreg2$z2,ivreg2$x)
cor(ivreg2$z2,e2)
cor(ivreg2$z1,ivreg2$z2)
cor(e2,ivreg2$x)
#f
mod10_6f_N10=ivreg(ivreg2$y[1:10]~ivreg2$x[1:10]|ivreg2$z1[1:10])
summary(mod10_6f_N10)
mod10_6f_N20=ivreg(ivreg2$y[1:20]~ivreg2$x[1:20]|ivreg2$z1[1:20])
summary(mod10_6f_N20)
mod10_6f_N100=ivreg(ivreg2$y[1:100]~ivreg2$x[1:100]|ivreg2$z1[1:100])
summary(mod10_6f_N100)
mod10_6f_N500=ivreg(ivreg2$y~ivreg2$x|ivreg2$z1)
summary(mod10_6f_N500)
#(g)
mod10_6g_N10=ivreg(ivreg2$y[1:10]~ivreg2$x[1:10]|ivreg2$z2[1:10])
summary(mod10_6g_N10)
mod10_6g_N20=ivreg(ivreg2$y[1:20]~ivreg2$x[1:20]|ivreg2$z2[1:20])
summary(mod10_6g_N20)
mod10_6g_N100=ivreg(ivreg2$y[1:100]~ivreg2$x[1:100]|ivreg2$z2[1:100])
summary(mod10_6g_N100)
mod10_6g_N500=ivreg(ivreg2$y~ivreg2$x|ivreg2$z2)
summary(mod10_6g_N500)






data(chard)

#(a)
mod10_7=lm(chard$q~chard$xper+chard$cap+chard$lab)
summary(mod10_7)
#(b)
b1=coef(mod10_7)[[1]]
b2=coef(mod10_7)[[2]]
b3=coef(mod10_7)[[3]]
b4=coef(mod10_7)[[4]]
var_b1=vcov(mod10_7)[1,1]
var_b2=vcov(mod10_7)[2,2]
var_b3=vcov(mod10_7)[3,3]
var_b4=vcov(mod10_7)[4,4]
cov_b1b2=vcov(mod10_7)[1,2]
cov_b1b3=vcov(mod10_7)[1,3]
cov_b1b4=vcov(mod10_7)[1,4]
cov_b2b3=vcov(mod10_7)[2,3]
cov_b2b4=vcov(mod10_7)[2,4]
cov_b3b4=vcov(mod10_7)[3,4]
cap_0=mean(chard$cap)
lab_0=mean(chard$lab)
xper_i=10
xper_ii=20
xper_iii=30
var_e=summary(mod10_7)$sigma
df=mod10_7$df.residual
tcr=qt(1-0.05/2,df)
#(i)
var_q=var_e+var_b1+var_b2*xper_i*xper_i+cap_0*cap_0*var_b3+lab_0*lab_0*var_b4+2*xper_i*cov_b1b2+2*cap_0*cov_b1b3+2*lab_0*cov_b1b4+2*xper_i*cap_0*cov_b2b3+2*xper_i*lab_0*cov_b2b4+2*cap_0*lab_0*cov_b3b4
se_q=sqrt(var_q)
q_head=b1+b2*xper_i+b3*cap_0+b4*lab_0
L=q_head-tcr*se_q
R=q_head+tcr*se_q
var_q
se_q
L
9.0647+1.9939*1.734
var_q=var_e+var_b1+var_b2*xper_ii*xper_ii+cap_0*cap_0*var_b3+lab_0*lab_0*var_b4+2*xper_ii*cov_b1b2+2*cap_0*cov_b1b3+2*lab_0*cov_b1b4+2*xper_ii*cap_0*cov_b2b3+2*xper_ii*lab_0*cov_b2b4+2*cap_0*lab_0*cov_b3b4
se_q=sqrt(var_q)
q_head=b1+b2*xper_ii+b3*cap_0+b4*lab_0
L=q_head-tcr*se_q
2.802*2.802
#(d)

mod10_7d=ivreg(chard$q~chard$xper+chard$cap+chard$lab|chard$age+chard$cap+chard$lab)
summary(mod10_7d)
R=q_head+tcr*se_q