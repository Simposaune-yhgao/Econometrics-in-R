install.packages("devtools")  # if not already installed
library(devtools)
install_git("https://github.com/ccolonescu/PoEdata")

#####2.10b
capm=data(capm4)
mod1 <- lm(dis-riskfree ~ mkt-riskfree, data = capm4 )
coef(mod1)[[1]]
mod1 <- lm(ge-riskfree ~ mkt-riskfree, data = capm4)
coef(mod1)[[1]]
mod1 <- lm(gm-riskfree ~ mkt-riskfree, data = capm4)
coef(mod1)[[1]]
mod1 <- lm(ibm-riskfree ~ mkt-riskfree, data = capm4)
coef(mod1)[[1]]
modelmsft <- lm(msft-riskfree ~ mkt-riskfree, data = capm4)
bm <- coef(mod1)[[1]]
mod1 <- lm(xom-riskfree ~ mkt-riskfree, data = capm4)
 coef(mod1)[[1]]

#####2.10c
 MP=capm4$mkt-capm4$riskfree
 rf=capm4$riskfree
 modelmsft1=lm((capm4$msft-rf)~MP+0)
 modelmsft1
 plot(MP,(capm4$msft-rf),xlab="market premium",ylab="msft - risk-free", col="blue")
 abline(modelmsft$coefficients[1],modelmsft$coefficients[2], col="red")
##2.10d
capm=data(capm4)
mod1 <- lm(dis-riskfree ~0+ mkt-riskfree, data = capm4 )
bdis <- coef(mod1)[[1]]
mod1 <- lm(ge-riskfree ~ 0+mkt-riskfree, data = capm4)
bge <- coef(mod1)[[1]]
mod1 <- lm(gm-riskfree ~0+ mkt-riskfree, data = capm4)
bgm <- coef(mod1)[[1]]
mod1 <- lm(ibm-riskfree ~ 0+mkt-riskfree, data = capm4)
bibm <- coef(mod1)[[1]]
mod1 <- lm(msft-riskfree ~ 0+mkt-riskfree, data = capm4)
bm <- coef(mod1)[[1]]
mod1 <- lm(xom-riskfree ~ 0+mkt-riskfree, data = capm4)
bx <- coef(mod1)[[1]]

#####2.13a

library(PoEdata)   # loads the package in memory
?andy              # shows dataset information
data(andy)         # loads the dataset in memory
data("star")
i=1
star[1]
a=0
temp1=0
temp2=0
for (i in  1 : 5768){
  if (star$small[i]==1|star$regular[i]==1){
    
    a=a+1
    temp1[a]=star$totalscore [i]
    temp2[a]=star$small[i]
 
    }
}
dat=data.frame(tosc=temp1,sm=temp2)
mod2=lm(tosc ~sm , data = dat )
summary(mod2)

#####2.13b
a=0
temp1=0
temp2=0
for (i in  1 : 5768){
  if (star$small[i]==1|star$regular[i]==1){
    
    a=a+1
    temp1[a]=star$mathscore [i]
    temp2[a]=star$small[i]
    
  }
}
dat=data.frame(tosc=temp1,sm=temp2)
mod2=lm(tosc ~sm , data = dat )
summary(mod2)

#####2.13c
a=0
temp1=0
temp2=0
for (i in  1 : 5768){
  if (star$small[i]==0){
    
    a=a+1
    temp1[a]=star$totalscore [i]
    temp2[a]=star$aide [i]
    
  }
}
dat=data.frame(tosc=temp1,sm=temp2)
mod2=lm(tosc ~sm , data = dat )
summary(mod2)

#####2.13d
a=0
temp1=0
temp2=0
for (i in  1 : 5768){
  if (star$small[i]==0){
    
    a=a+1
    temp1[a]=star$mathscore [i]
    temp2[a]=star$aide [i]
    
  }
}
dat=data.frame(tosc=temp1,sm=temp2)
mod2=lm(tosc ~sm , data = dat )
summary(mod2)

a=0
temp1=0
temp2=0
for (i in  1 : 5768){
  if (star$small[i]==0){
    
    a=a+1
    temp1[a]=star$readscore [i]
    temp2[a]=star$aide [i]
    
  }
}
dat=data.frame(tosc=temp1,sm=temp2)
mod2=lm(tosc ~sm , data = dat )
summary(mod2)

?cps4
#####2.15a
data(cps4_small)
summary(cps4_small$wage)
hist(cps4_small$wage, 20)
summary(cps4_small$educ)
hist(cps4_small$educ, 20)

#####2.15b
model15_b=lm(cps4_small$wage ~ cps4_small$educ, data=cps4_small)
summary(model15_b)

#####2.15c
resi=residuals(model15_b)
plot(cps4_small$educ, resi, col="blue")

#####2.15d
d1=subset(cps4_small,cps4_small$female==0)
d2=subset(cps4_small,cps4_small$female==1)
d3=subset(cps4_small,cps4_small$black==1)
d4=subset(cps4_small,cps4_small$black==0)
d4=subset(d4,d4$asian==0)
model15d1= lm(d1$wage ~ d1$educ, data=d1)
model15d1
model15d2= lm(d2$wage ~ d2$educ, data=d2)
model15d2
model15d3= lm(d3$wage ~ d3$educ, data=d3)
model15d3
model15d4= lm(d4$wage ~ d4$educ)
model15d4

#####2.15e 
edu2=(cps4_small$educ)^2
model15e= lm(cps4_small$wage ~ edu2)
summary(model15e)
2*coef(model15e)[[2]]*12
2*coef(model15e)[[2]]*14
#####2.15f
plot(cps4_small$educ, cps4_small$wage, col="blue")
abline(model15_b$coefficients[1],model15_b$coefficients[2])
curve(model15e$coefficients[1]+model15e$coefficients[2]*x^2, add=TRUE, col="red")

#####2.15g
lnwage=log(cps4_small$wage)
hist(lnwage, 20)

#####2.15h
model15f = lm(lnwage ~ cps4_small$educ)
model15f
resi2=residuals(model15f)

coef(model15f)[[2]]*exp(1.6+coef(model15f)[[2]]*12)
coef(model15f)[[2]]*exp(1.6+coef(model15f)[[2]]*14)

model15f2 =lm(lnwage ~ edu2)
plot(cps4_small$educ,cps4_small$wage, xlab = "educ", ylab = "wage")
abline(model15_b$coefficients[1],model15_b$coefficients[2], col="red")
curve(model15e$coefficients[1]+model15e$coefficients[2]*x^2, add=TRUE)
curve(exp(model15f$coefficients[1]+model15f$coefficients[2]*x), col="purple",add=TRUE)

