library(PoEdata)
data(mexican)
library(dplyr)
library(tidyverse)
library(magrittr)
library(plm)

fe1<-plm(lnprice~+regular+rich+alcohol+nocondom+bar+street, 
         data=mexican, model='within', effect='individual')
summary(fe1)
fixef(fe1)
#15.8
data("nls_panel2")
#(a)
a=nls_panel2[1:716,c(2,3,15,16,12,14)]
lm1=lm(lwage~exper+I(exper^2)+south+union,data=a)
summary(lm1)
b=nls_panel2[717:1432,c(2,3,15,16,12,14)]
lm2=lm(lwage~exper+I(exper^2)+south+union,data=b)
summary(lm2)
#(b)
poo=plm(lwage~exper+I(exper^2)+south+union,data=nls_panel2
        ,model='pooling',effect='individual')
summary(poo)

#(d)
library(lmtest)
library(multiwayvcov)
fe=plm(lwage~exper+I(exper^2)+south+union,data=nls_panel2
        ,model='within',effect='individual')
summary(fe)
coeftest(fe, vcov=vcovHC(fe, type="sss", cluster="group")) 



