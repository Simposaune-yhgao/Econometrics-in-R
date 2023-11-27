
J=read.csv("JCVWX.csv",skip=2,header=F)
N=read.csv("NWJAX.csv",skip=2,header=F)
V=read.csv("VFINX.csv",skip=2,header=F)
FF5=read.csv("ff5.csv",skip=1,header=F)
mkt_rf=read.csv("mkt_rf.csv",skip=1,header=F)

colnames(J)=c("Date","O","H","L","C","Adj_close","return")
colnames(N)=c("Date","O","H","L","C","Adj_close","return")
colnames(V)=c("Date","O","H","L","C","Adj_close","return")
colnames(mkt_rf)=c("Date","mkt-rf","rf","mkt","smb","hml","mom","rmw","cma")
colnames(FF5)=c("Date","mkt-rf","smb","hml","rmw","cma","rf")

return_J=J$return
return_N=N$return
return_V=V$return
return_mkt=mkt_rf$mkt

smb=mkt_rf$smb
hml=mkt_rf$hml
mom=mkt_rf$mom


Jmean=mean(return_J)
Nmean=mean(return_N)
Vmean=mean(return_V)
rf=mkt_rf$rf
rf_mean=mean(rf)
mkt_excess=mkt_rf$`mkt-rf`
mkt_excess_mean=mean(mkt_excess)

mkt_excess_5=FF5$`mkt-rf`
smb_5=FF5$smb
hml_5=FF5$hml
rmw_5=FF5$rmw
cma_5=FF5$cma
rf_5=FF5$rf

FF3_J=lm((return_J-rf)~mkt_excess+smb+hml)
summary(FF3_J)
alphaJ=Jmean-(rf_mean+1.14136*mkt_excess_mean
              +0.03484*mean(smb)+0.59431*mean(hml))
FF3_N=lm((return_N-rf)~mkt_excess+smb+hml)
summary(FF3_N)
alphaN=Nmean-(rf_mean+1.2265*mkt_excess_mean
              -0.2079*mean(smb)+0.2399*mean(hml))
FF3_V=lm((return_V-rf)~mkt_excess+smb+hml)
summary(FF3_V)
alphaV=Vmean-(rf_mean+0.98624*mkt_excess_mean
              -0.163414*mean(smb)-0.1423*mean(hml))
alphaJ
alphaN
alphaV

FF4_J=lm((return_J-rf)~mkt_excess+smb+hml+mom)
summary(FF4_J)
alphaJ=Jmean-(rf_mean+1.12809*mkt_excess_mean
              +0.03617*mean(smb)+.55008*mean(hml)+-0.06024*mean(mom))

FF4_N=lm((return_N-rf)~mkt_excess+smb+hml+mom)
summary(FF4_N)
alphaN=Nmean-(rf_mean+1.12809*mkt_excess_mean
              +0.03617*mean(smb)+0.55008*mean(hml)-0.6024*mean(mom))

FF4_V=lm((return_V-rf)~mkt_excess+smb+hml+mom)
summary(FF4_V)
alphaV=Vmean-(rf_mean+0.98209*mkt_excess_mean
              -0.16299*mean(smb)-0.02803*mean(hml)-0.01881*mean(mom))

alphaJ
alphaN
alphaV

FF5_J=lm((return_J-rf_5)~mkt_excess_5+smb_5+hml_5+rmw_5+cma_5)
summary(FF5_J)
alphaJ=Jmean-(rf_mean+1.14854*mkt_excess_mean
              -0.06788*mean(smb)+0.62473*mean(hml)-0.36311*mean(rmw_5)+0.00747*mean(cma_5))
FF5_N=lm((return_N-rf_5)~mkt_excess_5+smb_5+hml_5+rmw_5+cma_5)
summary(FF5_N)
alphaN=Nmean-(rf_mean+1.229028*mkt_excess_mean
              -0.20662*mean(smb)+0.226730*mean(hml)+0.002914*mean(rmw_5)+0.031249*mean(cma_5))
FF5_V=lm((return_V-rf_5)~mkt_excess_5+smb_5+hml_5+rmw_5+cma_5)
summary(FF5_V)
alphaV=Vmean-(rf_mean+0.99046*mkt_excess_mean
              -0.19180*mean(smb)-0.01687*mean(hml)-0.10173*mean(rmw_5)+0.02925*mean(cma_5))
alphaJ
alphaN
alphaV

