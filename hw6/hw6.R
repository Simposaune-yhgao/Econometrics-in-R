#10.6
data(ivreg2)
#(e) Z1和e、z2和e的corr 接近0 ，顯示z1 z2是好的工具變數x和z1的corr更大 所以z1比z2更好

cor(ivreg2$z1,ivreg2$x)
cor(ivreg2$z1,e)
cor(ivreg2$z2,ivreg2$x)
cor(ivreg2$z2,e)
cor(ivreg2$z1,ivreg2$z2)
cor(e,ivreg2$x)


#(f)隨樣本數增加 工具變數的beta愈來愈靠近true value 反映工具變數是consistent


mod10_6f_N10=ivreg(ivreg2$y[1:10]~ivreg2$x[1:10]|ivreg2$z1[1:10])
summary(mod10_6f_N10)
mod10_6f_N20=ivreg(ivreg2$y[1:20]~ivreg2$x[1:20]|ivreg2$z1[1:20])
summary(mod10_6f_N20)
mod10_6f_N100=ivreg(ivreg2$y[1:100]~ivreg2$x[1:100]|ivreg2$z1[1:100])
summary(mod10_6f_N100)
mod10_6f_N500=ivreg(ivreg2$y~ivreg2$x|ivreg2$z1)
summary(mod10_6f_N500)


#(g)隨樣本數增加 z2的beta愈來愈靠近true value 跟z1比較起來z1在樣本數小的時候比較準確


mod10_6g_N10=ivreg(ivreg2$y[1:10]~ivreg2$x[1:10]|ivreg2$z2[1:10])
summary(mod10_6g_N10)
mod10_6g_N20=ivreg(ivreg2$y[1:20]~ivreg2$x[1:20]|ivreg2$z2[1:20])
summary(mod10_6g_N20)
mod10_6g_N100=ivreg(ivreg2$y[1:100]~ivreg2$x[1:100]|ivreg2$z2[1:100])
summary(mod10_6g_N100)
mod10_6g_N500=ivreg(ivreg2$y~ivreg2$x|ivreg2$z2)
summary(mod10_6g_N500)

#(h)隨樣本數增加 beta愈來愈靠近true value 但是兩個工具變數的結果和z1的結果相差不遠，只有稍微改善

mod10_6h_N10=ivreg(ivreg2$y[1:10]~ivreg2$x[1:10]|ivreg2$z1[1:10]+ivreg2$z2[1:10])
summary(mod10_6h_N10)
mod10_6h_N20=ivreg(ivreg2$y[1:20]~ivreg2$x[1:20]|ivreg2$z1[1:20]+ivreg2$z2[1:20])
summary(mod10_6h_N20)
mod10_6h_N100=ivreg(ivreg2$y[1:100]~ivreg2$x[1:100]|ivreg2$z1[1:100]+ivreg2$z2[1:100])
summary(mod10_6h_N100)
mod10_6h_N500=ivreg(ivreg2$y~ivreg2$x|ivreg2$z1+ivreg2$z2)
summary(mod10_6h_N500)

#10.7
data(chard)

#(a)
#Q=1.7623+0.1468XPER+ .4380CAP+0.2392LAB
#係數是正的，符合預期。且皆為顯著
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

#(ii)
var_q=var_e+var_b1+var_b2*xper_ii*xper_ii+cap_0*cap_0*var_b3+lab_0*lab_0*var_b4+2*xper_ii*cov_b1b2+2*cap_0*cov_b1b3+2*lab_0*cov_b1b4+2*xper_ii*cap_0*cov_b2b3+2*xper_ii*lab_0*cov_b2b4+2*cap_0*lab_0*cov_b3b4
se_q=sqrt(var_q)
q_head=b1+b2*xper_ii+b3*cap_0+b4*lab_0
L=q_head-tcr*se_q
R=q_head+tcr*se_q

#(iii)
var_q=var_e+var_b1+var_b2*xper_iii*xper_iii+cap_0*cap_0*var_b3+lab_0*lab_0*var_b4+2*xper_iii*cov_b1b2+2*cap_0*cov_b1b3+2*lab_0*cov_b1b4+2*xper_iii*cap_0*cov_b2b3+2*xper_iii*lab_0*cov_b2b4+2*cap_0*lab_0*cov_b3b4
se_q=sqrt(var_q)
q_head=b1+b2*xper_iii+b3*cap_0+b4*lab_0
L=q_head-tcr*se_q
R=q_head+tcr*se_q

#(d)
#Q =-2.4867+ 0.5121 XPER+ 0.3321 CAP+ 0.2400 LAB
#和(a)比較，XPER CAP 的係數改變很多，LAB則差別不大。所有係數接顯著
mod10_7d=ivreg(chard$q~chard$xper+chard$cap+chard$lab|chard$age+chard$cap+chard$lab)
summary(mod10_7d)
