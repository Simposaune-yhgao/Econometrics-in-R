df <- read.table("C:/Users/User/Downloads/health.dat", 
                 header = TRUE)

mod1<-lm(mghb~edu.level,data=df)
mod1
-0.2962*6+11.8764

bachelor=df[which(df$edu.level==5),]

x=bachelor$mghb
mean(x, trim = 0, na.rm = T)+var(x,na.rm=T)/sqrt(1602)*1.96
mean(x, trim = 0, na.rm = T)-var(x,na.rm=T)/sqrt(1602)*1.96
cor(df$mghb,df$edu.level,use="pairwise.complete.obs")