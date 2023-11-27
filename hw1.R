require(dummies)
library(devtools)
library(PoEdata)
library(bookdown)
library(Knitr)
library(xtable)
library(printr)
library(stargazer)
library(rmarkdown)

data<-read.csv("C:/Users/User/Desktop/economtericsHw/NCHS_-_Death_rates_and_life_expectancy_at_birth.csv", header=T, sep=",")
data=data.frame(data)
data=alldummy_data


 datam<-data[which(data$SexMale==1),]
 datam
 dataf<-data[which(data$SexFemale==1),]
 dataw<-data[which(data$RaceWhite==1),]
 datab<-data[which(data$RaceBlack==1),]
 

t.test(dataw$Average.Life.Expectancy..Years.,datab$Average.Life.Expectancy..Years.,paired=TRUE )
ggplot(data , aes(x=data$Average.Life.Expectancy..Years.)) + geom_density()

ggplot(dataf,aes(x=datab$Average.Life.Expectancy..Years.))+ geom_density()
ggplot(datam,aes(x=datam$Average.Life.Expectancy..Years.))+ geom_density()
