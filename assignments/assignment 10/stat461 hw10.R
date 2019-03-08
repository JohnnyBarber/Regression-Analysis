#install.packages("lmerTest")
#install.packages("lsmeans")
#install.packages("car")
#install.packages("multcompView")
#install.packages("lme4")
#install.packages("jtools")

library(lsmeans)
library(car)
library(multcompView)
library(lme4)
library(lmerTest)
library(jtools)
options(contrasts = c("contr.sum", "contr.poly"))

#1---------------------------------------------------
cars <- mtcars[,c("am","mpg","hp")]
head(cars)

am<-as.factor(cars$am)
mpg<-cars$mpg
hp<-cars$hp
df<-data.frame(am=am,mpg=mpg,hp=hp)

cars.ancova=aov(mpg~am+hp+hp:am,data=df)
par(mfrow=c(2,2))
plot(cars.ancova)

cars.ancova=aov(sqrt(mpg)~am+hp+hp:am,data=df)
par(mfrow=c(2,2))
plot(cars.ancova)

anova(cars.ancova)

interact_plot(cars.ancova,pred="hp",modx="am")

cars.am=lsmeans(cars.ancova,~as.factor(am))
cld(cars.am,alpha=0.05)

#2---------------------------------------------------
hsb2= read.table("hsb2.csv")
head(hsb2)

ses<-as.factor(hsb2$ses)
schtyp<-as.factor(hsb2$schtyp)
write<-hsb2$write
math<-hsb2$math
df2=data.frame(ses=ses,schtyp=schtyp,write=write,math=math)

hsb2.ancova=aov(math~ses+schtyp+write+schtyp:write+ses:write+ses:schtyp,data=df2)
par(mfrow=c(2,2))
plot(hsb2.ancova)

shapiro.test(hsb2.ancova$residuals)

anova(hsb2.ancova)

interact_plot(hsb2.ancova,pred="write",modx="ses")

hsb2.ses=lsmeans(hsb2.ancova,~as.factor(ses))
cld(hsb2.ses,alpha=0.05)
