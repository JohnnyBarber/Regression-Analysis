type<-c("AlkName","AlkName","AlkName","AlkName","AlkGen","AlkGen","AlkGen","AlkGen","HDName","HDName","HDName","HDName","HDGen","HDGen","HDGen","HDGen")
life<-c(100.668,77.734,79.210,95.063,206.880,153.347,165.980,196.000,14.951,18.063,11.111,12.840,15.340,22.090,15.734,14.440)
batt<-data.frame(type=type, life=life)

plot(batt)

fit1<-aov(life~type,data=batt)
par(mfrow=c(2,2))
plot(fit1)

batt$SqrtLife<-sqrt(life)
batt$LogLife<-log(life)
batt$SqLife<-life*life

fit2<-aov(SqrtLife~type,data = batt)
par(mfrow=c(2,2))
plot(fit2)

fit3<-aov(LogLife~type,data = batt)
par(mfrow=c(2,2))
plot(fit3)

fit4<-aov(SqLife~type, data = batt)
par(mfrow=c(2,2))
plot(fit4)

summary(fit3)
install.packages("knitr")
library(knitr)
install.packages("lsmeans")
library(lsmeans)
lsm.life=lsmeans(fit3, ~type)
kable(summary(contrast(lsm.life,method="pairwise",adjust="tukey"),infer=c(T,T),level=0.5,side="two-sided"))

#8---------------------------------------------------------------------------------------------------------------
hotdog=read.table("hotdogs.txt",header=TRUE)
hotdog
attach(hotdog)
plot(Calories~Type)

m1<-aov(Calories~Type,data=batt)
par(mfrow=c(2,2))
plot(m1)

hotdog$SqrtC<-sqrt(Calories)
hotdog$LogC<-log(Calories)
hotdog$SqC<-Calories*Calories
hotdog$InvC<-1/Calories
hotdog$TrirtC<-Calories^(1/3)

m2<-aov(SqrtC~Type,data =hotdog)
par(mfrow=c(2,2))
plot(m2)

m3<-aov(LogC~Type,data = hotdog)
par(mfrow=c(2,2))
plot(m3)

m4<-aov(SqC~Type, data = hotdog)
par(mfrow=c(2,2))
plot(m4)

m5<-aov(InvC~Type, data = hotdog)
par(mfrow=c(2,2))
plot(m5)

m6<-aov(TrirtC~Type, data = hotdog)
par(mfrow=c(2,2))
plot(m6)

summary(m5)
lsm.hotdog=lsmeans(m5, ~Type)
kable(summary(contrast(lsm.hotdog,method="pairwise",adjust="tukey"),infer=c(T,T),level=0.5,side="two-sided"))
