install.packages("lsmeans")
install.packages("car")
install.packages("multcompView")
install.packages("lme4")
install.packages("lmerTest")

library(lsmeans)
library(car)
library(multcompView)
library(lme4)
library(lmerTest)
options(contrasts = c("contr.sum", "contr.poly"))

displays=read.table("displays.dat",header=TRUE)
displays$store=as.factor(displays$store)
displays$week=as.factor(displays$week)
displays

df=data.frame(store=as.factor(displays$store), week=as.factor(displays$week),sales=as.factor(displays$sales))

#exp=aov(sales~store+display+store:display, data=displays)
#summary(exp)
#anova(exp)

#experiment----------------------------------------------------------------------
exp2=lmer(sales~(1|store)+display, data=displays)
summary(exp2)
fitted2=fitted(exp2)
resid2=residuals(exp2)
par(mfrow=c(2,2))
plot(fitted2,resid2,main="Fitteed vs Residuals",pch=1,cex=2)
qqnorm(resid2)
qqline(resid2)
#---------------------------------------------------------------------------------

model=lmer(sales~(1|store)+display+(1|store:display), data=displays)
summary(model)

fitted=fitted(model)
resid=residuals(model)
par(mfrow=c(2,2))
plot(fitted,resid,main="Fitteed vs Residuals",pch=1,cex=2)
qqnorm(resid)
qqline(resid)

Anova(model,type="III")

rand(model)

difflsmeans(model,"display")
