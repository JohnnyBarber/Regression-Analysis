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
options(contrasts =c("contr.sum", "contr.poly"))

#1.data-------------------------------------------------------------------------------------------
cue=c(rep("auditory",9),rep("visual",9))
elapsed.time=rep(rep(1:3,each=3),2)
reaction.time=c(204,170,181,167,182,187,202,198,236,257,279,269,283,235,260,256,281,258)

#1.1-------------------------------------------------------------------------------------------
reaction=data.frame(cue=cue,elapsed.time=as.factor(elapsed.time),reaction.time=reaction.time)

par(mfrow=c(1,2))
plot(reaction$reaction.time~reaction$elapsed.time,main="Reaction Time versus Elapsed Time")
plot(reaction$reaction.time~reaction$cue,main="Reaction Time versus Cue")

#1.2-------------------------------------------------------------------------------------------
lsm.reaction=aov(reaction.time~cue+elapsed.time+cue:elapsed.time,data=reaction)
par(mfrow=c(2,2))
plot(lsm.reaction)

anova(lsm.reaction)

interact_plot(lsm.reaction,pred="cue",modx="elapsed.time")

unloadNamespace("lmerTest")
library(lsmeans)

reaction.cue=lsmeans(lsm.reaction,~cue)
cld(reaction.cue,alpha=0.05)

#2-------------------------------------------------------------------------------------------
Brand<-c(rep(1,10), rep(2, 10), rep(3,10), rep(4, 10))
Times<-c(167, 171, 178, 175, 184, 176, 185, 172, 178, 178,
         231, 233, 236, 252, 233, 225, 241, 248, 239, 248,
         176, 168, 171, 172, 178, 176, 169, 164, 169, 171,
         201, 199, 196, 211, 209, 223, 209, 219, 212, 210)

melt=data.frame(Brand=as.factor(Brand),Times=Times)
lsm.melt=aov(Times~Brand,data=melt)

summary(lsm.melt)

par(mfrow=c(2,2))
plot(lsm.melt)

anova(lsm.melt)

#3.2-------------------------------------------------------------------------------------------
wood=read.table("wood.csv",header=TRUE)
wood

lsm.wood=aov(Nconc~Type/Species,data=wood)
par(mfrow=c(2,2))
plot(lsm.wood)

anova(lsm.wood)

par(mfrow=c(1,1))
interaction.plot(x.factor = wood$Species, trace.factor = wood$Type, response = wood$Nconc, 
                 type ="b",col = 2:3,xlab ="Species", ylab ="nitrogen content", trace.label ="Type")

wood.ST=lsmeans(lsm.wood, ~Species:Type)
contrast(wood.ST, method="pairwise")

wood.T=lsmeans(lsm.wood, ~Type)
cld(wood.T, alpha=0.05)

#4.2--------------------------------------------------------------------------------------------
beans=read.table("Beans.csv",header=TRUE)
beans

Crock=as.factor(beans$Crock)
SoakTim=beans$SoakTim
Recipe=beans$Recipe
Jar=as.factor(beans$Jar)
Rating=beans$Rating

beans.frame=data.frame(Crock=Crock,SoakTim=SoakTim,Recipe=Recipe,Jar=Jar,Rating=Rating)

library(lmerTest)

lsm.taste=lmer(Rating~SoakTim+(1|Crock:SoakTim)+Recipe+SoakTim:Recipe,data=beans.frame)

par(mfrow=c(1,2))

fitted=fitted(lsm.taste)
resid=residuals(lsm.taste)
plot(fitted,resid,main="Fitted vs Residuals",pch=1,cex=2)

qqnorm(resid)
qqline(resid)

lsm.taste1=lmer(1/(Rating)~SoakTim+(1|Crock:SoakTim)+Recipe+SoakTim:Recipe,data=beans.frame)

fitted1=fitted(lsm.taste1)
resid1=residuals(lsm.taste1)
plot(fitted1,resid1,main="Fitted vs Residuals",pch=1,cex=2)

qqnorm(resid1)
qqline(resid1)

lsm.taste2=lmer(sqrt(Rating)~SoakTim+(1|Crock:SoakTim)+Recipe+SoakTim:Recipe,data=beans.frame)

fitted2=fitted(lsm.taste2)
resid2=residuals(lsm.taste2)
plot(fitted2,resid2,main="Fitted vs Residuals",pch=1,cex=2)

qqnorm(resid2)
qqline(resid2)

lsm.taste3=lmer(log(Rating)~SoakTim+(1|Crock:SoakTim)+Recipe+SoakTim:Recipe,data=beans.frame)

fitted3=fitted(lsm.taste3)
resid3=residuals(lsm.taste3)
plot(fitted3,resid3,main="Fitted vs Residuals",pch=1,cex=2)

qqnorm(resid3)
qqline(resid3)

Anova(lsm.taste,type="III")

difflsmeans(lsm.taste, "SoakTim:Recipe")

rand(lsm.taste)