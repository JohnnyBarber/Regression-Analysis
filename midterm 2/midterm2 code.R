#install.packages("jtools")

#1.data-------------------------------------------------------------------------------------------
cue=c(rep("auditory",9),rep("visual",9))
elapsed.time=rep(rep(1:3,each=3),2)
reaction.time=c(204,170,181,167,182,187,202,198,236,257,279,269,283,235,260,256,281,258)

#1.1-------------------------------------------------------------------------------------------
reaction=data.frame(cue=cue,elapsed.time=elapsed.time,reaction.time=reaction.time)

par(mfrow=c(1,2))
plot(reaction.time~elapsed.time,main="Reaction Time versus Elapsed Time")
plot(reaction.time~reaction$cue,main="Reaction Time versus Cue")

#1.2-------------------------------------------------------------------------------------------
library(lsmeans)
lsm.reaction=aov(reaction.time~cue+elapsed.time+cue:elapsed.time,data=reaction)
par(mfrow=c(2,2))
plot(lsm.reaction)

anova(lsm.reaction)

library(jtools)
interact_plot(lsm.reaction,pred="elapsed.time",modx="cue")

reaction.cue=lsmeans(lsm.reaction,~cue)
contrast(reaction.cue, method="pairwise")

#2-------------------------------------------------------------------------------------------
Brand<-c(rep(1,10), rep(2, 10), rep(3,10), rep(4, 10))
Times<-c(167, 171, 178, 175, 184, 176, 185, 172, 178, 178,
         231, 233, 236, 252, 233, 225, 241, 248, 239, 248,
         176, 168, 171, 172, 178, 176, 169, 164, 169, 171,
         201, 199, 196, 211, 209, 223, 209, 219, 212, 210)

melt=data.frame(Brand,Times)
lsm.melt=aov(Times~Brand,data=melt)
par(mfrow=c(2,2))
plot(lsm.melt)

lsm.melt.transf=aov(sin(Times)~Brand,data=melt)
par(mfrow=c(2,2))
plot(lsm.melt.transf)

shapiro.test(lsm.melt.transf$residuals)

#3.2-------------------------------------------------------------------------------------------
wood=read.table("wood.csv",header=TRUE)
wood

#Type<-as.factor(wood$Type)
#Species<-as.factor(wood$Species)
#Nconc<-wood$Nconc
#df=data.frame(Type=Type,Species=Species,Nconc=Nconc)

options(contrasts =c("contr.sum", "contr.poly"))

lsm.wood=aov(Nconc~Type/Species,data=wood)
par(mfrow=c(2,2))
plot(lsm.wood)

anova(lsm.wood)

par(mfrow=c(1,1))
interaction.plot(x.factor = wood$Species, trace.factor = wood$Type, response = wood$Nconc, 
                 type ="b",col = 2:3,xlab ="Species", ylab ="nitrogen content", trace.label ="Type")

library(multcompView)

wood.ST=lsmeans(lsm.wood, ~Species:Type)
contrast(wood.ST, method="pairwise")
#cld(wood.ST, alpha=0.05)

wood.T=lsmeans(lsm.wood, ~Type)
cld(wood.T, alpha=0.05)

#4.2-------------------------------------------------------------------------------------------
beans=read.table("Beans.csv",header=TRUE)
beans

lsm.tast=aov(Rating~SoakTim+Crock:SoakTim+Recipe+SoakTim:Recipe,data=beans)
par(mfrow=c(2,2))
plot(lsm.tast)

anova(lsm.tast)

interact_plot(lsm.tast,pred="SoakTim",modx="Recipe")
