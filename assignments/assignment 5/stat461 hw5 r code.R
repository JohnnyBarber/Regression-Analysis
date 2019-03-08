#1---------------------------------------------------------------------------------------------------------------
drug=c(rep("Drug A",2),rep("Drug B",2),rep("Control",2))
effect=c(-14,-4,5,-1,-2,6)
data=data.frame(drug=as.factor(drug),effect=effect)
data

install.packages("knitr")
library(knitr)
model1=aov(effect~drug,data=data)
kable(anova(model1),format="markdown")

install.packages("lsmeans")
library(lsmeans)
lsm.drug=lsmeans(model1, ~drug)
kable(summary(contrast(lsm.drug,method="pairwise",adjust="tukey"),infer=c(T,T),level=0.5,side="two-sided"))

#2---------------------------------------------------------------------------------------------------------------
type=c(rep("Regular",4),rep("Deordorant",4),rep("Moisturizing",4))
weightloss=c(-0.30,-0.10,-0.14,0.40,2.63,2.61,2.41,3.15,1.86,2.03,2.26,1.82)
experiment=data.frame(type,weightloss)
experiment

model2=aov(weightloss~type,data=experiment)
kable(anova(model2),format="markdown")

#3---------------------------------------------------------------------------------------------------------------
push=c(rep("0",7),rep("1",10),rep("2",10),rep("3",5))
time=c(38.14,38.20,38.31,38.14,38.29,38.17,38.20,38.28,38.17,38.08,38.25,38.18,38.03,37.95,38.26,38.30,38.21,38.17,38.13,38.16,38.30,38.34,38.34,38.17,38.18,38.09,38.06,38.14,38.30,38.21,38.04,38.37)
light=data.frame(push,time)
light

model3=aov(time~push,data=light)
kable(anova(model3),format="markdown")

lsm.light=lsmeans(model3, ~push)
kable(summary(contrast(lsm.light,method="pairwise",adjust="tukey"),infer=c(T,T),level=0.5,side="two-sided"))
