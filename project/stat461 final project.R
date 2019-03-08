library(lsmeans)
library(car)
library(multcompView)
library(lme4)

options(contrasts = c("contr.sum", "contr.poly"))

numdrops=c(17, 21, 21, 22, 27, 24, 13,15,13,14,14,14,11,12,12,15,15,13,
           16,15,18,13,17,12,10,12,12,11,10,11,11,11,11,12,13,14)
tissue=c(rep("ultra",12),rep("lotion",12),rep("everyday",12))
liquid=rep(c(rep("water",3),rep("pepsi",3),rep("milk",3),rep("soy",3)),3)

data1=data.frame(tissue=tissue,liquid=liquid,numdrops=numdrops)

model1 <- aov(numdrops~tissue+liquid+tissue:liquid,data=data1)

par(mfrow=c(2,2))

plot(model1)

model2 <- aov(1/(numdrops)~tissue+liquid+tissue:liquid,data=data1)
plot(model2)

anova(model2)

lm_interaction=lsmeans(model2,~tissue:liquid)
cld(lm_interaction,alpha=0.05)

par(mfrow=c(1,1))
interaction.plot(x.factor = data1$tissue, trace.factor = data1$liquid, response = data1$numdrops, 
                 type ="b",col = 2:3,xlab ="Species", ylab ="nitrogen content", trace.label ="Type")

lm_tissue=lsmeans(model2,~tissue)
cld(lm_tissue,alpha=0.05)
