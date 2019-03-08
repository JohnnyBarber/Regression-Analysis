install.packages("lsmeans")
install.packages("multcompView")
install.packages("car")

library(lsmeans)
library(multcompView) 
library(car) 
options(contrasts = c("contr.sum", "contr.poly"))

#1-----------------------------------------------------------------------------------------------------------
Fert<-c(rep("control", 12), rep("f1", 12), rep("f2", 12), rep("f3", 12))
Species<-c(rep(c(rep("SppA", 6), rep("SppB", 6)),4))
Height<-c(21.0, 19.5, 22.5, 21.5, 20.5, 21.0,
          23.7, 23.8, 23.8, 23.7, 22.8, 24.4,
          32.0, 30.5, 25.0, 27.5, 28.0, 28.6,
          30.1, 28.9, 30.9, 34.4, 32.7, 32.7,
          22.5, 26.0, 28.0, 27.0, 26.5, 25.2,
          30.6, 31.1, 28.1, 34.9, 30.1, 25.5,
          28.0, 27.5, 31.0, 29.5, 30.0, 29.2,
          36.1, 36.6, 38.7, 37.1, 36.8, 37.1)
df<-data.frame(Fert=Fert, Species=Species, Height=Height)

modelF1<-aov(Height~Fert+Species+Fert:Species, data=df)
par(mfrow=c(2,2))
plot(modelF1)
anova(modelF1)

df$LogH<-log(Height)
df$SqrtH<-sqrt(Height)
df$InvH<-1/Height

modelF2<-aov(SqrtH~Fert+Species+Fert:Species, data=df)
par(mfrow=c(2,2))
plot(modelF2)

modelF3<-aov(LogH~Fert+Species+Fert:Species, data=df)
par(mfrow=c(2,2))
plot(modelF3)

modelF4<-aov(InvH~Fert+Species+Fert:Species, data=df)
par(mfrow=c(2,2))
plot(modelF4)

anova(modelF3)

interaction.plot(x.factor = df$Species, trace.factor = df$Fert,
                 response = df$LogH, type ="b",col = 2:3,
                 xlab ="stain", ylab ="Mean", trace.label ="concentration")

interaction.plot(x.factor = df$Species, trace.factor = df$Fert,
                 response = df$Height, type ="b",col = 2:3,
                 xlab ="stain", ylab ="Mean", trace.label ="concentration")

anova(modelF1)
lsmInter=lsmeans(modelF1, ~Fert:Species)
contrast(lsmInter, mathod="pairwise")
cld(lsmInter,alpha=0.05)

lsmS=lsmeans(modelF3, ~Species)
contrast(lsmS, method="pairwise")

lsmF=lsmeans(modelF3, ~Fert)
contrast(lsmF, method="pairwise")
cld(lsmF, alpha=0.05)

#2-----------------------------------------------------------------------------------------------------------
A<-c(rep("1", 10), rep("2", 10))
B<-rep(c(c(rep("1", 5), rep("2", 5))), 2)
resp<-c(12.9, 11.3, 11.7, 12.1, 12.3,
        13.7, 12.8, 13.6, 13.1, 13.5,
        14.2, 14.5, 13.9, 13.6, 14.4,
        13.5, 13.1, 13.3, 13.1, 13.4)
df2<-data.frame(A=A, B=B, resp=resp)

model1<-aov(resp~A+B+A:B, data=df2)
par(mfrow=c(2,2))
plot(model1)

df2$LogR<-log(resp)
df2$SqrtR<-sqrt(resp)
df2$InvR<-1/resp

model2<-aov(SqrtR~A+B+A:B, data=df2)
par(mfrow=c(2,2))
plot(model2)

model3<-aov(LogR~A+B+A:B, data=df2)
par(mfrow=c(2,2))
plot(model3)

model4<-aov(InvR~A+B+A:B, data=df2)
par(mfrow=c(2,2))
plot(model4)

anova(model1)

interaction.plot(x.factor = df2$B, trace.factor = df2$A,
                 response = df2$resp, type ="b",col = 2:3,
                 xlab ="stain", ylab ="Mean", trace.label ="concentration")

lsmAB=lsmeans(model1, ~ A:B )
contrast(lsmAB,method="pairwise")
cld(lsmAB, alpha=0.05)

lsmA=lsmeans(model1, ~A)
contrast(lsmA, method="pairwise")

