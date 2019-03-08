widgets=read.table("widgets.txt",header=TRUE)
Batch=as.character(widgets$Batch)
Supplier=widgets$Supplier
WidgetSize=widgets$WidgetSize

df=data.frame(Supplier=Supplier,Batch=Batch,WidgetSize=WidgetSize)
#Supplier<-c(rep(c(rep("AcmeInd",2),rep("WHSupply",2),rep("GenWidget",2)),8))
#Batch<-c(rep(c("1","3"),12),rep(c("2","4"),12))
#WidgetSize<-c(94,91,94,92,96,93,92,93,91,93,94,91,93,94,90,91,92,94,91,93,92,90,93,93,
#              91,94,93,93,90,95,90,97,97,96,92,94,89,93,95,95,94,93,88,93,94,94,91,92)
#widgets=data.frame(Supplier=Supplier, Batch=Batch, WidgetSize=WidgetSize)

#c-------------------------------------------------------------
options(contrasts = c("contr.sum", "contr.poly"))

model1=aov(WidgetSize~Supplier+Batch:Supplier,data=df)
par(mfrow=c(2,2))
plot(model1)

widgets$LogWidgetSize<-log(WidgetSize)
widgets$SqrtWidgetSize<-sqrt(WidgetSize)
widgets$InvWidgetSize<-1/WidgetSize

model2=aov(LogWidgetSize~Supplier+Batch:Supplier,data=df)
par(mfrow=c(2,2))
plot(model2)

model3=aov(SqrtWidgetSize~Supplier+Batch:Supplier,data=df)
par(mfrow=c(2,2))
plot(model3)

model4=aov(InvWidgetSize~Supplier+Batch:Supplier,data=df)
par(mfrow=c(2,2))
plot(model4)

#d-------------------------------------------------------------


#e-------------------------------------------------------------
anova(model1)

#install.packages("lsmeans")
#install.packages("multcompView")

library(lsmeans)
library(multcompView)

lsmBS=lsmeans(model1, ~ Batch:Supplier)
cld(lsmBS)
