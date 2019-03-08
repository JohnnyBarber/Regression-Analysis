install.packages("lsmeans")
library(lsmeans)

#2-------------------------------------------------------------------------------
type=c(rep("Regular",4),rep("Deordorant",4),rep("Moisturizing",4))
cube=1:12
weightloss=c(-0.30,-0.10,-0.14,0.40,2.63,2.61,2.41,3.15,1.86,2.03,2.26,1.82)

experiment=data.frame(type,weightloss)
experiment

aov.experiment=aov(weightloss~type)
aov.experiment

lsm.experiment=lsmeans(aov.experiment, "type")
lsm.experiment
contrast(lsm.experiment,list("Regular.minus.(Deoderant.plus.Moisturizing).divided.by.2"=c(-1/2,-1/2,1)))

#3------------------------------------------------------------------------------
push=c(rep("0",7),rep("1",10),rep("2",10),rep("3",5))
time=c(38.14,38.20,38.31,38.14,38.29,38.17,38.20,38.28,38.17,38.08,38.25,38.18,38.03,37.95,38.26,38.30,38.21,38.17,38.13,38.16,38.30,38.34,38.34,38.17,38.18,38.09,38.06,38.14,38.30,38.21,38.04,38.37)
experiment.light=data.frame(push,time)
experiment.light

as.numeric(experiment.light$push)

plot(time~push,main="Plot of Waiting Time Against Number of Pushes")


mean.0=mean(time[push=="0"])
mean.0
mean.1=mean(time[push=="1"])
mean.1
mean.2=mean(time[push=="2"])
mean.2
mean.3=mean(time[push=="3"])
mean.3

aov.light=aov(time~push)
aov.light
lsm.light=lsmeans(aov.light, "push")
lsm.light
contrast(lsm.light,list("1.minus.0"=c(-1,1,0,0)))

contrast(lsm.light,list("(1.plus.2.plus.3).divided.by.3.minus.0"=c(-1,1/3,1/3,1/3)))
