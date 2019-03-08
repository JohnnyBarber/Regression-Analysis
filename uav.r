# 
# # 2. Drone experiment
# 
# Sriram Mahadevan (2009) conducted three experiments at Wright State University to evaluate 
# the performance of a semi-automated computer display system designed to support a 
# human operators ability to monitor and control the complex dynamic operation of 
# multiple unmanned aerial vehicles (UAVs) when the UAVs are involved in multiple 
# combat-related tasks. One of his experiments was conducted to examine the effects 
# of different visual tools on the task performance efficiency and situation awareness 
# of a single operator at a computer display to monitor and control dual-task scenarios 
# involving UAVs.
# 
# The experiment involved 16 subjects (factor W ) and a = 2 cue conditions (factor A), 
# with eight of the 16 participants utilizing a baseline user interface with basic visual 
# tools ( A = 1), and with the other eight participants utilizing an advanced user 
# interface involving more advanced visual tools (A = 2). Each subject ran through 
# eight trials, two at each level of task complexity.  Task complexity corresponds 
# to the number of UAVs in the scenario, the levels being: simple-simple if both the 
# primary and secondary tasks each involves two UAVs; simple-complex if the primary 
# and secondary tasks involve two and four UAVs, respectively; complex-simple if the 
# primary and secondary tasks involve four and two UAVs, respectively; and complex-complex 
# if both the primary and secondary tasks each involves four UAVs. These levels were 
# coded 1, 2, 3, 4, respectively.
# 



uav=read.table("uav.txt",header=TRUE)
uav



library(lme4)
library(car)
library(lmerTest)
## make everything a factor
TaskComplexity=as.factor(uav$TaskComplexity)
Subject=as.factor(uav$Subject)
CueCondition=as.factor(uav$CueCondition)
Time=uav$Time

## fit model (note the log transform below)
model=lmer(log(Time)~TaskComplexity + CueCondition+(1|Subject:CueCondition)
           + TaskComplexity:CueCondition)

plot(model)
resid=resid(model)
qqnorm(resid)
qqline(resid)

Anova(model,type="III")
rand(model)


difflsmeans(model,"TaskComplexity:CueCondition")

lsmeans(model)

difflsmeans(model,"CueCondition:TaskComplexity")


