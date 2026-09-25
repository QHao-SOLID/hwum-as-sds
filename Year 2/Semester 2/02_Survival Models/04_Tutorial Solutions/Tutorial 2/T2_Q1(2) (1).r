#
#    Tutorial 2, Q1: using survfit( ) to fit the Kaplan-Meier model
#
library(survival)
Times <- c(2,5,6,8,9,10,12,14,16,20)
Censor <- c(1,1,0,1,1,0,1,0,0,1)
Surv(Times, Censor)
KM.fit <- survfit(Surv(Times, Censor) ~ 1, conf.type = "plain")
names(KM.fit)
summary(KM.fit)
plot(KM.fit, main = "Time to recurrence")
