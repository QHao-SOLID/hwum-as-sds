#
#    Tutorial 2, Q2: using coxph( ) to fit the Cox model.
#
library(survival)
Times <- c(3,6,8,10,5,7,9,12)
Censor <- c(1,0,0,1,1,0,0,1)
Surv(Times, Censor)
Drug <- c(rep(1,4), rep(2,4))
cbind(Times, Censor, Drug)
#
Cox.fit <- coxph(Surv(Times, Censor) ~ Drug)
names(Cox.fit)
summary(Cox.fit)
#
#  Notice how we find all our answers from this output.  The coefficient (-0.792),
#  its standard error (1.23), the z-test is given as the Wald test or z^2 = 0.42,
#  the score test (0.44) and the LRT (0.44).
