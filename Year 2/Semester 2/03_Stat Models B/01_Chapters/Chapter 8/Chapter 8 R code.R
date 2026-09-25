### Example 8.1
### Poisson GLM example
### Numbers of stressful events reported over 18 months

x = 1:18 # Month of the study
n = c(15,11,14,17,5,11,10,4,8,10,7,9,11,3,6,1,1,4) # Numbers of stressful events
plot(n ~ x, xlab="Date", ylab="Number of events")

# Fit Poisson glm with default link (log)
stressmodel = glm(n~x, family=poisson)
summary(stressmodel)

fitted.values(stressmodel)
residuals(stressmodel)
summary(residuals(stressmodel)) # Residuals summary also appears in glm summary output

confint(stressmodel) # CIs for each regression parameter

# Plot observed data with fitted model
plot(n ~ x, xlab="Date", ylab="Number of events")
lines(fitted.values(stressmodel))

par(mfrow=c(2,1))
plot(residuals(stressmodel) ~ x)
plot(residuals(stressmodel) ~ fitted.values(stressmodel))

### Example 8.3
### Logistic GLM example
### Numbers of insects killed by different doses of drug

# Input the raw data
insectdata = matrix(c(64,66,59,62,55,60,63,57,60,61,7,13,15,25,27,43,52,51,57,61,3.689,3.912,4.094,4.248,4.382,4.500,4.605,4.700,4.787,4.868),10,3)

# Define variable names for raw data
groupsize = insectdata[,1]
numdead = insectdata[,2]
dose = insectdata[,3]

# Compute proportion killed, the response variable in fitting glm
propdead = numdead/groupsize

plot(propdead ~ dose)

plot(log(propdead/(1-propdead)) ~ dose) # Logistic transformation

drugmodel = glm(propdead ~ dose, weights=groupsize, family=binomial)
summary(drugmodel)

predict(drugmodel,data.frame(dose=4)) # Linear predictor eta
predict(drugmodel,data.frame(dose=4),type="response") # Estimated death probability

plot(residuals(drugmodel) ~ dose)

plot(residuals(drugmodel) ~ fitted(drugmodel))

par(mfrow=c(1,1))
pi.mean = sum(numdead)/sum(groupsize)
plot(propdead ~ dose, ylab = "pi", pch=19)
lines(dose, fitted(drugmodel), col="red", lwd=1.8)
lines(dose, rep(pi.mean,length(dose)), col="blue", lwd=2.0, lty=2)

# Examine fitted model on logistic scale
plot(log(propdead/(1-propdead)) ~ dose, ylab="logit(pi)", pch=19)
lines(dose, predict(drugmodel,type="link"), col="red", lwd=1.8)

# Compute deviance without using "glm"
#
# First compute log-likelihood terms to be summed for maximal model
l1 = numdead*log(propdead)+(groupsize-numdead)*log(1-propdead)+log(choose(groupsize,numdead))
l1 # Notice that final term in the sum evaluates as "NaN" (Not a Number)
# Final term in the sum is 0 log (0) for this data set
# this term should evaluate to zero 
# x log x -> 0 as x -> 0
l1[10]=0 # Correct the final term in the log-likelihood sum
lsat = sum(l1) # Sum the terms to obtain log-likelihood for maximal model

# Next evaluate log-likelihood for the null model
l2 = numdead*log(pi.mean)+(groupsize-numdead)*log(1-pi.mean)+log(choose(groupsize,numdead))
l2
lnull = sum(l2)

# Next evaluate log-likelihood for the logistic regression model
l3 = numdead*log(fitted(drugmodel))+(groupsize-numdead)*log(1-fitted(drugmodel))+log(choose(groupsize,numdead)) 
lbeta = sum(l3)

# Test goodness-of-fit by comparing logistic model to maximal model
D = -2*(lbeta-lsat)
D # Compare with deviance value obtained from "glm" summary
# Compute p-value for goodness-of-fit test
1 - pchisq(D, df=8)

# Assess how well the null model fits the data
Dnull = -2*(lnull-lsat)
Dnull
1 - pchisq(Dnull,df=9)

# Does logistic model fit better than null model?
1 - pchisq(Dnull-D,df=1) # zero to machine precision, logistic model much better fit than null model

# Compute deviance residuals for logistic model
di = -2*(l3-l1)
sqrt(di) # Give magnitude, but not sign (+/-), of deviance residuals
residuals(drugmodel) # Compute deviance residuals directly
residuals(drugmodel,type="pearson") # Compare values with Pearson residuals

drugmodel

# Linear logistic model is not a very good fit, so try quadratic predictor
drugmodel2 = glm(propdead~dose+I(dose^2), weights=groupsize, family=binomial)
summary(drugmodel2)

# Test whether quadratic model fits better than linear model
1 - pchisq(12.64, df=1) # Change in deviance is 12.64
# Can test more directly using "anova"
anova(drugmodel,drugmodel2,test="Chi")

# Use quadratic model to estimate a death probability
predict(drugmodel2, data.frame(dose=4), type="response")

# Residuals plots for quadratic model
par(mfrow=c(2,1))
plot(residuals(drugmodel2) ~ dose)
plot(residuals(drugmodel2) ~ fitted.values(drugmodel2))

# Compare deviance residuals with Pearson residuals
residuals(drugmodel2) # Deviance residuals by default
residuals(drugmodel2, type="pearson")

par(mfrow=c(1,1))
plot(residuals(drugmodel2) ~ residuals(drugmodel2, type="pearson"))
abline(0,1) # Deviance residuals and Pearson residuals not the same, but pretty similar values