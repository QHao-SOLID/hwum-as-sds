# Read in a small example data set
x=c(3.1,3.4,3.0,3.2,3.9,2.8,2.2)
y=c(3.5,3.2,3.0,2.9,4.0,2.5,2.3)
x
y

# Two ways to plot the data
plot(x,y)
plot(y~x)
plot(y~x,pch=16) # Use ?points to find out about pch values
# Also try using ?par to find out about graphical parameters

# Fit the linear regression model
example1 = lm(y~x)
example1

summary(example1)

# Examine fitted values
yhat = fitted.values(example1)
yhat

# Compute fitted values from their definition
example1$coefficients[1]
ahat = example1$coefficients[[1]] # -0.004506066

example1$coefficients[2]
bhat = example1$coefficients[[2]] # 0.992201

yhat = ahat + bhat*x
yhat

# Now revert to fitted values computed directly by R
yhat = fitted.values(example1)
plot(yhat ~ x)

# Examine residuals
epsilon = residuals(example1)
epsilon

# Compute residuals from their definition
epsilon = y - yhat
epsilon

# Examine residual plots
plot(epsilon ~ x)
abline(h=0,lty='dashed')

plot(epsilon ~ yhat)
abline(h=0,lty='dashed')

# Plot the data together with the fitted line
plot(y~x)
abline(example1)

# Alternative way to plot data with fitted line
plot(y~x)
abline(ahat,bhat)

# CI for mean response, PI for individual response
predict(example1, data.frame(x=3.1), se.fit=T, interval="confidence")
predict(example1, data.frame(x=3.1), interval="prediction")
?predict.lm

# Analysis of Variance (ANOVA)
summary.aov(example1)


##############################################################

# Read in Anscombe's artificial data sets
data(anscombe)
attach(anscombe)

# Take a look at the data sets
anscombe

# Fit linear regression models
model1=lm(y1~x1)
model2=lm(y2~x2)
model3=lm(y3~x3)
model4=lm(y4~x4)

# Fitted model summaries
summary(model1)
summary(model2)
summary(model3)
summary(model4)

# Examine plots of the data and of the residuals

par(mfrow=c(1,2))
plot(y1~x1)
abline(model1)
plot(model1$residuals ~ x1)
abline(h=0,lty="dashed")

par(mfrow=c(1,2))
plot(y2~x2)
abline(model2)
plot(model2$residuals ~ x2)
abline(h=0,lty="dashed")

par(mfrow=c(1,2))
plot(y3~x3)
abline(model3)
plot(model3$residuals ~ x3)
abline(h=0,lty="dashed")

par(mfrow=c(1,2))
plot(y4~x4)
abline(model4)
plot(model4$residuals ~ x4)
abline(h=0,lty="dashed")

# Examine residuals and standardised residuals for model 1
model1$residuals
residuals(model1)
hatvalues(model1)
residuals(model1)/(sigma(model1)*sqrt(1-hatvalues(model1)))
rstandard(model1)

# Examine Cook's distance values for model 3
cooks.distance(model3)
par(mfrow=c(1,2))
plot(cooks.distance(model3))
plot(model3, which=4)

# Investigate the command plot(model,which=)
par(mfrow=c(2,2))
plot(model3, which=1:4)
?plot.lm
par(mfrow=c(1,1))
plot(model3)

# Investigate the effect of omitting observation 3
x5=x3[-3]
y5=y3[-3]
model5=lm(y5~x5)
par(mfrow=c(1,1))
plot(y5 ~ x5)
summary(model5)
model3
model5

##############################################################

# Read in the mammals data set
library(MASS)
data(mammals)
attach(mammals)

# Take a look at the data
mammals

# Plot the data and identify interesting points
plot(brain~body, xlab="Body weight (kg)", ylab="Brain weight (g)")
identify(brain~body, labels=row.names(mammals))

# Plot data on log-log scale and identify interesting points
plot(log(brain) ~ log(body), xlab="log(body weight)", ylab="log(brain weight)") 
identify(log(brain) ~ log(body), labels=row.names(mammals))

# Fit linear regression model on log-log scale
mammals.model.1 = lm(log(brain) ~ log(body))
summary(mammals.model.1)
par(mfrow=c(2,2))
plot(mammals.model.1, which=1:4, labels.id=row.names(mammals), add.smooth=F)