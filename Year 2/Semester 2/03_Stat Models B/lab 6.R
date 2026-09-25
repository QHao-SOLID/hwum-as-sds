# Notes: 
# + without interaction only main effects
# * with interaction and main effects
# : only with interaction effects without main effects
# if you combine + and :, it will be same as using *
# Q1
illustration = data.frame(
  y = c(3.5, 3.2, 3, 2.9, 4, 2.5, 2.3),
  x1 = c(3.1, 3.4, 3, 3.2, 3.9, 2.8, 2.2), 
  x2 = c(30, 25, 20, 30, 40, 25, 30)
)

illustration

pairs(illustration)

attach(illustration)

model1 = lm(y~x1+x2)
summary(model1)

par(mfrow=c(2,2))
plot(residuals(model1)~x1)
plot(residuals(model1)~x2)
plot(residuals(model1)~fitted.values(model1))

par(mfrow=c(1,1))

summary.aov(model1)
plot(model1, which=1:4)

#Q2
data(trees)

attach(trees)
pairs(trees)

par(mfrow=c(2,2))

model1 = lm(Volume~Height)
plot(model1, which = 1:4)

model2=lm(Volume~I(Height*Girth^2)-1)
plot(model2, which = 1:4)

summary(model1)
summary(model2)
summary.aov(model1)
summary.aov(model2)

model3=lm(Volume~I(Height*Girth^2))
plot(model3, which = 1:4)
summary(model3)
summary.aov(model3)

# Q3
library(MASS)
data(whiteside)

attach(whiteside)
pairs(whiteside)

# Differentiating plots
plot(Gas~Temp, type = "n")
points(Gas[Insul=='Before']~Temp[Insul=='Before'])
points(Gas[Insul=='After']~Temp[Insul=='After'], pch=3)
legend(x="topright",legend=c("Before", "After"),pch = c(1,3), cex = 0.8)

gas.before = lm(Gas ~ Temp, subset = Insul=="Before")
gas.after = lm(Gas ~ Temp, subset = Insul=="After")
summary(gas.before)
summary(gas.after)

gas.sep.intercept = lm(Gas ~ Insul + Temp )
summary(gas.sep.intercept)

gas.sep.slope = lm(Gas ~ Temp:Insul)
summary(gas.sep.slope)

gas.sep.interc.slope = lm(Gas ~ Insul * Temp)
summary(gas.sep.interc.slope)

lines(fitted(gas.sep.interc.slope)[Insul=="Before"]~Temp[Insul=="Before"])
lines(fitted(gas.sep.interc.slope)[Insul=="After"]~Temp[Insul=="After"],col="red")

# Q4
