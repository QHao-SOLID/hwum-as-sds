# Read in Scottish hill races data set
library(MASS)
data(hills)
attach(hills)

# Fit quadratic model using Ordinary Least Squares (OLS)
hills.model.2 <- lm(time ~ -1 + dist + I(dist^2) + climb + I(climb^2),
                    data = hills[-18,])
summary(hills.model.2)

# Fit quadratic model using Weighted Least Squares (WLS)
hills.model.3 <- lm(time ~ -1 + dist + I(dist^2) + climb + I(climb^2),
                    data = hills[-18,], weights = 1/dist^2)
summary(hills.model.3)

# Compare residuals from WLS fit and from OLS fit
plot(resid(hills.model.3) ~ resid(hills.model.2))

##############################################################

# Read in phones data
library(MASS)
data(phones)
attach(phones)

par(mfrow=c(1,2))

# Plot data with OLS fitted line
plot(calls~year,main="Ordinary Least Squares")
phones.ls=lm(calls~year)
abline(phones.ls)

# Plot data with LTS fitted line
plot(calls~year,main="Least Trimmed Squares")
phones.lts=lqs(calls~year)
abline(phones.lts)

##############################################################

# Read in cars data
library(MASS)
detach(hills) 
# "hills" and "cars" both include a variable named "dist"
# so detach the "hills" data before attaching the "cars" data
data(cars)
attach(cars)

# Plot stopping distance against speed
par(mfrow=c(1,1))
plot(dist ~ speed)

# Plot the data with the loess fitted curve
plot(cars)
cars.lo=loess(dist ~ speed)
lines(fitted(cars.lo) ~ speed)

?loess # Take a look at details of the loess command

# Plot the data with loess fit using a different span
plot(cars)
cars.lo2=loess(dist ~ speed, span=1)
lines(fitted(cars.lo2) ~ speed)