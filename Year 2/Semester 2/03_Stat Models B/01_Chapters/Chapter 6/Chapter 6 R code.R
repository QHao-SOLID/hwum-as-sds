# Set up the design matrix X
X = matrix(c(1,1,1,1,1,1,1,3.1,3.4,3.0,3.2,3.9,2.8,2.2,30,25,20,30,40,25,30),7,3)

# Set up a single column of response values Y
Y = matrix(c(3.5, 3.2, 3.0, 2.9, 4.0, 2.5, 2.3),7,1)

# Set up vectors x1, x2, y containing the variable values
x1 = X[,2]; x2 = X[,3]
y = Y[,1]

# Plot response Y against each explanatory variable x1, x2
par(mfrow=c(2,1))
plot(y ~ x1, pch = 19); plot(y ~ x2, pch = 19)

XT = t(X) # Transpose of X

XTX = XT %*% X # Matrix multiplication. 
# Note that XT * X would give the wrong answer
# Need to use XT %*% X for matrix multiplication

XTXinverse = solve(XTX) # Compute matrix inverse using "solve"

XTY = XT %*% Y # Matrix multiplication

betahat = XTXinverse %*% XTY # Matrix multiplication
betahat # Least Squares parameter estimates

# Get R to do all the above calculations using "lm" command
# You should check that parameter estimates are the same
mod1 = lm(y ~ x1 + x2); summary(mod1);

H = X %*% XTXinverse %*% XT # Compute the hat matrix
H

Yhat = H %*% Y # Compute fitted values Yhat
Yhat

fitted.values(mod1) # Alternatively, use "fitted.values" command

Y - Yhat # Compute residuals
residuals(mod1) # Check that "residuals" command gives same answer

# Compute sigma hat using matrix operations
YT = t(Y)
sigma.sq.hat = ( YT %*% Y - t(betahat) %*% XT %*% Y ) / (7-2-1)
sigma.hat = sqrt(sigma.sq.hat)
sigma.hat # Compare result with lm summary output

confint(mod1, level=0.95) # CIs for regression parameters

aov(mod1) # Analysis of Variance, brief output

mod10 = lm(y ~ x1); summary(mod10);
aov(mod10)

mod11 = lm(y ~ x2); summary(mod11);
aov(mod11)

mod12 = lm(y ~ x2 + x1); summary(mod12);
aov(mod12)

##############################################################

# Read in data set
data2 <- scan()
1 	3.4    	2.4 	1	
2  	4.6    	2.8 	1	
3  	3.8    	3.7 	1
4  	5.0    	4.4  	1	
5   	4.4    	5.1 	1  
6  	5.7   	5.2 	1
7  	6.4    	6.0 	1
8  	6.6    	7.9 	1
9 	8.9    	8.4 	1
10    	6.7    	8.9 	1
11    	7.9    	9.6 	1
12 	8.7  	10.4 	1
13   	9.1     12.0 	1
14  	10.1	 12.9 	1	
15  	7.1  	 5.1 	2
16  	7.2 	 6.3 	2
17  	8.6  	 7.2 	2
18  	8.3     	8.1 	2
19   	9.7     	8.8 	2
20   	9.2     	9.1 	2
21 	10.2 	  9.6 	2
22  	9.8 	  10.0 	2

# Set up a matrix containing the data
data2.mat = matrix(data2, ncol=4, byrow=T)

# Set up a data frame containing the data
data2.df = data.frame(data2.mat, row.names=1)
names(data2.df) <- c("y", "x", "type")
data2.df
attach(data2.df)

# Plot y against x, distinguising between type 1 and type 2
plot(y~x, type='n')
points(y[type=='1']~x[type=='1'])
points(y[type=='2']~x[type=='2'], pch=3)

# Fit linear model, initially ignoring the 'type' variable
mod3 = lm(y ~ x); summary(mod3);
summary.aov(mod3)

plot(y~x)
lines(fitted(mod3)~x)

plot(mod3$resid ~ mod3$fitted)

# Set the 'type' variable to be treated as a factor
type <-as.factor(type)

# Fit linear model with two explanatory variables, no interaction term
mod4=lm(y~x+type); summary(mod4);
summary.aov(mod4)

plot(y~x, type='n')
points(y[type=='1']~x[type=='1'])
points(y[type=='2']~x[type=='2'], pch=3)
lines(fitted(mod4)[type==1]~x[type==1])
lines(fitted(mod4)[type==2]~x[type==2])

plot(x, mod4$res, pch=8)
abline(0,0,lty="dashed")

plot(mod4$fit, mod4$res, pch=8)
abline(0,0,lty="dashed")

# Fit linear model with two explanatory variables and an interaction
mod5 = lm(y ~ x * type); summary(mod5);
summary.aov(mod5)

##############################################################

# Read in Scottish hill races data set
library(MASS)
data(hills)
attach(hills)

pairs(hills)

plot(time ~ dist)
identify(time ~ dist,labels=row.names(hills))

plot(time ~ climb)
identify(time ~ climb,labels=row.names(hills))

hills.model.1 <- lm(time ~ -1 + dist + climb, data=hills[-18,])
summary(hills.model.1)

plot(hills.model.1, which=1:4)

hills.model.2 <- lm(time ~ -1 + dist + I(dist^2) + climb + I(climb^2),
                    data = hills[-18,])
summary(hills.model.2)

plot(hills.model.2, which=1:4)

plot(residuals(hills.model.2) ~ residuals(hills.model.1))

hills.model.3 <- lm(time ~ -1 + dist + I(dist^2) + climb + I(climb^2),
                    data = hills[-18,], weights = 1/dist^2)

summary(hills.model.3)

plot(resid(hills.model.3) ~ resid(hills.model.2))