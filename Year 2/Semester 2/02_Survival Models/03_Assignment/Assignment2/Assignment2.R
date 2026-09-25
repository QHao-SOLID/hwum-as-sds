#
#     R-code to generate data for Assignment 2.  This code MUST be
#     placed at the start of your own R-script.  You must edit
#     the argument to the set.seed( ) function to fit your own
#     HW student ID number
#
#
RNGkind(sample.kind = "Rejection")
set.seed(6014)  #### Replace with the last 4 digits of your HW student ID
#
#      Read CMI data
#
source("CMI_read.r")
#
#  Age range
#
start.age = 35; end.age = 80;
AGE <- start.age:end.age
#
#  MyYear
#
MyYear <- 1950 + round(runif(1,0,55),0)
#
# Data for Gompertz GLM for MyYear 
#
DTH <- Dth[ (start.age <= Age) & (Age <= end.age), Year==MyYear]
EXP <- Exp[ (start.age <= Age) & (Age <= end.age), Year==MyYear]
mu.x <- DTH/EXP
OBS <- log(mu.x)
#
#
####################################################
# Please insert your R code after this line
####################################################
# Sourcing available test functions
source("Test_GoF.r")

# Q1
# Plotting log(mu.x) Against Ages x
plot(AGE, log(mu.x), 
     xlab = "Age", 
     ylab = "log(mortality)", 
     main = "Gompertz law: CMI data, ages 35 to 80, year 1955")

# Q2
# Creating the custom covariates
xbar = mean(AGE)
xbar

cov2 = (AGE - xbar)^2
cov3 = (AGE - xbar)^3

# Fitting the models
model1 = glm(DTH ~ offset(log(EXP)) + AGE, family = poisson)
model2 = glm(DTH ~ offset(log(EXP)) + AGE + cov2, family = poisson)
model3 = glm(DTH ~ offset(log(EXP)) + AGE + cov2 + cov3, family = poisson)

# Obtaining summary of models
summary(model1)
summary(model2)
summary(model3)

# Q3
# Adding model 1 fit to plot
lines(AGE, log(model1$fit/EXP), col = "red")
legend("bottomright", legend = "Model M1", fill = "red")

# Q4
# Adding model 2 and 3 fit to plot
lines(AGE, log(model2$fit/EXP), col = "green")
lines(AGE, log(model3$fit/EXP), col = "blue")
legend("bottomright", 
       legend = c("Model M1", "Model M2", "Model M3"), 
       fill = c("red", "green", "blue"))

# Q5
# Computing the residuals for each model
r1 = resid(model1, type = "pear")
r2 = resid(model2, type = "pear")
r3 = resid(model3, type = "pear")

# Plot Pearson residuals of the 3 models
# Model 1
plot(AGE, r1, 
     ylab = "residuals",
     xlab = "Age", 
     main = "Residuals for Model 1")
abline(0, 0, col = "red", lty = 2)

# Model 2
plot(AGE, r2, 
     ylab = "residuals",
     xlab = "Age", 
     main = "Residuals for Model 2")
abline(0, 0, col = "red", lty = 2)

# Model 3
plot(AGE, r3, 
     ylab = "residuals",
     xlab = "Age", 
     main = "Residuals for Model 3")
abline(0, 0, col = "red", lty = 2)

# Q6
# Part a
Chi.Square(r1, 2)
Chi.Square(r2, 3)
Chi.Square(r3, 4)

# Part b
Standard.Area(r1, 4)
Standard.Area(r2, 4)
Standard.Area(r3, 4)

# Part c
Sign(r1)
Sign(r2)
Sign(r3)

# Part d
Change.Sign(r1)
Change.Sign(r2)
Change.Sign(r3)

# Part e
Runs.test(r1)
Runs.test(r2)
Runs.test(r3)

# Part f
Serial(r1)
Serial(r2)
Serial(r3)

# Q7
aic1 = model1$aic
aic2 = model2$aic
aic3 = model3$aic

aic1
aic2
aic3

# Q8 
# Comments are present in the report. 


