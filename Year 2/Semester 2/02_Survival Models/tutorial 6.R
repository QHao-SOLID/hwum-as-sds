# Tutorial 6 Graduation II
source("Test_GoF.r")

# Q1
# Part a
data = scan()
1 60 52250 314 -5.11 314.8 -0.044
2 61 47400 312 -5.02 321.1 -0.506
3 62 44860 352 -4.85 341.7 0.559
4 63 42920 334 -4.86 367.5 -1.749
5 64 38920 405 -4.57 374.7 1.564
6 65 24950 257 -4.58 270.1 -0.797
7 66 16900 206 -4.41 205.7 0.020
8 67 14990 212 -4.26 205.1 0.478
9 68 13550 235 -4.05 208.5 1.835
10 69 12160 223 -4.00 210.4 0.870
11 70 10950 200 -4.00 213.0 -0.891
12 71 9770 217 -3.81 213.7 0.226
13 72 8770 195 -3.81 215.7 -1.408

# Set up a matrix containing the data
database = matrix(data, ncol=7, byrow=T)

# Set up a data frame containing the data
df = data.frame(database, row.names=1)
names(df) <- c("x", "Ex", "dx", "log_mu", "d.x", "z_x")
df
attach(df)

plot(x,log_mu, xlab = "Age", main = "Plot of log_mu Against Age")

# Part b
model = glm(dx ~ offset(log(Ex)) + x, family = poisson)
summary(model)

# log(mu) = log(death) - log(exposure)
model_fit = predict(model) - log(Ex)
lines(x,model_fit, col = "red", lwd = 2)

# Part c
model$fitted.values
resid(model, type = "pear")
# Yay all correct so far. 
# Comment: 

# Part d
Chi.Square(z_x, 2) # Do not reject H_0, model fit is adequate. 
Standard.Area(z_x, 4) # Do not reject H_0, residuals are normally distributed mean 0 sd 1
Sign(z_x) # fail to reject H_0, There's no positive or negative bias in residuals
Change.Sign(z_x) # fail to reject H_0, residuals are normally distributed mean 0 sd 1
Runs.test(z_x) # fail to reject H_0, the pos and neg residuals are in random order
Serial(z_x) # fail to reject H_0, the residuals are not serially correlated
# Conclusion: The graduation given above is an adequate fit to the data, 
# since we do not reject the H_0 of any of our tests. 

# Part e
# w, to account for the variance for each of the observations, 
# and to assign a weight or importance to each of our observations
# Note: w = dx


# Q2
# Part a
