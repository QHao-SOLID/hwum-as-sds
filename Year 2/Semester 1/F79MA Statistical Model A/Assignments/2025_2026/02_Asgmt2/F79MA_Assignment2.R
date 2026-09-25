#
#     R-code to generate data for F79MA Assignment 2.  
#     This code MUST be placed at the start of your own R-script. 
#     You must edit the variable last_four_digits to fit 
#     your own student ID number
#
RNGkind(sample.kind = "Rejection")
last_four_digits = 6014 ## change this number

set.seed(last_four_digits)

dataset <- read.csv(file = "count_data.csv", header = TRUE)
Full_Data <- dataset$x
My_Data <- sample(Full_Data,size=1000,replace=FALSE)


#####################################################################
# Please insert your R code after this line
#####################################################################

# Quantity1

x_bar = mean(My_Data)
k = 5

Quantity1 = k / x_bar

# Quantity2

n = length(My_Data)
sum_x_i = sum(My_Data)

Quantity2 = ( n * k )/(sum_x_i + 0.5)

# Plot of densities Prior vs Posterior
p_vals <- seq(0.005, 0.4, length = 500)

prior = function(p) p^(-1) * (1-p)^(-0.5)
prior_vals <- prior(p_vals)

posterior_vals = dbeta(p_vals, n*k, sum_x_i - n * k + 0.5)

plot(p_vals, prior_vals, type = "l", col = "red", lwd = 2,
     xlab = "Parameter p", ylab = "Density", main = "Prior and Posterior Densities")
lines(p_vals, posterior_vals, col = "blue", lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = c("red", "blue"), lwd = 2)

# Posterior Predictive Distribution Prediction

posterior_predictive <- function(z, k) {
  choose(z - 1, k - 1) * exp(lbeta(n * k + k, sum_x_i - n * k + 0.5 + z - k) - lbeta(n * k, sum_x_i - n * k + 0.5))
}

Quantity3 = posterior_predictive(5, k)

# Posterior Predictive Distribution Simulation
posterior_samples = rbeta(n, n*k, sum_x_i - n * k + 0.5)
posterior_pred_samples <- rnbinom(10000, k, prob = posterior_samples) + k

hist(posterior_pred_samples, breaks = 40, col = "lightblue", main = "Posterior Predictive Simulation",
     xlab = "Simulated counts", xlim = range(0,80), ylim = range(0,1000))

hist(Full_Data, breaks = 40, col = "red", main = "Full Data Histogram",
     xlab = "Actual counts",xlim = range(0,80), ylim = range(0,1000))

