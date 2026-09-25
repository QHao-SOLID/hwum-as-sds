#
#     R-code to generate data for F79MA Assignment 1.  This code MUST be
#     placed at the start of your own R-script.  You must edit
#     the argument to the set.seed( ) function to fit your own
#     registration number
#
#     Please also ensure you have created all required variables 
#     (Quantity1, Quantity2, Quantity3, Quantity4, Quantity5, 
#     Quantity6, Quantity7, Quantity8, Quantity9, Quantity10) 
#     and include code for all of your plots
#
RNGkind(sample.kind = "Rejection")
set.seed(6014) ## Change this number
#
My_theta <- runif(1)
if (My_theta < 0.01){ # ensure My_theta at least 0.01
  My_theta <- 0.01
}
if (My_theta > 0.99){ # ensure My_theta at most 0.99
  My_theta <- 0.99
}
#
#     Generate pseudorandom data for Parts 1 and 2
#
n <- 100  # size of each sample (sample size)
m <- 1000 # number of samples (simulations)
# generate m*n U(0,1) sample
u <- runif(n*m,-0.5, 0.5) 
# generate samples for my distribution using inverse transform method
x<- My_theta - sign(u) * log(1 - 2*abs(u))
samples <- matrix(x, nrow = m, ncol = n)
#
#####################################################################
# Please insert your R code after this line
#####################################################################

#####################################################################
# Task 4
#####################################################################

# Store first sample
first_sample = samples[1,]

# Calculate Maximum Likelihood Estimate (MLE) = median
Quantity1 = median(first_sample)

#####################################################################
# Task 5
#####################################################################

# Count number of samples
first_sample_count = length(first_sample)

# Calculate log-likelihood for the maximum likelihood estimate Quantity1
Quantity2 = -first_sample_count * log(2) - sum(abs(first_sample - Quantity1))

# Creating possible range of values for theta
theta_grid = seq(min(first_sample) - 1, max(first_sample) + 1, length.out = 2001)

# Define the log-likelihood function
log_likelihood <- function(first_sample_count, first_sample, theta) {
  -first_sample_count * log(2) - sum(abs(first_sample - theta))
}

# Generating relative values of log-likelihood function using theta_grid
loglik_values = numeric(length(theta_grid))

for(i in 1:length(theta_grid)) {
  loglik_values[i] = log_likelihood(first_sample_count, first_sample, theta_grid[i])
}

# Plot the log-likelihood function
plot(theta_grid, loglik_values, type = "l", col = "blue", lwd = 2,
     xlab = "theta", ylab = "l(theta;x)", main = "Plot of Log-likelihood function")

# Plotting Quantity2 and Quantity1 to show that Quantity1 maximises the log-likelihood
abline(h = Quantity2, v = Quantity1, lty = 2, col = "red", lwd = 2)

#labeling of Quantity1 and Quantity2
text(x = 1.25, y = -800, labels = "Quantity1", col = "red", cex = 0.8)
text(x = 5, y = -225, labels = "Quantity2", col = "red", cex = 0.8)

#####################################################################
# Task 6
#####################################################################

# Define alpha
confidence_level = 0.9
alpha = 1 - confidence_level

# Generate relevant z-score
z_score = qnorm(confidence_level + alpha/2)
I = first_sample_count

# Lower Limit
Quantity3 = Quantity1 - z_score * sqrt(1/I)

# Upper Limit
Quantity4 = Quantity1 + z_score * sqrt(1/I)

#####################################################################
# Task 7
#####################################################################

# Setting threshold
thresh = qchisq(0.9,1)

# Estimating range of possible theta values
theta = c(1:40000)*0.0001 - 1

# Calculating deviance statistics for range of possible theta values
dev = numeric(length(theta))
for(i in 1:length(theta)) {
  dev[i] = 2*(Quantity2-log_likelihood(first_sample_count, first_sample, theta[i]))
}

# Range of values that satistfy asymptotic distribution of deviance statistic
ci = theta[dev<thresh]

# Lower Limit
Quantity5 = ci[1]

# Upper Limit
Quantity6 = ci[length(ci)]

#####################################################################
# Task 8
#####################################################################

# Calculating theta_hat for all 1000 simulations
theta_hat_vector = numeric(m)
for (i in 1:m){
  
  theta_hat = median(samples[i,])
  theta_hat_vector[i] = theta_hat
  
}

# Estimating Bias
Quantity7 = mean(theta_hat_vector - My_theta)

# Estimating Variance
Quantity8 = var(theta_hat_vector)

# Estimating Mean Squared Error (MSE)
Quantity9 = mean((theta_hat_vector - My_theta)^2)

#####################################################################
# Task 9
#####################################################################

# Calculating the CRLB for a smooth unbiased estimator for a sample of size n
Quantity10 = 1 / first_sample_count

#####################################################################
# Task 10
#####################################################################

# Creating function to calculate theta_hat for 1000 simulations
calculate_theta_hat_vec_new <- function(m, samples_new){
  
  theta_hat_vector_new = numeric(m)
  for (i in 1:m){
    
    theta_hat_new = median(samples_new[i,])
    theta_hat_vector_new[i] = theta_hat_new
    
  }
  
  return(theta_hat_vector_new)
}

# Initialising variables
Bias = numeric(1000)
Variance = numeric(1000)
MSE = numeric(1000)

# Start loop
for (i in 100:1100){
  n_new <- i  # size of each sample (sample size)
  m_new <- 1000 # number of samples (simulations)
  # generate m*n U(0,1) sample
  u_new <- runif(n_new*m_new,-0.5, 0.5) 
  # generate samples for my distribution using inverse transform method
  x_new<- My_theta - sign(u_new) * log(1 - 2*abs(u_new))
  samples_new <- matrix(x_new, nrow = m_new, ncol = n_new)
  
  # Calculate new theta hat
  theta_hat_vector_new = calculate_theta_hat_vec_new(m_new, samples_new)
  
  # Estimating Bias
  Bias[i-99] = mean(theta_hat_vector_new - My_theta)
  
  # Estimating Variance
  Variance[i-99] = var(theta_hat_vector_new)
  
  # Estimating Mean Squared Error (MSE)
  MSE[i-99] = mean((theta_hat_vector_new - My_theta)^2)
  
}

# Creating x-axis values (n sample sizes)
sample_sizes = 100:1100

# Plotting the Graphs for comparison
# Bias graph
plot(sample_sizes, Bias, type = "l", col = "blue",
     xlab = "sample size", ylab = "Bias", main = "Plot of Bias")

# Variance graph
plot(sample_sizes, Variance, type = "l", col = "red",
     xlab = "sample size", ylab = "Variance", main = "Plot of Variance")

# MSE graph
plot(sample_sizes, MSE, type = "l", col = "purple",
     xlab = "sample size", ylab = "MSE", main = "Plot of MSE")
