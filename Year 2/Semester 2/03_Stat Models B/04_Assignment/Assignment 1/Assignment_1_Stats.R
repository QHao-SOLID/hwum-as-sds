set.seed(6014)
#############################################################
# Question 1
#############################################################

# Part a
# load data
file = scan("bug_time.txt")
file

# Histogram
hist(file, main = "Histogram of Bug-fix 
Resolution Times", xlab = "Time (in minutes)")

# Boxplot
boxplot(file, horizontal = T, main = "Boxplot of Bug-fix 
Resolution Times", xlab = "Time (in minutes)")

# numerical summaries
summary(file)

n = length(file)
n

xbar_bug = mean(file)
xbar_bug

sd_bug = sd(file)
sd_bug

# Part b
# log-transform data
transform_data = log(file)
transform_data

# Compute MLE values
mu_hat = mean(transform_data)
mu_hat

sigma_hat = 1/n * sum((transform_data - mu_hat)^2)
sigma_hat

# Compare with sample mean and variance
# Mean
trans_xbar = mean(transform_data)
trans_xbar

# Standard Deviation
trans_sd = sd(transform_data)
trans_sd

# Variance
trans_var = trans_sd^2
trans_var

# Part c

# Q-Q plot
qqnorm(transform_data, main = "Normal Q-Q Plot of log(Bug-fix 
Resolution Times)")
qqline(transform_data, col = "red")

# Histogram of transformed data
hist(transform_data, main = "Histogram of log(Bug-fix 
Resolution Times)", xlab = "Time (in minutes)")

# Boxplot of transformed data
boxplot(transform_data, horizontal = T, main = "Boxplot of log(Bug-fix 
Resolution Times)", xlab = "Time (in minutes)")

# Part d
# Breaking transformed data into 10 cells with equal probability
breaks = qnorm(seq(0,1,by=1/10), mean=mu_hat, sd=sqrt(sigma_hat))
breaks

actual_breaks = exp(breaks)
actual_breaks

# Expectation (e_i > 5)
expectation = n*(1/10)
expectation

# Computing observed frequencies f_i
observed = hist(transform_data, breaks = breaks, plot=F, right=F)$counts
observed

# Calculate test-statistic
Chi2 = sum((observed-expectation)^2/expectation)
Chi2

# Calculate p-value (2 estimated parameters, so 10 - 2- 1 degrees of freedom)
p_value = 1 - pchisq(Chi2,7)
p_value

# Reject H_0, bug-fix resolution times does not follow lognormal distribution. 

#############################################################
# Question 2
#############################################################

# Part a
# load data
file2 = scan("app_session_duration.txt")
file2

# Histogram
hist(file2, main = "Histogram of Duration of User Sessions", 
     xlab = "Time (in minutes)")

# Boxplot
boxplot(file2, horizontal = T, 
        main = "Boxplot of Histogram of Duration of User Sessions",
        xlab = "Time (in minutes)")

# numerical summaries
summary(file2)

n2 = length(file2)
n2

xbar_app = mean(file2)
xbar_app

sd_app = sd(file2)
sd_app

# Part b
# Sampling
A = 1000
median.a = numeric(A)
for(i in 1:A){ median.a[i] = median(sample(file2, size=32, replace=TRUE)) }

# Summary statistics
summary(median.a)

# Histogram
hist(median.a, main="Histogram of bootstrap medians", 
     xlab= "Bootstrap medians", col="cyan")

# Boxplot
boxplot(median.a, main="Boxplot of bootstrap medians", 
        xlab= "Bootstrap medians", col="cyan", horizontal=TRUE)

# Q-Q plot
qqnorm(median.a, main="Normal Q-Q Plot of bootstrap medians")
qqline(median.a, col = "red")

# 95% CI
bs_CI = quantile(median.a,c(0.025,0.975))
bs_CI

# Part c
# log-transform data
transform_data2 = log(file2)
transform_data2
summary(transform_data2)

# Histogram of transformed data
hist(transform_data2, main="Histogram of log(Duration of User Sessions)", 
     xlab= "Time (in minutes)", col="cyan")

# Boxplot of transformed data
boxplot(transform_data2, main="Histogram of log(Duration of User Sessions)", 
        xlab= "Time (in minutes)", col="cyan", horizontal=TRUE)

# Estimating parameters using MLE
mu_hat2 = mean(transform_data2)
mu_hat2

sigma_hat2 =  1/n2 * sum((transform_data2 - mu_hat2)^2)
sigma_hat2

# sample median
m_hat = median(transform_data2)
m_hat

fm_hat = 1/(sqrt(2*pi*sigma_hat2))
fm_hat

# Standard error
sem_hat = 1/(2*fm_hat*sqrt(n2))
sem_hat

# 95% CI for log(population median)
log_CI = c(m_hat+qnorm(0.025)*sem_hat,m_hat+qnorm(0.975)*sem_hat)
log_CI

# 95% CI for population median
actual_CI = exp(log_CI)
actual_CI

# Part d
# Sampling
I = 1000
IQR = numeric(I)
for(i in 1:I){ 
  
  x = sample(file2, size=32, replace=TRUE)
  IQR[i] = quantile(x,0.75) - quantile(x,0.25)
  
}

# Summary statistics
summary(IQR)

# Histogram
hist(IQR, main="Histogram of bootstrap IQR", 
     xlab= "Bootstrap IQRs", col="cyan")

# Boxplot
boxplot(IQR, main="Boxplot of bootstrap IQR", 
        xlab= "Bootstrap IQRs", col="cyan", horizontal=TRUE)

# Q-Q plot
qqnorm(IQR, main="Normal Q-Q Plot of bootstrap IQRs")
qqline(IQR, col = "red")

# 95% CI
IQR_CI = quantile(IQR,c(0.025,0.975))
IQR_CI

# sd
sd_IQR = sd(IQR)
sd_IQR

# variance
var_IQR = sd_IQR^2
var_IQR

# Part e
PB = 10000

# log-transform H_0
median.hyp = log(15)
boot.median.H0 = numeric(PB)

# Generating samples
for (i in 1: PB){
  para_boot = rnorm(32, mean = median.hyp, sd = sem_hat) # Notice this is under H0
  boot.median.H0[i] = median(para_boot)
}

# Histogram
hist(boot.median.H0, col="cyan", 
     main="Histogram of bootstrap medians & sample median", 
     xlab = "Bootstrap medians under H0")
abline(v = m_hat, col="red", lwd=2.0)

# p-value
para_boot.pval = (1 + length(boot.median.H0[boot.median.H0 <= m_hat])) / (PB + 1) 
para_boot.pval
