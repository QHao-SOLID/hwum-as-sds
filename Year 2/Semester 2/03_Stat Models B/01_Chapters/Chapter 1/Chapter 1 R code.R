#################
#  Example 1.1  #
#################

# Read in data set
times=c(0.341,0.368,0.358,0.335,0.347,0.373,0.365,0.332,0.330,0.362,0.364,
	0.352,0.338,0.327,0.350,0.358,0.385,0.370,0.350,0.341,0.349,0.354,0.330,
	0.352,0.377,0.355,0.324,0.375,0.338,0.335,0.382	,0.355,0.338,0.373,0.352,
	0.385,0.349,0.359	,0.378,0.346)

# Compute summary statistics
n = length(times); n
xbar = mean(times); xbar
s = sd(times); s

# Compute normal-based 95% CI for the mean
xbar - qnorm(0.975)*s/sqrt(n) ; xbar + qnorm(0.975)*s/sqrt(n)

# Compute t-distribution based 95% CI for the mean
xbar - qt(0.975,df=n-1)*s/sqrt(n) ; xbar + qt(0.975,df=n-1)*s/sqrt(n)

# Can more easily compute CI using the t.test command
# Check that you can find the CI in the output
t.test(times)

# To see if the data look normally distributed, have a look at stemplot
stem(times)

# Alternatively, have a look at histogram
hist(times)

# To see more clearly whether data look normally distributed,
# have a look at a normal QQ plot
qqnorm(times)

#################
#  Example 1.2  #
#################

# Read in data
speeds=c(115,82,96,87,100,69,108,116,180,99,94,	86,104,98,103,102,62,94,126,96)

# Compute summary statistics
n = length(speeds); n
xbar = mean(speeds); xbar
s = sd(speeds); s

# Examine normal QQ plot
qqnorm(speeds)

# Compute t-distrubtion based CI for mean
xbar - qt(0.975,df=n-1)*s/sqrt(n) ; xbar + qt(0.975,df=n-1)*s/sqrt(n)

# Compute CI more easily using t.test
t.test(speeds)

# Compute 95% CI for variance
(n-1)*s^2/qchisq(0.975,df=n-1) ; (n-1)*s^2/qchisq(0.025,df=n-1)

# Take square roots to get 95% CI for standard deviation
sqrt((n-1)*s^2/qchisq(0.975,df=n-1)) ; sqrt((n-1)*s^2/qchisq(0.025,df=n-1))

# Now set up a data set with the one extreme value (180) removed
corrected=c(115,82,96,87,100,69,108,116,80,99,94,86,104,98,103,102,62,94,126,96)

# Summary statistics for new data set
n = length(corrected); n
xbar = mean(corrected); xbar
s = sd(corrected); s

# Normal QQ plot for new data set
qqnorm(corrected)

# t-distribution based CI for mean for new data set
xbar - qt(0.975,df=n-1)*s/sqrt(n) ; xbar + qt(0.975,df=n-1)*s/sqrt(n)

# CI computed using t.test 
t.test(corrected)

# CI for variance of new data set
(n-1)*s^2/qchisq(0.975,df=n-1) ; (n-1)*s^2/qchisq(0.025,df=n-1)

# CI for standard deviation of new data set
sqrt((n-1)*s^2/qchisq(0.975,df=n-1)) ; sqrt((n-1)*s^2/qchisq(0.025,df=n-1))

#################
#  Example 1.3  #
#################

# Read in data
data(eurodist)

# Compute summary statistics
mhat = median(eurodist); mhat

n = length(eurodist); n
xbar = mean(eurodist); xbar
s = sd(eurodist); s

# Check histogram to see if data look normal
hist(eurodist)

# Or take a look at normal QQ plot
qqnorm(eurodist)

# Compute standard error of the sample median
se = s*sqrt(pi/(2*n)); se

# Compute 95% CI for the population median
mhat - 1.96 * se; mhat + 1.96 *se

# For comparison, compute normal-based CI for the mean
xbar - 1.96 * s/sqrt(n); xbar + 1.96 * s/sqrt(n)