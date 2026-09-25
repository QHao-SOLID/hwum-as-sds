###########################
### Chapter 4

library(MASS)
data(chem)
chem
n=length(chem); n
mean(chem)
sd(chem)

qqnorm(chem)

chem1=chem[-17]; chem1
qqnorm(chem1)
qqline(chem1)

qqnorm(chem1,xlim=c(-0.5,0.5),ylim=c(3,4))
qqline(chem1)
grid(10,10,lwd=2)

mean(chem1)
t.test(chem1)
t.test(chem)

###########################
### Chapter 4.2 

### Example 4.1
failures = c(413,14,58,37,100,65,9,169,447,184,36,201,118,34,31,18,18,67,57,62,7,22,34)

par(mfrow=c(2,2))
hist(failures, main='Histogram of failure times', col='orange')
boxplot(failures, horizontal=TRUE, main='Boxplot of failure times', col='orange')
stripchart(failures, main='Dotplot of failure times', pch=19, col='orange')

############################################
### Non-parametric bootstrap estimation ####
############################################

B = 1000
median.b = numeric(B)
for(i in 1:B){ median.b[i] = median(sample(failures, size=23, replace=TRUE)) }
summary(median.b)

par(mfrow=c(2,2))
hist(median.b, main="Histogram of bootstrap medians", col="cyan")
boxplot(median.b, main="Boxplot of bootstrap medians", col="cyan", horizontal=TRUE)

bootstrap_CI = quantile(median.b,c(0.025,0.975))
bootstrap_CI

par(mfrow=c(1,1))
hist(median.b, main="Histogram of bootstrap medians and 95% CI", col="cyan")
abline(v=bootstrap_CI, lwd=3, col='red')

### C.I for median using asymptoptic theory ######
mhat = median(failures)
n = length(failures)
lambdahat = log(2)/mhat

fmhat = lambdahat * exp(-lambdahat * mhat)
fmhat = lambdahat / 2

asymp_CI = c(mhat-1.96/(2*fmhat*sqrt(n)),mhat+1.96/(2*fmhat*sqrt(n)))
asymp_CI

##################################################
### Parametric bootstrap estimation for median ###
##################################################

B = 1000
mhat = median(failures)
lambdahat= log(2)/mhat
median.pb = numeric(B)
for(i in 1:B){ median.pb[i] = median (rexp(23, rate=lambdahat)) }
summary(median.pb)

par(mfrow=c(1,2))
hist(median.pb, main="Histogram of bootstrap medians", col="red")
boxplot(median.pb, main="Boxplot of bootstrap medians", col="red", horizontal=TRUE)

bootstrap_CI = quantile(median.pb,c(0.025,0.975))
bootstrap_CI

par(mfrow=c(1,1))
hist(median.pb, main="Histogram of bootstrap medians and 95% CI", col="red")
abline(v=bootstrap_CI, lwd=3, col="blue")

###########################
#Chapter 4.3
#####################################################################
##   Perform test for mean = mean.hyp vs mean < mean.hyp          ###
##   Parametric bootstrap is well suited for hypothesis testing   ###
#####################################################################

B = 10000
mean.hyp = 140
boot.mean.H0 = numeric(B)  # Create vector for bootstrap values
for (i in 1: B){
  y.boot = rexp(23,rate = (1/mean.hyp)) # Notice this is under H0
  boot.mean.H0[i] = mean(y.boot)
}

# Obtain sampling distn under H0 and p-value

hist(boot.mean.H0, col="cyan", main="Histogram of bootstrap means and sample mean")
mu.hat = mean(failures)
abline(v = mu.hat, col="red", lwd=2.0)

boot.pval = (1 + length(boot.mean.H0[boot.mean.H0 <= mu.hat])) / (B + 1) 
boot.pval

t.test(failures,mu=140, alternative="less")

############################################################
##  Perform test for mean = mean.hyp vs mean < mean.hyp  ###
##  using non-parametric bootstrap                       ###
############################################################

# Rescale data so that HO is true
failures.new = failures - mean(failures) + mean.hyp 
mean(failures.new)

B = 10000
boot.mean.new.H0 = numeric(B)  #Create vector for bootstrap values
for (i in 1: B){
  boot.new.y = sample(failures.new, 23, replace=T) #This is under H0
  boot.mean.new.H0[i] = mean(boot.new.y)
}

hist(boot.mean.new.H0, col="cyan", main="Histogram of non-parametric bootstrap means and sample mean")
mu.hat = mean(failures)
abline(v = mu.hat, col="red", lwd=2.0)
boot.pval = (1 + length(boot.mean.new.H0[boot.mean.new.H0 <= mu.hat])) / (B + 1) 
boot.pval

