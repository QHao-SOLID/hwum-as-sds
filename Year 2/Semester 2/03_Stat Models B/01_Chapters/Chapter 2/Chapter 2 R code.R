#####################################################
# Chapter 2-1

ppoints(20)
seq(from=0.025, to=0.975, by=0.05)

qnorm(0.025)
pnorm(-1.96)

qnorm(ppoints(20))
qexp(ppoints(20))


# Example 2.1
ozone=dget("ozone.R")
attach(ozone)
ozone

length(ozone)
length(stamford)
length(yonkers)

sort(stamford)
hist(stamford, xlab="ozone concentration", main="Stamford ozone concentrations")

summary(stamford)
quantile(stamford)
quantile(stamford, probs=c(0.1,0.7,0.9))
quantile(stamford, probs=seq(from=0, to=1, by=0.1))

#####################################################
# Chapter 2-2(i) Q-Q plots

x= rnorm(200)
qqnorm(x, main="N(0,1) QQ plot for sample from N(0,1)", pch=19, col="red")
abline(0,1,lty=2, col="blue")
legend(x="topleft",legend="Q = Qe", lty=2,col="blue")

par(mfrow=c(1,1))
x1 = rnorm(200,3,1)
qqnorm(x1, main="N(0,1) QQ plot for sample from N(3,1)", pch=19, col="red")
abline(0,1,lty=2, col="blue", lwd=1.5)
legend(x="topleft",legend="Q = Qe", lty=2,col="blue")

grid = seq(-4,10,length=100)
hist(x1, freq =F, axes=F, labels=F, xlab=NULL, ylab=NULL,xlim=c(min(x1)-1,max(x1)+1),col="red",main="N(0,1) density & N(3,1) sample")
par(new=T)
plot(grid, dnorm(grid), type="l", xlab="x1", col="blue")

## Or alternative method to plot Q-Q plot using qqplot()

a=qnorm(ppoints(200))
qqplot(a, x1, col="red", pch=19)
abline(0,1,lty=2, col="blue")

##############################
x2 = rnorm(200,0,4)
qqnorm(x2, main="N(0,1) QQ plot for sample from N(0,4)", pch=19, col="red")
abline(0,1,lty=2, col="blue")
legend(x="topleft",legend="Q = Qe", lty=2,col="blue")

grid = seq(-4,4,length=100)
hist(x2, freq =F, axes=F, labels=F, xlab=NULL, ylab=NULL,col="red",main="N(0,1) density & N(0,4) sample")
par(new=T)
plot(grid, dnorm(grid), type="l", xlab="x2", col="blue")

##############################

x3= rgamma(200,1,4)
qqnorm(x3, main="N(0,1) QQ plot for sample from Ga(1,4)", pch=19, col="red")
abline(0,1,lty=2, col="blue")
legend(x="topleft",legend="Q = Qe", lty=2,col="blue")

grid = seq(-3,3,length=200)
hist(x3, freq =F, axes=F, labels=F, xlab=NULL, ylab=NULL, xlim=c(-3,3),col="red",main="N(0,1) density & Ga(1,4) sample")
par(new=T)
plot(grid, dnorm(grid), type="l", xlab="x3", col="blue", ylim=c(0,2.0))

##############################

par(mfrow=c(1,2))
x8= rgamma(200,1,1)
qqplot(qnorm(ppoints(200), 1, 1),x8, 
       main="N(1,1) QQ plot for sample from Ga(1,1)", ylab="Sample Quantiles",
       xlab="Theoretical Quantiles", pch=19, col="red")
abline(0,1,lty=2, col="blue")
legend(x="topleft",legend="Q = Qe", lty=2,col="blue")

grid = seq(-3,6,length=200)
hist(x8, freq =F, axes=F, labels=F, xlab=NULL, ylab=NULL, xlim=c(-3,6),col="red",main="N(1,1) density & Ga(1,1) sample")
par(new=T)
plot(grid, dnorm(grid, 1, 1), type="l", xlab="x8", col="blue", ylab="Density", ylim=c(0,2.0))

###############################
# Chapter 2-2(ii) Refinement
# Example 2.1
ozone=dget("ozone.R")
attach(ozone)
qqnorm(stamford,main="Normal Q-Q Plot for Stamford Ozone")
qqline(stamford)

qqnorm(stamford^0.5,main="Normal Q-Q Plot for Square Root of Stamford Ozone")
qqline(stamford^0.5)

x1 = qnorm(0.25); x1
y1 = quantile(stamford^0.5,0.25,names=FALSE); y1

x2 = qnorm(0.75); x2
y2 = quantile(stamford^0.5,0.75,names=FALSE); y2

b = (y2-y1)/(x2-x1); b
a = y1 - b*x1; a

qqnorm(stamford^0.5,main="Normal Q-Q Plot for Square Root of Stamford Ozone")
qqline(stamford^0.5,col='red')


##############################################################
# Chapter 2-3 Checking data modelled by exponential distribution
# Example 2.2

library(MASS)
data(abbey)
abbey

qexp(ppoints(31))
sort(abbey)

plot( qexp(ppoints(31)), sort(abbey), xlab="Standard exponential quantiles",
      ylab="Sorted nickel contents", main="Exponential Q-Q Plot for Nickel Content")

qqplot( qexp(ppoints(31)), abbey)

lqs(sort(abbey) ~ qexp(ppoints(31)) - 1)

plot( sort(abbey) ~ qexp(ppoints(31)) )
abline(0,11.84)

1 - pexp(125, rate=1/11.84)

#####################
# Boxplot

boxplot(ozone, ylab="ozone concentration (ppb)", main="Stamford and Yonkers Ozone Distributions")

