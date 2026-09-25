x1 = rnorm(200)
qqnorm(x1, main="QQ plot of N(0,1) sample of size 200", pch=19, col="red")

?qqnorm
?par

abline(0, 1, lty=2, col="blue")

hist(x1, freq=F, xlim=c(min(x1)-1,max(x1)+1),col="red",
     main="N(0,1) density & N(0,1) sample")
grid = seq(-4,4,length=100)
lines(grid, dnorm(grid), col = "blue")

x2 = rgamma(200, 2,1)
qqnorm(x2, main="QQ plot of N(-2,1) sample of size 200", pch=19, col="red")

abline(2,sqrt(2),lty=2, col="blue")

hist(x2, freq = F, xlim=c(min(x2)-1, max(x2)+1), col="red",
     main="N(-2,1) density & N(0,1) sample")
grid = seq(-4,4,length=100)
lines(grid,dnorm(grid), col="blue")

# Q2

file = dget("ozone-1.R")
attach(file)

# summary, sd, quantile, hist, boxplot and
# stem, e.g.

summary(yonkers)
sd(yonkers)
quantile(yonkers)
hist(yonkers)
boxplot(yonkers)
stem(yonkers)

# idealised sample

ppoints(132)
qnorm(ppoints(132))
plot(qnorm(ppoints(132)),sort(yonkers))
qqnorm(yonkers)
qqplot(qnorm(ppoints(132)),yonkers)
qqline(yonkers)

# detach(filename)