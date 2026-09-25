# Lab 4
############################################################
# Question 1
############################################################
results = scan()
23 22 20 23 19 19 16 17 25 20 21 22 20

results

# summary
summary(results)
sd(results)

# histogram
hist(results)

# boxplot
boxplot(results, horizontal = T)

# sampling
results1 = sample(results, size=13, replace=TRUE)

# summary
summary(results1)
sd(results1)

# histogram
hist(results1)

# boxplot
boxplot(results1, horizontal = T)

# bootstrap using 1000 samples
res = numeric(10000)
for(i in 1:10000) res[i] = mean(sample(results, size=13, replace=TRUE))
res

# histogram
hist(res)

# 95% conf int. 
quantile(res,c(0.025,0.975))

t.test(results)

############################################################
# Question 2
############################################################
# Part a
lung = scan()
0 0 0 0 1 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 1

lung

# summary
summary(lung)
sd(lung)

# histogram
hist(lung)

# boxplot
boxplot(lung, horizontal = T)

# sampling
lung1 = sample(lung, size=13, replace=TRUE)/52

# summary
summary(lung1)
sd(lung1)

# histogram
hist(lung1)

# boxplot
boxplot(lung1, horizontal = T)

# bootstrap using 1000 samples
res = numeric(10000)
for(i in 1:10000) res[i] = (sum(sample(lung, size=52, replace=TRUE)))/52
res

# histogram
hist(res)

# 95% conf int. 
quantile(res,c(0.025,0.975))

# Part b
n = length(lung)

# 95% CI norm approx to binom
phat = sum(lung)/n

conf_int = c(phat + qnorm(0.025)*sqrt(phat*(1-phat)/n), phat + qnorm(0.975)*sqrt(phat*(1-phat)/n))
conf_int

############################################################
# Question 3
############################################################
# Part a
fail = scan()
413 14 58 37 100 65 9 169 447 184 36 201
118 34 31 18 18 67 57 62 7 22 34

fail

# summary
summary(results)
sd(results)

# histogram
hist(results)

# boxplot
boxplot(results, horizontal = T)

