#####################################
## Example 9.1, workers' accidents ##
#####################################

x = 0:5 # Number of accidents
o = c(296,74,26,8,4,6) # Observed frequencies
N = sum(o) # Total number of observations

lambda_hat = sum(x*o)/N
lambda_hat

p_hat = exp(-lambda_hat)*lambda_hat^x/factorial(x) # Poisson model
e = N*p_hat # Expected frequencies

x[1]; p_hat[1]; e[1]

p_hat = c(p_hat,1-sum(p_hat))
p_hat

chisq.test(c(o,0),p=p_hat)
sum((o-e)^2/e)

e = N*p_hat
e

e=c(e[1],e[2],e[3],e[4]+e[5]+e[6]+e[7])
e

o=c(o[1],o[2],o[3],o[4]+o[5]+o[6])
o

Xsq = sum((o-e)^2/e)
Xsq

1-pchisq(Xsq,df=2)

chisq.test(o,p=e/N) # Note incorrect df in output

#############################
## Example 9.2, eye colour ##
#############################

o = c(89,66,60,85) # Observed frequencies
N = sum(o) # Total number of observations

p = c(0.25,0.25,0.25,0.25) # Uniform probabilities model
e = N*p # Expected frequencies

e

Xsq = sum((o-e)^2/e)
Xsq

1-pchisq(Xsq,df=3)

chisq.test(o,p=e/N)

#################################
## Example 9.3, plant genetics ##
#################################

o = c(1071,62,68,299) # Observed frequencies
N = sum(o) # Total number of observations

theta = (0:100)/100
loglike = 1071*log(2+theta)+130*log(1-theta)+299*log(theta)
plot(loglike ~ theta,type='n')
lines(loglike ~ theta) 
grid()

theta = 0.82+0.01*((0:100)/100)
loglike = 1071*log(2+theta)+130*log(1-theta)+299*log(theta)
plot(loglike ~ theta,type='n')
lines(loglike ~ theta) 
grid()

theta = 0.824+0.001*((0:100)/100)
loglike = 1071*log(2+theta)+130*log(1-theta)+299*log(theta)
plot(loglike ~ theta,type='n')
lines(loglike ~ theta) 
grid()

loglike <- function (theta) 1071*log(2+theta)+130*log(1-theta)+299*log(theta)
theta_hat <- optimize(loglike, c(0, 1), maximum=T)
theta_hat

theta_hat = 0.8247
p_hat = c((2+theta_hat)/4,(1-theta_hat)/4,(1-theta_hat)/4,theta_hat/4) # Genetic model
e = N*p_hat # Expected frequencies

e

Xsq = sum((o-e)^2/e)
Xsq

1-pchisq(Xsq,df=2)

chisq.test(o,p=e/N) # Note incorrect df in output