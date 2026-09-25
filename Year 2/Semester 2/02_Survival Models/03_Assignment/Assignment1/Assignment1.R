############################################################################################
#     
#     Set seed
#
RNGkind(sample.kind = "Rejection")
set.seed(6014) # change this to the last 4 digits of your student ID
#
#     Generate American Shorthair lifetimes with censoring
#
LenA <- sample(160:180, 1)
LifetimeA <- c(sort(round(pmin(rgamma(LenA-10,1.2,0.4), 15), 6)), rep(15,10))
CensorA <- c(rbinom(LenA,1,0.8))
CensorA[LifetimeA == 15] <- 0
#
#     Generate British Shorthair lifetimes with censoring
#
LenB <- sample(140:160, 1)
LifetimeB <- c(sort(round(pmin(rgamma(LenB-10,1,0.3), 15), 6)), rep(15,10))
CensorB <- c(rbinom(LenB,1,0.8))
CensorB[LifetimeB == 15] <- 0
#
#     Combine data & Generate ages at FIP contraction
#
Lifetime <- c(LifetimeA, LifetimeB)
Censor <- c(CensorA, CensorB)
Breed <- factor(c(rep(1,LenA), rep(2,LenB)))
FIPAge <- ifelse(Lifetime>2, ifelse(runif(LenA+LenB)<0.4, pmax(Lifetime-rexp(LenA+LenB,1.5),0), 0), 0)
#
############################################################################################
# Please insert your R code after this line
############################################################################################

library(survival)

############################################################################################
# Part 1
############################################################################################

# Part 1 Q1
# Kaplan–Meier estimates for American Shorthairs
Surv(LifetimeA,CensorA)
KM.fitA = survfit(Surv(LifetimeA,CensorA) ~ 1, conf.type = "plain")

# probability that an American Shorthair survives for at least 10 years
Quantity1 = summary(KM.fitA, times = 10)$surv
Quantity1

# Part 1 Q2
# lower limit of equal tailed 95% confidence interval
Quantity2 = summary(KM.fitA, times = 10)$lower
Quantity2

# upper limit of equal tailed 95% confidence interval
Quantity3 = summary(KM.fitA, times = 10)$upper
Quantity3

# Part 1 Q3
# Kaplan–Meier estimates for British Shorthairs
Surv(LifetimeB,CensorB)
KM.fitB = survfit(Surv(LifetimeB,CensorB) ~ 1, conf.type = "plain")

# probability that a British Shorthair dies within 12 years
Quantity4 = 1-summary(KM.fitB, times = 12)$surv
Quantity4

# Part 1 Q4
# lower limit of equal tailed 95% confidence interval
Quantity5 = 1-summary(KM.fitB, times = 12)$upper
Quantity5

# upper limit of equal tailed 95% confidence interval
Quantity6 = 1-summary(KM.fitB, times = 12)$lower
Quantity6

# Part 1 Q5
# equivalent to calculating 5q3 for American Shorthair. 
Quantity7 = 1 - (summary(KM.fitA, times = 8)$surv/summary(KM.fitA, times = 3)$surv)
Quantity7

# Part 1 Q6
# equivalent to calculating 5q3 for British Shorthair. 
Quantity8 = 1 - (summary(KM.fitB, times = 8)$surv/summary(KM.fitB, times = 3)$surv)
Quantity8

# Part 1 Q7
# Kaplan–Meier estimates for All Shorthairs
Surv(Lifetime,Censor)
KM.fit = survfit(Surv(Lifetime,Censor) ~ 1, conf.type = "plain")

# Probability that new data point is an American Shorthair, given death within 8.2 years
# By Bayes Theorem, we get: P(A|T<=8.2) = P(T<=8.2|A)P(A)/P(T<=8.2)
Quantity9 = ((1-summary(KM.fitA, times = 8.2)$surv)*(LenA/(LenA+LenB)))/(1-summary(KM.fit, times = 8.2)$surv)
Quantity9

############################################################################################
# Part 2
############################################################################################

# Part 2 Q1
Quantity10 = 4

# Part 2 Q2
# Combine data into dataframe
cbind(Lifetime, Censor, Breed)

# Define function
Quantity11 = function(b){
  
  # sort lifetimes that are uncensored
  sorted_uncensored_lifetimes = sort(Lifetime[Censor == 1])
  
  # starting of the loop
  pll = 0
  
  for(j in 1:length(sorted_uncensored_lifetimes)){
    
    t_j = sorted_uncensored_lifetimes[j]
    
    # check if at time t_j is it uncensored and British Shorthair
    z_j = (Breed[Lifetime == t_j & Censor == 1] == 2)
    
    # number of survivors for American Shorthair(A) and British Shorthair(B)
    numA = sum(Lifetime[Breed == 1] >= t_j)
    numB = sum(Lifetime[Breed == 2] >= t_j)
    
    # the derived pll formula
    pll = pll + b * z_j - log(numA + numB * exp(b))
    
  }
  
  return(pll)
  
}

# value at beta = 0.1
beta = 0.1
Quantity12 = Quantity11(beta)
Quantity12

# Part 2 Q3
# Define graph plotting function
PlotPLL = function(Left, Right){
  x = seq(Left, Right, length = 1000)
  plot(x, Quantity11(x), main = "Partial log likelihood", type = "l",
       xlab = "beta", ylab = "log-likelihood", col = "red")
  grid( )
}
# trial and error to obtain MLE of beta. 
PlotPLL(-4, 4)
PlotPLL(-1, 1)
PlotPLL(-0.3, 0)
PlotPLL(-0.2, -0.1)
PlotPLL(-0.15, -0.13)
PlotPLL(-0.145,-0.140)
PlotPLL(-0.142,-0.141)
PlotPLL(-0.1414,-0.1412)

# draw line at beta hat and indicate legend
abline(v=-0.1413, col="blue")
legend("topright", legend=c("Beta_hat", "Pll function"), fill=c("blue", "red"), cex = 0.7)

# partial log likelihood function maximised at beta_hat = -0.1413 from observation. 
Quantity13 = -0.1413

# Part 2 Q4
# Using Cox proportional hazards model
Cox.fit = coxph(Surv(Lifetime, Censor) ~ Breed)
summary(Cox.fit)

# Conducting Likelihood Ratio Test (LRT) on Beta, obtaining test statistic. 
loglikelihood = Cox.fit$loglik

Quantity14 = 2 * (loglikelihood[2]-loglikelihood[1])

# Obtaining p-value. 
Quantity15 = pchisq(Quantity14, df = 1, lower.tail = FALSE)

# Part 2 Q5
rejection_region = qchisq(0.98, df = 1)
rejection_region

# Since rejection region > p-value for 2% significance level, 
Quantity16 = 2
Quantity16

# Part 2 Q6
Quantity17 = 3
Quantity17

############################################################################################
# Part 3
############################################################################################

# Part 3 Q1
# Combine data into dataframe
cbind(Lifetime, Censor, Breed, FIPAge)

# Creating log-likelihood function
Quantity18 = function(mu, sigma, nu){
  
  # obtaining W (wait time) and T (Number of events), 
  # where h, i, d represent healthy, infected and death respectively. 
  
  contracted_FIP_lifetimes = Lifetime[FIPAge != 0]
  
  W_h = sum(FIPAge[FIPAge != 0]) + sum(Lifetime[FIPAge == 0])
  W_i = sum(contracted_FIP_lifetimes - FIPAge[FIPAge != 0])
  
  T_hd = length(Lifetime[FIPAge == 0 & Censor == 1])
  T_hi = length(Lifetime[FIPAge != 0])
  T_id = length(Lifetime[FIPAge != 0 & Censor == 1])
  
  # Derived formula of Log-likelihood function
  LLF = T_hd * log(mu) + T_hi * log(sigma) + T_id * log(nu) - (mu + sigma) * W_h - (nu) * W_i
  
  return(LLF)
  
}

Quantity19 = Quantity18(0.1,0.1,0.1)

# Part 3 Q2
# Define graph plotting function
PlotLLF = function(Left, Right){
  v = seq(Left, Right, length = 1000)
  plot(v, Quantity18(0.1, 0.1, v), main = "Log-likelihood function", type = "l",
       xlab = "v_hat", ylab = "log-likelihood", col = "red")
  grid( )
}

# trial and error to obtain MLE of v_hat. (fixing mu and sigma to 0.1)
PlotLLF(0.001, 7)
PlotLLF(0.5, 2)
PlotLLF(1, 1.5)
PlotLLF(1.2, 1.4)
PlotLLF(1.25, 1.3)
PlotLLF(1.275, 1.285)
PlotLLF(1.281,1.283)
PlotLLF(1.2817, 1.2821)
PlotLLF(1.28185, 1.28195)
PlotLLF(1.2819, 1.28194)

# draw line at v_hat and indicate legend
abline(v=1.28192, col="blue")
legend("topright", legend=c("v_hat", "LLF function"), fill=c("blue", "red"), cex = 0.7)

Quantity20 = 1.28192

# Part 3 Q3
# Since sigma_x = -d/dx ln(S(x)), 
Quantity21 = 2
  
# Part 3 Q4
# First, define S(x)

S = function(x){
  
  exp(-0.1*x - (1/2) * 0.08 * x^2)
  
}

# Probability that cat contracts FIP between 7th and 10th year. 
Quantity22 = 1 - S(10)/S(7)
Quantity22
