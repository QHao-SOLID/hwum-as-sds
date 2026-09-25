# Question 3

# this is to compute Likelihood L(B) using R completely, only giving in data points. 

# idea, get log likelihood l(B) theoretically in the general form, 
# then loop the data into the "formula"

# code: (ref tuto 2 Q3)

# prep
Times = c(1,1,4,4,5,5,7,8,10,10,12,16,16,16,1,2,3,3,5,5,5,rep(16,8))

Censor = c(1,0,1,0,1,0,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0, rep(0,8))

Treat = factor(c(rep(1,14),rep(2,15)))

# see dataset
cbind(Times, Censor, Treat)

# the function

PLL = function(b){
  
  # sort stuff that are uncensored
  sorted_Times_uncens = sort(Times[Censor == 1])
  
  # the loop
  pll = 0
  
  for(j in 1:length(sorted_Times_uncens)){
    
    t_j = sorted_Times_uncens[j]
    # check if at time t_j is it uncensored and control side data
    z_j = (Treat[Times == t_j & Censor == 1] == 2)
    # number of survivors in each group
    numrisk_A = sum(Times[Treat == 1] >= t_j)
    numrisk_B = sum(Times[Treat == 2] >= t_j)
    
    # the formula
    pll = pll + b * z_j - log(numrisk_A + numrisk_B * exp(b))
    
  }
  
  return(pll)
  
}

# Demonstration
PLL(-1)

# plots
plot_pll = function(Left, Right, Len){
  
  b = seq(Left, Right, length = Len)
  plot(b, PLL(b), type = "l")
  
}

plot_pll(-1.1929,-1.1928,1000)

# Cox

Cox.fit = coxph(Surv(Times, Censor) ~ Treat)
summary(Cox.fit)

# END