# Q3

library(MASS)

file = scan("call_centre_data.txt")
file

hist(file)
boxplot(file)

# a
plot(qgamma(ppoints(100),3,2),sort(file))
abline(0,1, col="blue")

# function 

gamma.chi2.gof <- function(data, ncells, eq.prob = TRUE)
{
  eps = 0.001
  mi = min(data)-eps; ma = max(data)+eps; 
  
  if (eq.prob == TRUE){
    ## Determine cells (equal probability) 
    breaks.f <- qgamma( (0:(ncells)/ncells), shape = 3, rate = 1 )
  }
  else{
    ## OR Determine cells (equal length)
    breaks.f <- seq(mi,ma,by=((ma-mi)/ncells)); 
  }
  
  cut.f <- cut(data, breaks = breaks.f, right=F); ##bin data
  table.f <- table(cut.f);  ##count data in bins
  
  ## Compute expected frequencies
  prob <- numeric(ncells);  exp.f <- numeric(ncells)
  for (i in 1:(length(breaks.f)-1)) {
    prob[i] = pgamma(breaks.f[i+1], shape = 3, rate = 1 ) - pgamma(breaks.f[i], shape = 3, rate = 1 )
    exp.f[i] <- prob[i] * length(data)
  }
  
  ## Extract observed frequencies
  obs.f <- numeric(ncells)
  for (i in 1:ncells) obs.f[i] <- table.f[[i]]
  
  ## Compute chi-sq statistic
  x2 <- sum( (obs.f - exp.f)^2 / exp.f)
  
  ## Calculate pvalue
  df.f = ncells-1
  pval = 1 - pchisq(x2, df.f)
  
  res = list(table.f, exp.f, x2, df.f, pval)
  return(res)
}

# b i 
gamma.chi2.gof(file,7,FALSE)

# b ii
gamma.chi2.gof(file,7,TRUE)

# There is no point comparing p-values between different tests. 