################################################
#Chapter 3.1 Chi-square goodness of fit tests

ozone = dget("ozone-1.R")
attach(ozone)
stamford
xbar= mean(stamford); xbar
sdbar= sd(stamford); sdbar

## Use cells of length 25
stam.breaks = seq(0,250,by=25); stam.breaks  ##define breaks
stam.cut = cut(stamford, breaks = stam.breaks, right=F); ##bin data
stam.table = table(stam.cut); stam.table ##count data in bins

## Compute expected frequencies
prob = numeric(10);  exp.f = numeric(10)
for (i in 1:(length(stam.breaks)-1)) {
  prob[i] = pnorm(stam.breaks[i+1], xbar, sdbar) - pnorm(stam.breaks[i], xbar, sdbar)
  exp.f[i] <- prob[i] * length(stamford)
}
exp.f

## Extract observed frequencies
obs.f = numeric(10)
for (i in 1:10) obs.f[i] = stam.table[[i]]
obs.f 
# or
obs.f = hist(stamford, breaks=stam.breaks, plot=F, right=F)$counts

## Compute chi-sq statistic
x2 = sum( (obs.f - exp.f)^2 / exp.f); x2

## Calculate pvalue; 
pval = 1 - pchisq(x2, (10-2-1))
pval

##########################
## Now join cells together
exp.f1 = c(exp.f[1:7], sum(exp.f[8:10])); exp.f1
obs.f1 = c(obs.f[1:7], sum(obs.f[8:10])); obs.f1

## Perform test again
x2.new = sum( (obs.f1 - exp.f1)^2 / exp.f1); x2.new

pval.1 = 1 - pchisq(x2.new, (8-2-1))
pval.1


###################################################
#Chapter 3.2 KS goodness of fit tests

### Example 3.1
x = c(0.58, 0.42, 0.52, 0.33, 0.43, 0.23, 0.58, 0.76, 0.53, 0.64)
sort(x)
ks.test(x,punif,0,1)

### Example 3.2
x1 = c(108,112,117,130,111,131,113,113,105,128)
ks.test(x1,pnorm,120,10)
