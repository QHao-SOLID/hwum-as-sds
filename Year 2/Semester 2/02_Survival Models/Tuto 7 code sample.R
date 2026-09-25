source("Engwales_read.r")
library(gnm)

# Extraction of appropriate ranges of data for analysis
DTH = Dth.M[Age >= 50 & Age <= 90, Year >= 1940 & Year <= 1980]
EXP = Exp.M[Age >= 50 & Age <= 90, Year >= 1940 & Year <= 1980]
logMU = log(DTH/EXP)

AGE = 50:90; YEAR = 1940:1980

# Part A
# Lining up of dataset
AGE.FCT = factor(rep(AGE, length(YEAR)))
YEAR.FCT = factor(rep(YEAR, each = length(AGE)))
DTH.VCT = c(DTH); EXP.VCT = c(EXP)

# Fitting the Lee-Carter Model
Lc.model = gnm(DTH.VCT ~ -1 + AGE.FCT + Mult(AGE.FCT, YEAR.FCT), 
               offset = log(EXP.VCT), 
               family = poisson)

Lc.model

# Extracting coefficient values
alpha.gnm = Lc.model$coef[1:length(AGE)]
beta.gnm = Lc.model$coef[(length(AGE)+1):(2*length(AGE))]
kappa.gnm = Lc.model$coef[-c(1:(2*length(AGE)))]

# Identifiability constraints
alpha.LC = alpha.gnm - beta.gnm * mean(kappa.gnm)
beta.LC = beta.gnm / (length(AGE) * mean(beta.gnm))
kappa.LC = length(AGE) * mean(beta.gnm) * (kappa.gnm - mean(kappa.gnm))

# Plotting the values
plot(AGE, alpha.LC, type = "p", pch = 19)
plot(AGE, beta.LC, type = "p", pch = 19)
plot(YEAR, kappa.LC, type = "p", pch = 19)

# Part B
delta = mean(diff(kappa.LC))
proj.kappa = kappa.LC[length(kappa.LC)] + (c(1981:2005)-1980) * delta
sigmahat2 = var(diff(kappa.LC))
pred.errors.kappa = sqrt((c(1981:2005)-1980) * sigmahat2)

# Confidence intervals
CI.low.kappa = proj.kappa + qnorm(0.025) * pred.errors.kappa
CI.upp.kappa = proj.kappa + qnorm(0.975) * pred.errors.kappa

# Plotting points without projection and CI
plot(YEAR, kappa.LC, type = "p", pch = 19, 
     xlim = c(1940, 2005), 
     ylim = c(min(proj.kappa) + qnorm(0.025) * max(pred.errors.kappa), max(kappa.LC)))

# Plot projection
lines(1981:2005, proj.kappa, lwd=2)

# plot CI
lines(1981:2005, CI.low.kappa, lwd = 2, lty = 2)
lines(1981:2005, CI.upp.kappa, lwd = 2, lty = 2)

# Transform for log mu
# Choose age x = 60
age.plot = 60
ind = age.plot - 50 + 1

proj.logmu = alpha.LC[ind] + beta.LC[ind] * proj.kappa
CI.low.logmu = alpha.LC[ind] + beta.LC[ind] * CI.low.kappa
CI.upp.logmu = alpha.LC[ind] + beta.LC[ind] * CI.upp.kappa

# Plotting values for logmu
plot(YEAR, logMU[AGE == age.plot], type = "p", pch = 19, 
     xlim = c(1940, 2005), 
     ylim = c(min(CI.low.logmu), max(logMU[AGE == age.plot])))

lines(1981:2005, proj.logmu, lwd = 2)
lines(1981:2005, CI.low.logmu, lwd = 2, lty = 2)
lines(1981:2005, CI.upp.logmu, lwd = 2, lty = 2)

# Part C
# Extracting the correct datasets
correct.dth = Dth.M[Age == age.plot, Year >= 1981 & Year <= 2005]
correct.exp = Exp.M[Age == age.plot, Year >= 1981 & Year <= 2005]
correct.logmu = log(correct.dth/correct.exp)

points(1981:2005, correct.logmu, pch = 19, col = "blue")



