###################################################
# Function for Normal GOF test                    #
#                                                 #
# Note: as given here, the function can only be   #
# used for testing a normal dstn with unspecified #
# parameters. Also it does not perform any        #
# grouping of cells. You can modify the function  #
# for a more complete gof test.                   #
###################################################

norm.chi2.gof <- function(data, ncells, eq.prob = TRUE)
{
	xbar = mean(data); sdbar = sd(data)
	
	eps = 0.001
	mi = min(data)-eps; ma = max(data)+eps; 
	
	if (eq.prob == TRUE){
	  ## Determine cells (equal probability) 
	  breaks.f <- qnorm( (0:(ncells)/ncells), xbar, sdbar )
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
	  prob[i] = pnorm(breaks.f[i+1], xbar, sdbar) - pnorm(breaks.f[i], xbar, sdbar)
	  exp.f[i] <- prob[i] * length(data)
	}

	## Extract observed frequencies
	obs.f <- numeric(ncells)
	for (i in 1:ncells) obs.f[i] <- table.f[[i]]

	## Compute chi-sq statistic
	x2 <- sum( (obs.f - exp.f)^2 / exp.f)

	## Calculate pvalue
        df.f = ncells-3
	pval = 1 - pchisq(x2, df.f)

	res = list(table.f, exp.f, x2, df.f, pval)
	return(res)
}



















