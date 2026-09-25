#
#    Tutorial 2, Q3: defining the partial log likelihood function, plotting it 
#    and then using coxph( ) to confirm the fit of the Cox model
#
PartialL <- function(b){
  p <- exp(b)
  2*b - log(14+15*p) - log(12+14*p) - log(12+13*p)- log(12+11*p) -
  log(10+11*p) - log(8+8*p) - log(7+8*p) - log(6+8*p)
}
#
#    Define a convenient plotting function
#
PlotL <- function(Left, Right){
  x <- seq(Left, Right, length = 1000)
  plot(x, PartialL(x), main = "Partial log likelihood", type = "l",
       xlab = "beta", ylab = "logL", col = "red")
  grid( )
}
#
par(ask = T)
PlotL(-4, 4)
PlotL(-2, 0)
PlotL(-1.5, -1)
PlotL(-1.25, -1.15)
PlotL(-1.20, -1.18)
#
#   and beta = -1.19
#
library(survival)
Times <- c(1,1,4,4,5,5,7,8,10,10,12,16,16,16, 1,2,3,3,5,5,5, rep(16, 8))
Censor <- c(1,0,1,0,1,0,1,1,1,rep(0,5), 0,1,1,rep(0,12))
Surv(Times, Censor)
Treat <- c(rep(1,14), rep(2,15))
cbind(Times, Censor, Treat)
#
Cox.fit <- coxph(Surv(Times, Censor) ~ Treat, method = "breslow")
summary(Cox.fit)
# note that method="breslow" specifies a method for tie handling. 
# As there are no tied death times here, it has no effect, 
# see help(coxph) for more details. 
