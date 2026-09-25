#
#    Tutorial 2, Q4: defining the partial log likelihood function, plotting it 
#    and then using coxph( ) to confirm the fit of the Cox model
#
PartialL <- function(b){
  p <- exp(b)
  2*b - 3*log(1+p) - log(4+5*p) - log(2+3*p)
}
#
U <- function(b){
  p <- exp(b)
  2 - 3*p/(1+p) - 5*p/(4+5*p) - 3*p/(2+3*p)
}
#    Define a convenient plotting function
# Note that this plotting function is for the partial log-likelihood
# rather than the score function that is asked for in the questions. The
# obtained estimates are the same.
#
PlotL <- function(Left, Right){
  x <- seq(Left, Right, length = 1000)
  plot(x, PartialL(x), main = "Partial log likelihood", type = "l",
       xlab = "beta", ylab = "logL", col = "red", lwd = 2)
  grid( )
}
#
PlotL(-0.7, -0.35)
PlotL(-0.6, -0.45)
PlotL(-0.55, -0.50)
PlotL(-0.54, -0.53)
Beta <- -0.534
#
PlotU <- function(Left, Right){
  x <- seq(Left, Right, length = 1000)
  plot(x, U(x), main = "Score of beta", type = "l",
       xlab = "beta", ylab = "U(beta)", col = "red", lwd = 2)
  grid( )
  abline(h=0)
}
#
PlotU(-0.7, -0.35)
PlotU(-0.6, -0.45)
PlotU(-0.55, -0.50)
PlotU(-0.54, -0.53)
Beta <- -0.534
#
#   and beta = -0.534
#
Score <- function(x) 2/x - 3/(1+x) - 3/(2+3*x) - 5/(4+5*x)
#
PlotScore <- function(Left, Right){
  x <- seq(Left, Right, length = 1000)
  plot(x, Score(x), main = "Score function", type = "l",
       xlab = "Phi", ylab = "Score", col = "red", lwd = 2)
  grid()
  abline(h = 0, lty = 2)
}
PlotScore(0.5, 0.7)
PlotScore(0.55, 0.6)
PlotScore(0.58, 0.59)
Phi <- 0.586
#
#  and phi = 0.586
#
Slope <- (Score(0.586 + 0.001) - Score(0.586 - 0.001))/0.002; Slope
SE <- sqrt(-1/Slope); SE
#
#  We check that the LRT for beta is equivalent to the LRT for phi
#
#  First LRT for beta
#
log.Lambda.Beta <- -2*(PartialL(0) - PartialL(Beta)); log.Lambda.Beta 
#
#  Now fo phi - first we need to define the partial log likelihood for phi
#
PartialL.Phi <- function(x) 2*log(x) - 3*log(1+x) - log(4+5*x) - log(2+3*x)
log.Lambda.Phi <- -2*(PartialL.Phi(1) - PartialL.Phi(Phi)); log.Lambda.Phi
#
#   And the two values of the LRT statistic are the same and equal to 0.348..
#
#   Finally using coxph( )
#
Times <- c(3,6,8,10,15, 5,7,9,12,16)
Censor <- c(1,0,0,1,1, 1,0,1,0,1)
Group <- factor(c(rep(1,5), rep(2,5)))
#
library(survival)
Cox <- coxph(Surv(Times, Censor) ~ Group)
summary(Cox)
