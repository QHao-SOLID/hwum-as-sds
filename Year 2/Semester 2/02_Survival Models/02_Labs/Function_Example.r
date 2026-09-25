#
#   Defining a simple function, plotting it and solving an equation.
#
#   Define the function - note that X is a dummy argument in
#   the definition
#.  
My.function <- function(X) X + 5 - exp(X)/X
#
#   Set up the x-values
#
x <- seq(1, 4, by = 0.001)
#
#   and plot
#
plot(x, My.function(x), type = "l", ylab = "f(x)",
     main = "f(x) = x + 5 - exp(x)/x, 1 < x < 4")
#
#   You can solve the equation graphically.  Another function helps.
#
Solve.function <- function(Left, Right, By){
  x <- seq(Left, Right, by = By)
  plot(x, My.function(x), type = "l", ylab = "f(x)",
       main = "f(x) = x + 5 - exp(x)/x")
  abline(h = 0)
}
#
#  Now call the plotting function with a sequence of values
#  which home in on the answer.
#
Solve.function(3, 4, 0.001)
Solve.function(3.2, 3.4, 0.001)
Solve.function(3.3, 3.35, 0.001)
Solve.function(3.31, 3.32, 0.001)
#
#  and stop when you are happy.
#
#  You can also do this by Newton-Raphson, for example.
#
#  First we define the derivative as a function
#
Deriv <- function(x) 1 - exp(x)/x + exp(x)/x^2
#
#   and then use both our functions to define the Newton-Raphson algorithm
#
Newton <- function(x) x - My.function(x)/Deriv(x)
#
plot(x, My.function(x), type = "l", ylab = "f(x)",
     main = "f(x) = x + 5 - exp(x)/x, 1 < x < 4")
#
Newton(3)
Newton(3.37674)
Newton(3.31933)
#
#  or four iteration in one go
#
Newton(Newton(Newton(Newton(3))))
#
#  which will give us the solution to about 6 sig figs.
