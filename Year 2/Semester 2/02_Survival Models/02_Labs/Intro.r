#
#    R is a command line language so you need to know how to stop it!
#    The command prompt is > and you type q() to leave the program as in
#
#    > q()
#
#    ===============================================
#
#    Program to show how to define a function and plot it.  The
#    example is Q3 from the tutorial on the Cox model
#
#    Define a function
#
PartialL = function(b){
  x = exp(b)
  2*b - log(14+15*x) - log(12+14*x) - log(12+13*x) - log(12+11*x) -
    log(10+11*x) - log(8+8*x) - log(7+8*x) - log(6+8*x)
}
#
#     This defines a function with name PartialL and argument b.  You need
#     the {  } because the function is defined over more than one line.  The
#     value of the function is the final line without an assignment.  The
#     following gives you the value of the partial loglikelihood at 1
#
PartialL(1)
#
#     Here is how to plot PartialL over a range of b values
#
x = seq(-1, 1, length = 1000)
plot(x, PartialL(x), type = "l", xlab = "beta", ylab = "PartialL")
#
#     Now we can refine the plot to find the MLE
#
x = seq(-2.0, -1.0, length = 1000)
plot(x, PartialL(x), type = "l", xlab = "beta", ylab = "PartialL")
#
#     We can even make a function to do this
#
Plot.PartialL = function(Left, Right){
  x = seq(Left, Right, length = 1000)
  plot(x, PartialL(x), type = "l", xlab = "beta", ylab = "PartialL")
}
#
Plot.PartialL(-1.4,-1.0)
#
#    and you quickly find that the MLE of beta is -1.19
#
#    If you want to leave R then
#
q()
