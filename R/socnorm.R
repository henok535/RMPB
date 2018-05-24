
# This function is used in the event probability colculations for integral
# but doesn't have any independent function

simpson_v2 <- function(fun, a, b, n=1000) {

     # numerical integral using Simpson's rule
       # assume a < b and n is an even positive integer

  if (a == -Inf & b == Inf) {

              f <- function(t) (fun((1-t)/t) + fun((t-1)/t))/t^2
              s <- simpson_v2(f, 0, 1, n)

            } else if (a == -Inf & b != Inf) {
                      f <- function(t) fun(b-(1-t)/t)/t^2
                      s <- simpson_v2(f, 0, 1, n)

            } else if (a != -Inf & b == Inf) {
                             f <- function(t) fun(a+(1-t)/t)/t^2
                            s <- simpson_v2(f, 0, 1, n)

            } else {
                          h <- (b-a)/n
                         x <- seq(a, b, by=h)
                         y <- fun(x)
                         y[is.nan(y)] <- 0
           s <- y[1] + y[n+1] + 2*sum(y[seq(2,n,by=2)]) + 4 *sum(y[seq(3,n-1, by=2)])
           s <- s*h/3
            }

  return(s)

}

# end of code


#' socnorm : The probability of an event when all subjects are assigned to the standard of care.
#'
#' @description This function is used to estimate the probability of an event assuming all the subjects are
#'              are assigned to the "placebo" or "standard of care" group. The predictive biomarker is assumed
#'              to have a normal distribution with mean nu and variance sigma squared.
#'
#' @param coeff The coefficients obtained from fitting the log linear model and must be a vector of length four
#' @param  dpar The parameters of the biomarker which has a normal distribution and must be of a vector legnth two
#' @return A value of the estimated probability and must be a number between 0 and 1
#'
#' @examples
#'   # Let coff is a vector of length four with each elemnt represent the value of the coefficients of the model
#'   # Let dpar is a vector of legnth two such that the  mean= 2.3 and variance = 0.2 of the predictive biomarker are given
#'   eventplacebo <- socnorm(coff, dpar=c(2.3,0.2))
#' @author Henok Woldu
#' @export


socnorm <- function(coeff,dpar) {
            myreturn=0

              fun2=function(x){ # function

                    a1 <- exp(coeff[1]+ coeff[3] + (coeff[2]+coeff[4])*x)
                    a2 <- (1 + a1)
                    b1 <- 1/(sqrt(2*pi*dpar[2]))
                    b2 <- -(x-dpar[1])^2
                    b3 <- 2*dpar[2]

                      (a1/a2)*b1*exp(b2/b3)

                }

         myreturn=simpson_v2(fun2,-Inf,Inf,1000)

     myreturn;
}



# end of code
