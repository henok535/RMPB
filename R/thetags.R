
#' thetags : Estimated theta when the gold standard biomarker is observed
#'
#' @description This function is used to estimate theta assuming the gold standard biomarker is used.
#'              Further the distribution of the biomarker is assumed to be normaly distributed with
#'              a given mean and variance.
#'
#' @param mydat A data frame with the outcome variable and exposure variables
#' @param  dpar The parameters of the normally distributed biomarker.
#' @return A value in the range  of 0-1.
#'
#' @details This metric measure the decrease in the expected proportion of events that were avoided
#'          as a result of using the biomarker guided treatment. This is a direct biomarker utility
#'          metric as can quantify the net benefit that we can gain using the biomarker to guide treatmetn.
#.
#'
#' @examples
#'   # Let the data is stored in an object named mydat
#'   # Let dpar the parmaters of the normally distributed biomarker
#'   truetheta <- thetags(mydat, dpar)
#' @author Henok Woldu
#' @export


thetags <- function(mydatt, dpar) {

       fit1 <- glm (mydatt$outcome1 ~ mydatt$xx + mydatt$A + mydatt$Z1, family = binomial(link = "logit"))

              coff <- rbind(fit1$coeff)

                         A0 <- socnorm(coff, dpar)
                         A1 <- actnorm(coff, dpar)
                         Opt <- optnorm(coff, dpar)

             th0 <- A0 - Opt
             th1 <- A1 - Opt

         return(th1)
 }

# end of code
