#' thetama : Estimated theta when the modified assay is observed.
#'
#' @description This function is used to estimate theta assuming the gold standard biomarker is used.
#'              Further the distribution of the biomarker is assumed to be normaly distributed with
#'              a given mean and variance.
#'
#' @param mydat A data frame with the outcome variable and exposure variables
#' @return A value in the range  of 0-1.
#'
#' @details This metric measure the decrease in the expected proportion of events that were avoided
#'          as a result of using the biomarker guided treatment. However the biomarker observed is the
#'          modified asssay rather than the gold standard assay. To reduce the computational time, a raoblackwelization
#'          method conditioning over the sufficient statistics was used.
#.
#'
#' @examples
#'   # Let the data is stored in an object named mydat
#'   # Let dpar the parmaters of the normally distributed biomarker
#'   modtheta <- thetama(mydat)
#' @author Henok Woldu
#' @export


thetama <- function (mydat) {

        bmrk <- mydat$xnew  # extract the sufficient statistic

        # fit the logit model

       # fit1 <- glm (mydat$outcome1 ~ mydat$ww + mydat$A +  mydat$Z2 ,family = binomial(link = "logit"))

        # fit2 <- glm (mydat$outcome1 ~ mydat$xx + mydat$A + mydat$Z1,family = binomial(link = "logit"))

         fit1 <- glm(mydat$outcome1 ~ mydat$xnew + mydat$A + mydat$Z3, family = binomial(link = "logit"))


        coeff <- rbind(fit1$coeff)  # extract coefficient from the fitted model





             # determine the optimal treatmetn assignment rule

            if (coeff[4] > 0 ) {

                      xx0 <- bmrk[bmrk > -(coeff[3]/coeff[4])];
                      xx1 <- bmrk[bmrk < -(coeff[3]/coeff[4])];

                     } else {

                           xx0 <- bmrk[bmrk < -(coeff[3]/coeff[4])];
                           xx1 <- bmrk[bmrk > -(coeff[3]/coeff[4])];
             }



                        c1 <- exp(coeff[1]+ coeff[3] + (coeff[2]+coeff[4])*bmrk )
                        c2 <- (1 + c1)
                        c3 <- (c1/c2)


                            a1 <- exp(coeff[1]+ coeff[3] + (coeff[2]+coeff[4])*xx1 )
                            a2 <- (1 + a1)
                            a3 <- (a1/a2)

                                   b1 <- exp(coeff[1]+ coeff[2]*xx0 )
                                   b2 <- (1 + b1)
                                   b3 <- (b1/b2)

               # under treat all using monte carlo estimation

                   trt <- mean(c3)

              # under optimal using the monte carlo estimation

                     comb <- c(a3,b3)
                    opt <- mean(comb)

            # assuming the default treatment is treat all, theta under the modified biomarker is obtained as

              theta11 <- trt - opt

      return(theta11)

  }


# end of code
