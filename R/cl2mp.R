#' cl2mp : Change Clinician Input to MOdel parameters
#'
#' @description  The function is used to change the clinician input values to model parameters.
#' This values are to be the best intelectural guess from the expertise in that
#' area of study or they can be taken from previous studies.
#'
#'
#' @param k A vector of length four which specify the clinician input values.
#' @param z A vector of length two for the 25th and 75th percentile value of the biomarker values.
#' @return  A vector of lenght four containing the coefficients of the model
#' @examples
#'   # If we know the 25th and 75th percentile values of the biomarker are 3.2 and 5.8
#'   # And K1 = 0.6 , K2 = 2.5 , K3= 1.5 and K4= 0.6, then use the function like
#'   betavalues <- cl2mp(c(k1,k2,k3,k4), c(3.2,5.8))
#'   betavalues
#' @author Henok Woldu
#' @export



 cl2mp <- function (k,z) {
    Bvalues=numeric(0)
    if (k[1]==k[3] && k[2]==k[4]) {warning ("Biomarker is not related to Outcome")}
    if (z[1]==z[2] | z[1]>= z[2]) { stop ("Wrong  choice of z values")}
    if (length(k)!=4) {stop ("length of K must be four")}

    Bvalues[1] <- (k[2]*z[1] -k[1]*z[2])/(z[1]-z[2])
    Bvalues[2] <- (k[1] -k[2])/(z[1]-z[2])
    Bvalues[3] <- (k[1]*z[2] -k[2]*z[1]-k[3]*z[2]+k[4]*z[1])/(z[1]-z[2])
    Bvalues[4] <- (k[2] -k[1]+k[3]-k[4])/(z[1]-z[2])

    return(c(Bvalues));
  }

# end of code






