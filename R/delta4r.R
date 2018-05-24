#' delta4r : Estimated reproducibility metric
#'
#' @description This function is used the reproducibility metric. It is the difference in
#'              theta
#'
#' @param mydat A data frame with the outcome variable and exposure variables
#' @param  dpar The parameters of the normally distributed biomarker.
#' @return A value in the range  of 0-1.
#'
#' @details This metric measure the difference in the decrease of  expected proportion of events when the
#'          gold standard bimarker is measured and when the modified asssay is measured.
#.
#'
#' @examples
#'   # Let the data is stored in an object named mydat
#'   # Let dpar the parmaters of the normally distributed biomarker
#'   truetheta <- thetags(mydat, dpar)
#' @author Henok Woldu
#' @export


delta4r <-  function(mydatt, dpar){

             truetheta <-  thetags(mydatt, dpar)
             modtheta <- thetama(mydatt)

        delta <- truetheta - modtheta

   return(delta)

 }

# end of code
