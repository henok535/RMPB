#' datgen : Function to generate data
#'
#' This is used to generate a data from a logit model. The out come is a binary which represents
#' the whether the event is observed or not. The treatment arm, biomarker value are the main
#' exposure variables.
#'
#'
#' @param m The number of data sets to generate
#' @param n The sample size of each data set
#' @param coff The initial coefficients of the model to generate data
#' @param dpar The parameter of the biomarker distribution
#' @return A list with m data set each with n sample size
#' @examples
#'   # Lets say we want to generat m =100 data set each with n= 500 sample size
#'   # Further lets assume the biomarker is normally distributed with mean 0 and var = 1
#'   # If the var of the error is given to be 0.1, then
#'   mydat <- datgen(100,500,c(0,1,0.1))
#' @author Henok Woldu
#' @export



datgen <- function(m, n, coeff, dpar) {

  # initialize the matrices and lists to store the values

  xx <- matrix(nrow=n,ncol = m) ;            ww <- matrix(nrow=n, ncol = m);
  z1 <- matrix(nrow=n,ncol = m) ;            z2 <- matrix(nrow=n, ncol = m) ;
  z3 <- matrix(nrow=n,ncol = m) ;
  A <- matrix(nrow=n,ncol = m);               xnew   <- matrix(nrow=n,ncol = m);
  outcome1 <- matrix(nrow=n,ncol=m) ;
  outcome2 <- matrix(nrow=n,ncol=m);
  rhoxw <- c() ; sigx <- c()  ; sigw <- c()
  myreturn <- list()
  mylog1 <- matrix(nrow=n,ncol=m);
  mylog2=matrix(nrow=n,ncol=m);



  for (i in 1:m) {

    A[,i] <- sample(0:1,size=n,replace = T,prob=rep(1/2,2))   # for treatment assignment
    xx[,i] <- rnorm(n,dpar[1],dpar[2])                        #  the gold standard biomarker
    ww[,i] <- xx[,i] + rnorm(n,0,dpar[3]) ;                   # the modified biomarker
    rhoxw[i] <- cor(xx[,i],ww[,i])                            # correlation between xx vs ww
    xnew[,i] <- dpar[1] + rhoxw[i]*(dpar[2]/dpar[3])*(ww[,i]- dpar[1]) # obtained using conditional distribution

    # the interaction terms

           z1[,i] <- A[,i]*xx[,i] ;
           z2[,i] <- A[,i]*ww[,i] ;
           z3[,i] <- A[,i]*xnew[,i];

    #Caculate probability of an event

    mylog1[,i] <- exp(coeff[1]+coeff[2]*xx[,i]+coeff[3]*A[,i]+coeff[4]*z1[,i])/
                 (1+exp(coeff[1]+coeff[2]*xx[,i]+coeff[3]*A[,i]+coeff[4]*z1[,i]))


    #Caluate the outcome for a given probability p   q
    outcome1[,i] <- rbinom(n, 1, mylog1[,i]) ;

    myreturn[[i]] <- data.frame(outcome1[,i], A[,i], xx[,i], ww[,i], xnew[,i], z1[,i], z2[,i], z3[,i])

     }
            myreturn <- lapply( myreturn,setNames,nm=c("outcome1", "A","xx", "ww","xnew", "Z1", "Z2", "Z3"))
  myreturn;

}


