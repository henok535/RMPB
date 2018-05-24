#' RMPB: A package to assess the reproducibility of predictive biomarker
#' clinical utility.
#'
#' The RMPB package provided a number of functions used to estimate the predictive
#' biomarker clinical utility. It main purpose it to estimate the change in the
#' theta value when the obsered biomarker is the gold standard and a modified biomarker.
#' The modified biomarker is consider as one which is the gold standard plus some
#' error.
#'
#' @section Package Structure:
#'   The package is structured in such a way that a user can simulate his/her
#'   own data that suits the study under consideration. Once the data is generated
#'   the following procedures can be used to get an estimate of the reproducibility
#'   metric:
#'
#'   1. Estimate theta assuming the observed biomarker is the gold standard using the
#'      function \thetags\.
#'
#'  2. Estimate theta assuming the observed biomarker is the modified assay using the
#'     function *thetama*.
#'
#'  3. Estimate the difference between 1 and 2 using the function *delta4r*. This value
#'     tells us whether the clinical utility of the predictive biomarker under consideration
#'     which was obtained when the gold standard biomarker was observed is replicable under the
#'     modified assay.
#'
#' @docType package
#' @name RMPB
NULL
