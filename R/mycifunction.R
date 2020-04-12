#' 95 per cent Confidence Interval Function for the Mean of a Sample
#'
#' Finds the 95 per cent confidence interval for a sample with mean mu
#'
#' This is the 4th function in this package. This function produces a vector of an interval. This interval shows the confidence interval for the 95 per cent confidence level of the mean of a sample which is being processed or which goes through this function.
#'
#' @param x vector of the sample we choose to find the confidence interval for
#'
#' @return vector with the 95 per cent confidence interval for the sample input
#' @export
#'
#' @examples
#' set.seed(50)
#' x=rnorm(30,mean=10,sd=5)
#' myci(x)
myci = function(x){
  t.test(x,mu=0,conf.level = 0.95)$conf.int
}
