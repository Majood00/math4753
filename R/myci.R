#' 95% Confidence Interval Function for the Mean of a Sample
#'
#' Finds the 95% confidence interval for a sample with mean mu
#'
#' This is the 4th function in this package. This function produces a vector of an interval. This interval shows the confidence interval for the 95% confidence level of the mean of a sample which is being processed or which goes through this function.
#'
#' @param x the vector of the sample we choose to find the confidence interval for
#'
#' @return vector with an interval for the 95% confidence level
#' @export
#'
#' @examples
#' set.seed(23)
#' x = rnorm(30,mean=10,sd=12)
#' myci(x)
myci = function(x){
  t.test(x,mu=0,conf.level = 0.95)$conf.int
}
