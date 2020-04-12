#' My Standard Normal Distribution Functiom
#'
#' Finds standard normal distribution from vectors
#'
#' This is the first function in this package for the MATH4753 class. It takes the values of the vectors and plugs them into the equation. The answer shows the standard normal distribution, or in other words, how much are the data far away from the mean value.
#'
#' @param x a vector in which its value is used to calculate the standard normal distribution
#'
#' @return a standard normal distribution vector in the form of a vector object
#' @export
#'
#' @examples
#' x=1:30
#' mysnd(x)
mysnd = function(x){
  (x - mean(x)) / sd(x)
}
