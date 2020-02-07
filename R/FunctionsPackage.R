#' My Standard Deviation Function
#'
#' Creates standard deviation from vectors
#'
#' This is the first function in this package for the MATH4753 class. It takes the values of the vectors and plugs them into the equation. The answer shows the standard deviation or in other words, how much are the data far away from the mean value.
#'
#' @param x a vector in which its value is used to calculate the standard deviation
#'
#' @return a standard deviation vector on the form of a vector object
#' @export
#'
#' @examples
#' x=1:30;mysd(x)
mysd = function(x){ # x is a vector
  obj1=sqrt(sum((x-mean(x))^2)/(length(x)-1))
  obj1
}
