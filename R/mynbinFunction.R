#' My Negative Binomial Distribution Function
#'
#' Finds negative binomial distribution based on the parameters size, probabilities and the value of x. So, in other words, vecators are used to do the calculation
#'
#' This is the second function in this package for the MATH4753 class. It takes the values of the vectors and plugs them into the equation. The answer shows the negative binomial distribution, which is a discrete probability distribution in relation to the number of successes in a sequence of Bernoulli trials that are independent and identically distributed, and that is before a specified number of failures occurs.
#'
#' @param y a vector of number of trials until the kth success
#' @param r a vector which shows the size of the sample
#' @param p a vector in which its value shows the probability
#'
#' @return a negative binomial distribution vector in the form of a vector object
#' @export
#'
#' @examples
#' y=12, r=5, p=0.3;mynbino(y,r,p)
mynbino=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}
