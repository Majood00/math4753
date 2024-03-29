#' My Normal Distribution Function
#'
#' Creates a curve of a normal distribution function and plots and shaded the area
#'
#' This is one of the functions in this package for the MATH4753 class. It takes the values of the vectors and plugs them into the equation. It creates a curve of a normal distribution, and shades the area created by the specific vectors and adds the value of the area to the curve.
#' @param x a vector in which is value represents the upper limit for the x-axis
#' @param mu a vector representing the mean
#' @param sigma a vector representing the standard deviation
#'
#' @return a curve with the area shaded and plotting the value of the area
#' @export
#'
#' @examples
#' x=5
#' mu=3
#' sigma=6
#' myncurve(x,mu,sigma)
myncurve = function(mu, sigma, a){
  curve(dnorm(x, mean=mu,sd=sigma), xlim = c( mu - 3*sigma, mu + 3*sigma))
  xcurve=seq(-1000,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(-1000,xcurve,a),c(0,ycurve,0),col="Orange")
  prob=pnorm(a,mean=mu,sd=sigma)-pnorm(-1000,mean=mu,sd=sigma)
  prob=round(prob,4)
  text(3.5,0.015,prob)
}
