#' My Normal Distribution Function
#'
#' Creates a curve of a normal distribution function and plots and shaded the area
#'
#' This is the tjird function in this package for the MATH4753 class. It takes the values of the vectors and plugs them into the equation. It creates a curve of a normal distribution, and shades the area created by the specific vectors and adds the value of the area to the curve.
#' @param a a vector in which is value represents the upper limit for the x-axis
#' @param mu a vector representing the mean
#' @param sigma a vector representing the standard deviation
#'
#' @return a curve with the area shaded and plotting the value of the area
#' @export
#'
#' @examples
#' x=5, mu=3, sigma=6;mycurve(x,mu,sigma)
myncurve = function(mu, sigma, a){
  curve(dnorm(a,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma)) #draws a normal curve
  xcurve=seq(-∞,a,length=1000)  #finds the area between x=-∞ and x
  ycurve=dnorm(xcurve,mean,sd) #y values correspond to the x values
  polygon(c(-∞,xcurve,a),c(0,ycurve,0),col="Orange") #fills in the polygon with the given vertices
  prob=pnorm(a,mean,sd)-pnorm(-∞,mean,sd) #calclates or finds the area
  prob=round(prob,4) #sets the area to 4 decimal places
  text((-∞+a)/2,0.5*0.1,prob)  #plots the area
}
