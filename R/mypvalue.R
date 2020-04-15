#' p-value Function
#'
#' Finds the two rejection regions and the acceptance region along with the p-value
#'
#' This function calculates alpha/2, then plots the t distance. This function then sets up points on the polygon to the right and left
#'
#' @param t0 a vector of t-calc
#' @param xmax vector of the maximum value of x
#' @param n the sample size
#' @param alpha 1 - confidence level
#'
#' @return a vector with the value of p-value along with a curve that shows the two rejection regions and the acceptance region, then shades in the polygon defined by the line segments. It makes quantiles then plots the cut off t value and adds some annotation to the graph.
#' @export
#'
#' @examples
#'set.seed(55)
#'x1=rnorm(30,mean=25,sd=5)
#'bootpval(x=x1,mu0=24,test="two")
mypvalue=function(t0,xmax=4,n=20, alpha=0.05){
  va=round(pt(-t0,df=n-1),4)
  pv=2*va

  curve(dt(x,df=n-1),xlim=c(-xmax,xmax),ylab="T Density",xlab=expression(t),
        main=substitute(paste("P-value=", pv, " alpha=", alpha)))


  xcurve=seq(t0,xmax,length=1000)
  ycurve=dt(xcurve,df=n-1)

  xlcurve=seq(-t0,-xmax,length=1000)
  ylcurve=dt(xcurve,df=n-1)

  polygon(c(t0,xcurve,xmax),c(0,ycurve,0),col="green")
  polygon(c(-t0,xlcurve,-xmax),c(0,ylcurve,0),col="green")

  q=qt(1-alpha/2,n-1)
  abline( v=c(q,-q),lwd=2)
  axis(3,c(q,-q),c(expression(abs(t[alpha/2])),expression(-abs(t[alpha/2]))))

  text(0.5*(t0+xmax),max(ycurve),substitute(paste(area, "=",va)))
  text(-0.5*(t0+xmax),max(ycurve),expression(area))

  return(list(q=q,pvalue=pv))
}
