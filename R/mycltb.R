#' Central Limit Binomial Function
#'
#' Creates a matrix of vectors that works for both continuous and discrete distributions. The values from the matrix are taken and are used to create a histogram of the sample mean, along with a curved graph showing how close to a normal-shaped graph these graphs that are obtained from the samples are.
#'
#' This is one of the functions that have to be added to the the projext created for MATH4753 course. This function creates curved graphs and histograms based on the central limit theory.
#' It takes a random binomial sample from the population. It then places these numbers into a matrix, where the columns correspond to the interation and the rows will equal the sample size n.
#' Then the apply function is used to apply the mean function to the columns (2) and not rows (1) of the matrix. These values are then placed in an object that is then used to make a histogram.
#' The height of the y-axis is determined by multiplying ymax, which is the maximum density value of the histogram, by 1.1, just to be on the safe side. A density curve is made from the sample distribution.
#' A theoretical normal curve is obtained along with the density plot.
#'
#' @param n sample size
#' @param iter number of iterations; the greater the iteration the better the representation of the graphs are; used instead of infinity
#' @param p probability value
#' @param ...
#'
#' @return a theoretical normal curve along with a density plot/a histogram
#' @export
#'
#' @examples
#' n=5
#' iter=10000
#' p=0.5
#' mycltb(n,iter,p)
mycltb=function(n,iter,p=0.5,...){
  y=rbinom(n*iter,size=n,prob=p)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  w=apply(data,2,mean)
  param=hist(w,plot=FALSE)
  ymax=max(param$density)
  ymax=1.1*ymax
  hist(w,freq=FALSE,  ylim=c(0,ymax),
       main=paste("Histogram of sample mean","\n", "sample size= ",n,sep=""),
       xlab="Sample mean",...)
  curve(dnorm(x,mean=n*p,sd=sqrt(p*(1-p))),add=TRUE,col="Red",lty=2,lwd=3)
}
