#' Bootstrap Function for Confidence Intervals
#'
#' The function uses another function to create confidence intervals in which the data is likely to fall, adjustable to a given percentage.
#'
#' @param iter The number of samples that are used to generate statistics.
#' @param x The vector of the sample we choose to resample from. Example: set.seed(39); sam=rnorm(25,mean=25,sd=10) was the line we used above myboot2.
#' @param fun Function which takes the value of a statistical function, such as mean, median, IQR, etc.
#' @param alpha The determinant of the confidence interval in percentage. Example: .05 = 95% confidence.
#' @param cx Size of the text.
#'
#' @return The function takes a sample of size "n" from sample "n" with replacement, creates a confidence interval from alpha/2 and (1-alpha)/2.
#' The function then creates a histogram with a verticl line segment where the statistic in question lies (i.e. mean).
#' A line segment underlining the confidence interval appears on the histogram as well. The upper and lower bounds
#' are printed onto the histogram as well as the statistic.
#' @export
#'
#' @examples
#' iter=10000;sam=round(rnorm(30,mean=20,sd=3),3);fun="mean";alpha=0.05;myboot(iter=10000,x=sam,fun="mean",alpha=0.05,xlab="mean(x)",col=rainbow(35))
myboot2=function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){
  n=length(x)   #sample size
  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)
  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Blue",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Blue",cex=cx)
  text(pte,max(para$density)/2,round(pte,2),cex=cx)
  invisible(list(ci=ci,fun=fun,x=x))
}
