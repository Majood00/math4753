#' Maximum Likelihood Function for Repeated Sampling from the same Distribution
#'
#' Finds the maximum likelihood for repeated sampling fron=m the same distribution
#'
#' This is the 3rd Function in this packages. This function produces a matrix where each x, param is replaced with the function evaluated at those values. Moreover, it makes a vector that is made up of the column sums. Each vector is the log of the maximum likelihood for a new parameter value. This gives the index for the value of y == max. There is a possibility or a probability that there would be a max between two values of the parameter, therefore 2 indices. The first max will take the larger index. Then the function plots a nice point where the maximum likelihood is. Then the function checks slopes, and if it is a max the slope shoud change sign from + to -. And finally, we should get three + and two -'s.
#'
#' @param lfun
#' @param x
#' @param param
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' #mymaxlik(x=c(9,9,1,9,9,9),param=seq(0,1,length=1000),lfun=logbin,xlab=expression(pi),main="Binomial",cex.main=2)
#' #mymaxlik(x=c(3,4,3,5),param=seq(0,20,length=1000),lfun=logpoiss,xlab=expression(lambda),main="Poisson",cex.main=2)
mymaxlik=function(lfun,x,param,...){
  np=length(param)
  z=outer(x,param,lfun)
  y=apply(z,2,sum)
  plot(param,y,col="Blue",type="l",lwd=2,...)
  i=max(which(y==max(y)))
  abline(v=param[i],lwd=2,col="Red")
  points(param[i],y[i],pch=19,cex=1.5,col="Black")
  axis(3,param[i],round(param[i],2))
  ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
  return(list(i=i,parami=param[i],yi=y[i],slope=slope))
}

