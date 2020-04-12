% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2.R
\name{myboot2}
\alias{myboot2}
\title{Bootstrap Function for Confidence Intervals}
\usage{
myboot2(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...)
}
\arguments{
\item{iter}{The number of samples that are used to generate statistics.}

\item{x}{The vector of the sample we choose to resample from. Example: set.seed(39); sam=rnorm(25,mean=25,sd=10) was the line we used above myboot2.}

\item{fun}{Function which takes the value of a statistical function, such as mean, median, IQR, etc.}

\item{alpha}{The determinant of the confidence interval in percentage. Example: .05 = 95% confidence.}

\item{cx}{Size of the text.}
}
\value{
The function takes a sample of size "n" from sample "n" with replacement, creates a confidence interval from alpha/2 and (1-alpha)/2.
The function then creates a histogram with a verticl line segment where the statistic in question lies (i.e. mean).
A line segment underlining the confidence interval appears on the histogram as well. The upper and lower bounds
are printed onto the histogram as well as the statistic.
}
\description{
The function uses another function to create confidence intervals in which the data is likely to fall, adjustable to a given percentage.
}
\examples{
myboot2()
}