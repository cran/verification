\name{quantile2disc}
\alias{quantile2disc}
\title{Convert Continuous Forecast Values to Discrete Forecast Values.}
\description{
Converts continuous forecast values into discrete forecast values. This is necessary in calculating the quantile score decomposition. Discrete forecasts are defined by the mean value of forecasts within a bin specified by the bin vector (bin boundaries).
}
\usage{quantile2disc(x, bins)}
\arguments{
  \item{x}{A vector of forecast values}
  \item{bins}{Vector with bin boundaries}
}
\value{
  \item{new}{New vector x (continuous forecast values replaced with disretized forecast values)}
  \item{mids}{Vector of discrete forecast values}
}
\author{Sabrina Bentzien}
\note{This function is used within \code{quantileScore}.}
\examples{
x <- rnorm(100)
bins <- quantile(x,seq(0,1,length.out=11))

newx <- quantile2disc(x,bins)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
