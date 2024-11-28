\name{hg.test}
\alias{hg.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hering-Genton Statistic
}
\description{
Calculate the Hering-Genton test statistic and its p-value
}
\usage{
hg.test(loss1, loss2, alternative = c("two.sided", "less", "greater"),
       mu = 0, alpha = 0.05, plot = FALSE, type = "OLS" )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{loss1}{
  The loss series for the first model
}
  \item{loss2}{
  The loss series for the second model
}
  \item{alternative}{a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". }
  \item{mu}{a number indicating the true value of the mean loss differential}
  \item{alpha}{the size for the test.}
  \item{plot}{
  logical, should the ACF and PACF functions be plotted for each of the loss functions and the loss differential.  Also produces a scatter plot of the auto-covariances against the lag terms.
}
  \item{type}{
  Should ordinary (default) or weighted loss be used?
}
}
\details{

The Hering-Genton test (Hering and Genton 2011) uses a parametric model (in this case the exponential) fit to the auto-covariance function (using all the lags up to half the total distance in the series) and uses a weighted average of all the lag terms from this parametric model as an estiamte for the standard error of the mean loss differential statistic.  The rest is a usual paired t-test for differences in mean whereby the standard error is estimated using the (single) differenced series (the loss differential series) and accounts for temporal dependence.  The series need not be loss series.
}
\value{
An object of class \dQuote{htest} is returned with components:
\item{statistic }{The value of the estimated test statistic.}
\item{p.value}{The estimated p-value.}
\item{conf.int}{a confidence interval for the mean appropriate to the specified alternative hypothesis.}
\item{estimate}{the estimated mean-loss differential.}    
\item{null.value}{the specified hypothesized value of the mean loss differential.}
\item{stderr}{the estimated standard error.}
\item{alternative, method}{a character string describing the alternative hypothesis.}
\item{data.name}{a character vector giving the names of the data.}
}
\references{
Hering, A. S. and Genton, M. G. (2011) Comparing Spatial Predictions. \emph{Technometrics}, \bold{53} (4), 414--425. doi: 10.1198/TECH.2011.10136.
}
\author{
Eric Gilleland
}

\seealso{
\code{\link{predcomp.test}}
}
\examples{
\dontrun{
x <- arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          sd = sqrt(0.1796))

y <- arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          sd = sqrt(0.1796))

rho <- cbind( c( 1, 0.8 ), c( 0.8, 1 ) )
xy <- t( rho %*% rbind( x,y ) )

hg.test( abs( xy[,1] ), abs( xy[,2] ) )

t.test( abs( xy[,1] ) - abs( xy[,2] ) )
}

}
\keyword{ htest }
