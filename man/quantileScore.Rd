\name{quantileScore}
\alias{quantileScore}
\title{Quantile Score}
\description{Calculates verification statistics for quantile forecasts.}
\usage{quantileScore(obs, pred, p, breaks, ...)}
\arguments{
  \item{obs}{Vector of observations}
  \item{pred}{Vector of quantile forecasts}
  \item{p}{Probability level of quantile forecasts [0,1].}
  \item{breaks}{Values used to bin the forecasts}
  \item{\dots}{Optional arguments}
}
\details{
This function calculates the quantile score and its decomposition into reliability, resolution, and uncertainty. Note that a careful binning (discretization of forecast values) is necessary to obtain good estimates of reliability and resolution (see Bentzien and Friederichs (2013) for more details).
}
\value{
  \item{qs.orig }{Quantile score for original data}
  \item{qs }{Quantile score for binned data}
  \item{qs.baseline }{Quantile score for climatology}
  \item{ss }{Quantile skill score}
  \item{qs.reliability }{Reliability part of the quantile score}
  \item{qs.resolution }{Resolution part of the quantile score}
  \item{qs.uncert }{Uncertainty part of the quantile score}
  \item{y.i }{Discretized forecast values -- defined as the mean value of forecasts in each bin}
  \item{obar.i }{Conditional observed quantiles}
  \item{prob.y }{Number of forecast-observation pairs in each bin}
  \item{obar }{Climatology -- unconditional sample quantile of observations}
  \item{breaks }{Values used to bin the forecasts}
  \item{check }{Difference between original quantile score and quantile score decomposition}
}
\references{Bentzien, S. and Friederichs, P. (2013) Decomposition and graphical portrayal of the quantile score. Submitted to \emph{QJRMS}.}
\author{Sabrina Bentzien}
\note{This function is used within \code{verify}.}
\seealso{\code{\link{check.func}}, \code{\link{qrel.plot}}}
\examples{
data(precip.ensemble)

#Observations are in column 3
obs <- precip.ensemble[,3]

#Forecast values of ensemble are in columns 4 to 54
eps <- precip.ensemble[,4:54]

#Quantile forecasts from ensemble
p <- 0.9
qf <- apply(eps,1,quantile,prob=p,type=8)

#generate equally populated binnng intervals
breaks <- quantile(qf,seq(0,1,length.out=11))

qs <- quantileScore(obs,qf,p,breaks)
\dontrun{ qrel.plot(qs) }

}
\keyword{ manip }
