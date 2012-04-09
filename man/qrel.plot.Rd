\name{qrel.plot}
\alias{qrel.plot}

\title{Quantile Reliability Plot}

\description{The quantile reliability plot gives detailed insights into the performance of quantile forecasts. The conditional observed quantiles are plotted against the discretized quantile forecasts. For calibrated forecasts (i.e., reliability), the points should lie on a diagonal line. The interpretation concerning over or under forecasting of a quantile reliability diagram is analogous to the interpretation of a reliability diagram for probability forecasts of dichotomous events (see for example Wilks (2006), pp. 287 - 290).}

\usage{qrel.plot(A, ...)}

\arguments{

  \item{A}{A "quantile" class object from \code{verify}}
  \item{\dots}{optional arguments.}
}

\references{Bentzien and Friederichs (2013), Decomposition and graphical portrayal of the quantile score, submitted to QJRMS.}

\author{Sabrina Bentzien}

\note{This function is based on \code{reliabiliy.plot} by Matt Pocernich.}

\seealso{\code{\link{quantileScore}}, \code{\link{reliability.plot}}}

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
qrel.plot(qs)

}
\keyword{ hplot }
