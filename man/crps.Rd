\name{crps}
       \alias{crps}
       \title{Continuous Ranked Probability Score}
       \description{Calculates the crps for a forecast made in terms of
	 a normal probability distribution and an observation expressed
	 in terms of a continuous variable.}
       \usage{
    crps(obs, pred, ...)
       }
      \arguments{
	\item{obs}{A vector of observations.}
	\item{pred}{A vector or matrix of the mean and standard
	  deviation of a normal distribution.  If the vector has a
	  length of 2, it is assumed that these values represent the
	  mean and standard deviation of the normal distribution that
	  will be used for all forecasts.}
	\item{\dots}{Optional arguments}
       }
       \value{
	 \item{crps}{Continous ranked probability scores}
	 \item{CRPS}{Mean of crps}
	 \item{ign}{Ignorance score}
	 \item{IGN}{Mean of the ignorance score}
       }
       \note{This function is used within \code{verify}.}
	 \examples{

#  probabilistic/ binary example


x <- runif(100) ## simulated observation.
crps(x, c(0,1))

## simulated forecast in which mean and sd differs for each forecast.
frcs<- data.frame( runif(100, -2, 2), runif(100, 1, 3 ) )
crps(x, frcs)

}


\references{
Gneiting, T., Raferty, A., Westveld, A., and Goldman, T, 2005:
  Calibrated Probabilistic Forecasting Using Ensemble Model Output
    Statistics and Minimum CRPS Estimation. \emph{Monthly Weather Review}, \bold{133} (5), 1098--1118, doi: 10.1175/MWR2904.1.
    }
\author{Matt Pocernich}

       \keyword{file}
