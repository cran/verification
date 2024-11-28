\name{observation.error}
       \alias{observation.error}
        \title{Observation Error}
       \description{Quantifies observation error through use of a ``Gold
       Standard'' of observations.}
     \usage{observation.error(obs, gold.standard = NULL, \dots) }
       \arguments{
	 \item{obs}{Observation made by method to be quantified.   This
	   information can be entered two ways.  If obs is a vector of
	   length 4, it is assumed that is contains the values c(n11,
	   n10, n01, n00), where n11 are the number of correctly
	   predicted events and n01 is the number of incorrectly
	   predicted non-events.  }
	 \item{gold.standard}{The gold standard.  This is considered a
	   higher quality observation (coded \{0, 1 \} ).}
	 \item{\dots}{Optional arguments.}
       }
       \value{
          \item{t}{Probability of forecasting an event, when an event
	   occurs.  A perfect value is 1. }
	 \item{u}{Probability of forecasting that no event will occur,
	   when and event occurs.  A perfect value is 0. }
       }
       \note{ This function is used to calculate values for the
	 \code{\link{measurement.error}} function.}
       \examples{
obs <- round(runif(100))
gold <- round(runif(100) )
observation.error(obs, gold)

## Pirep gold standard

observation.error(c(43,10,17,4) )
}
 \seealso{\code{\link{measurement.error}}}
\author{Matt Pocernich}

       \keyword{file}
