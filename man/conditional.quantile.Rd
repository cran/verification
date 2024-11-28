\name{conditional.quantile}
       \alias{conditional.quantile}
       \title{Conditional Quantile Plot}
       \description{This function creates a conditional quantile plot as shown in
	 Murphy, et al (1989) and Wilks (1995).}
       \usage{
    conditional.quantile(pred, obs, bins = NULL, thrs = c(10, 20), main, ... ) }
       \arguments{
\item{pred}{Forecasted value. \emph{([n,1] vector, n = No. of forecasts)}}
\item{obs}{Observed value.\emph{([n,1] vector, n = No. of observations)}}
\item{bins}{Bins for forecast and observed values.  A minimum number of
values are required to calculate meaningful statistics.  So for
variables that are continuous, such as temperature, it is frequently
convenient to bin these values. \emph{([m,1] vector, m = No. of bins)}}
\item{thrs}{The minimum number of values in a bin required to calculate
the 25th and 75th quantiles and the 10th and 90th percentiles
respectively.  The median values will always be displayed. \emph{( [2,1]
  vector)} }
\item{main}{Plot title}
\item{\dots}{ Plotting options. }
}
        \value{This function produces a conditional.quantile plot.
The \code{y} axis represents the observed values, while the \code{x} axis
represents the forecasted values.  The histogram along the bottom axis
illustrates the frequency of each forecast.}
\note{In the example below, the median line extends beyond the range of
  the quartile or 10th and 90th percentile lines.  This is because there
  are not enough points in each bin to calculate these quartile values.
  That is, there are fewer than the limits set in the \code{thrs} input.}
\examples{

set.seed(10)
m<- seq(10, 25, length = 1000)  
frcst <- round(rnorm(1000, mean = m, sd = 2) )
obs<- round(rnorm(1000, mean = m, sd = 2 ))
bins <- seq(0, 30,1)
thrs<- c( 10, 20) # number of obs needed for a statistic to be printed #1,4 quartile, 2,3 quartiles

conditional.quantile(frcst, obs, bins, thrs, main = "Sample Conditional Quantile Plot")
#### Or plots a ``cont.cont'' class object.

obs<- rnorm(100)
pred<- rnorm(100)
baseline <- rnorm(100, sd = 0.5) 

A<- verify(obs, pred, baseline = baseline,  frcst.type = "cont", obs.type = "cont")
 plot(A)
}

\references{
  Murphy, A. H., B. G. Brown and Y. Chen. (1989) Diagnostic Verification of Temperature Forecasts. \emph{Weather and Forecasting}, \bold{4}, 485--501.
}
\author{Matt Pocernich}

       \keyword{file}
