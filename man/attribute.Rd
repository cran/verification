% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2004 
% ** University Corporation for Atmospheric Research(UCAR) 
% ** National Center for Atmospheric Research(NCAR) 
% ** Research Applications Program(RAP) 
% ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
% ** 2004/1/7 11:31:8 
% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
\name{attribute}
\alias{attribute}
\alias{attribute.default}
\alias{attribute.prob.bin}
       \title{Attribute plot}
       \description{An attribute plot  illustrates
       the reliability, resolution and uncertainty of a forecast with
       respect to the observation.
       The frequency of binned forecast probabilities are plotted
       against proportions of binned observations.  A perfect forecast would
       be indicated by a line plotted along the 1:1 line.  Uncertainty
       is described as the vertical distance between this point and the
       1:1 line.  The
       relative frequency for each forecast value is displayed in parenthesis. }
      \usage{
\method{attribute}{default}(x, obar.i,  prob.y = NULL, obar = NULL,
    class = "none", main = NULL, CI = FALSE, n.boot = 100, alpha = 0.05,
    tck = 0.01, freq = TRUE, pred = NULL, obs = NULL, thres = thres,
    bins = FALSE, \dots)

\method{attribute}{prob.bin}(x, \dots)
       }
\arguments{
     \item{x}{A vector of forecast probabilities or a \dQuote{prob.bin}
       class object produced by the \code{verify} function. }
     \item{obar.i}{A vector of observed relative frequency of forecast bins.} 
         \item{prob.y}{Relative frequency of forecasts of forecast bins. }
         \item{obar}{Climatological or sample mean of observed
       events.}
     \item{class}{Class of object.  If prob.bin, the function will
       use the data to estimate confidence intervals.}
         \item{main}{Plot title.}
     \item{CI}{Confidence Intervals. This is only an option if the
       data is accessible by using the verify command first. Calculated by bootstrapping
       the observations and prediction, then calculating PODy and
       PODn values.  }
    \item{n.boot}{Number of bootstrap samples.}
    \item{alpha}{Confidence interval.  By default = 0.05}
    \item{tck}{Tick width on confidence interval whiskers.}
    \item{freq}{Should the frequecies be plotted. Default = TRUE} 
    \item{pred}{Required to create confidence intervals}
    \item{obs}{Required to create confidence intervals} 
    \item{thres}{thresholds used to create bins for plotting confidence intervals.}   
    \item{bins}{Should probabilities be binned or treated as unique predictions?}
     \item{\dots}{Graphical parameters} 
       }
      \note{Points and bins are plotted at the mid-point of bins.  This can create distorted graphs if forecasts are created at irregular intervals.
      	}
     \examples{
## Data from Wilks, table 7.3 page 246.
 y.i   <- c(0,0.05, seq(0.1, 1, 0.1))
 obar.i <- c(0.006, 0.019, 0.059, 0.15, 0.277, 0.377, 0.511, 
             0.587, 0.723, 0.779, 0.934, 0.933)
 prob.y<- c(0.4112, 0.0671, 0.1833, 0.0986, 0.0616, 0.0366,
            0.0303,  0.0275, 0.245, 0.022, 0.017, 0.203) 
 obar<- 0.162
 
attribute(y.i, obar.i, prob.y, obar, main = "Sample Attribute Plot")  

## Function will work with a ``prob.bin'' class objects as well.
## Note this is a random forecast.
obs<- round(runif(100))
pred<- runif(100)

A<- verify(obs, pred, frcst.type = "prob", obs.type = "binary")
attribute(A, main = "Alternative plot", xlab = "Alternate x label" )
## to add a line from another model
obs<- round(runif(100))
pred<- runif(100)

B<- verify(obs, pred, frcst.type = "prob", obs.type = "binary")
lines.attrib(B, col = "green")


## Same with confidence intervals
attribute(A, main = "Alternative plot", xlab = "Alternate x label", CI =
TRUE)

#### add lines to plot
data(pop)
d <- pop.convert()
## internal function used to
## make binary observations for
## the pop figure.

### note the use of bins = FALSE
mod24 <- verify(d$obs_rain, d$p24_rain,
    bins = FALSE)

mod48 <- verify(d$obs_rain, d$p48_rain,
    bins = FALSE)
plot(mod24, freq = FALSE)

lines.attrib(mod48, col = "green",
    lwd = 2, type = "b")

}

\references{
  Hsu, W. R., and A. H. Murphy, 1986: The attributes diagram: A geometrical framework for assessing the quality of probability forecasts.  \emph{Int. J. Forecasting} \bold{2}, 285--293.
  
  Wilks, D. S. (2005) \emph{Statistical Methods in the Atmospheric Sciences } Chapter 7, San Diego: Academic Press.
}
\author{Matt Pocernich}

\seealso{\code{\link{verify}}  \code{\link{reliability.plot}}}

       \keyword{file}
