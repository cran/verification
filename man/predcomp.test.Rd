\name{predcomp.test}
\alias{predcomp.test}
\alias{losserr}
\alias{exponentialACV}
\alias{summary.predcomp.test}
\title{
Time Series Prediction Comparison Test
}
\description{
Forecast prediction comparison test for two competing forecasts against an observation.
}
\usage{
predcomp.test(x, xhat1, xhat2, alternative = c("two.sided", "less", "greater"),
    lossfun = "losserr", lossfun.args = NULL,
    fitmodel = NULL, fitmodel.args = NULL, ...)

losserr(x, xhat, method = c("abserr", "sqerr", "simple", "power", 
    "corrskill", "dtw"), scale = 1, p = 1, dtw.interr = c("abserr", 
    "sqerr", "simple", "power"), ...)

exponentialACV(x, y, ...)

\method{summary}{predcomp.test}(object, ...)
}
\arguments{
  \item{x,xhat1,xhat2,xhat}{numeric vectors giving the verification data and each competing forecast model output (1 and 2).  For \code{losserr}, \code{xhat} is a numeric giving a single forecast model output (i.e., by default the function is called internally by \code{predcomp.test} once for \code{xhat1} and once for \code{xhat2}). For \code{exponentialACV}, see argument \code{y} below.}
  \item{y}{
   \code{x} for \code{exponentialACV} is a numeric giving the separation distance, and \code{y} a numeric giving the autocovariance values.
}
  \item{object}{list object of class \dQuote{predcomp.test} as returned by \code{predcomp.test}.}
  \item{alternative}{character string stating which type of hypothesis test to conduct.}
  \item{lossfun}{character string naming the loss function to call.  The default, \code{losserr}, calls one of several methods depending on its \code{method} argument.  Any function that takes \code{x} and \code{xhat} numeric vectors as arguments and returns a numeric vector of the same length can be used.}
  \item{lossfun.args}{List providing additional arguments to \code{lossfun}.}
  \item{fitmodel}{character string naming a function such as \code{exponentialACV}.

Should only be used if the method of Hering and Genton (2011) is to be employed.

This is a function for fitting a parameteric covariance to the empirical autocovariances.  For example, \code{exponentialACV} can be used to fit the exponential model used in Hering and Genton (2011).

If NULL, then the Diebold-Mariano test is employed instead (though, the user may want to use \code{dm.test} from \pkg{forecast} for this purpose instead).

This function must take \code{x} and \code{y} as arguments where \code{x} are the separation distances (lags) and \code{y} are the associated autocovariances as well as \code{\dots}, but other arguments are also allowed.  It should return an object with a class for \code{coef} whose first coefficient is C(0), and must have a method for \code{predict}; for example, an \code{nls} object.
}
  \item{fitmodel.args}{list object giving any additional arguments required by the function given by \code{fitmodel}.}
  \item{method,dtw.interr}{character string stating which type of loss (or skill) function to use.  In the case of \code{dtw.interr}, this is the loss function for the intensity part of the error only.}
   \item{scale}{numeric giving a value by which to scale the loss function.  In the case of \dQuote{\code{dtw}}, this is only applied to the intensity part of the loss function, and can be used to scale the influence of the intensity vs. temporal lag errors.  See Details section for more.}
   \item{p}{numeric only used by the \dQuote{power} loss function.}
   \item{\dots}{For \code{predcomp.test}, these are any additional arguments to the \code{acf} function besides \code{x}, \code{type} and \code{plot}, which may not be passed.

For \code{losserr}, these are any additional arguments to \code{dtw} except for \code{x}, \code{y}, and \code{step.pattern}, which may not be passed.

For \code{exponentialACV} these are any optional arguments to \code{nls} except for \code{formula} and \code{data}.  If \code{start} is not passed, then reasonable starting values are calculated and passed in for this argument.

For the \code{summary} method function, these are not used.}
}
\details{
This function performs the analyses described in Gilleland and Roux (2014).  Namely, the Diebold Mariano test for competing forecast performance, the Hering and Genton (2011) modification of this test, as well as the dynamic time warping extension.

The Diebold-Mariano test was proposed in Diebold and Mariano (1995) for obtaining hypothesis tests to compare the forecast accuracy of two competing forecasts against a common verification series.  The null hypothesis is that they have the same accuracy.  The test statistic is of the form

S = dbar/sqrt(2*pi*se_{d}(0)/N),

where d is the loss differential, d = e1 - e2 (e1 = loss(x, xhat1) and e2 = loss(x, xhat2)), dbar is its sample mean, and se_{d}(0) is the standard error for d, which must be estimated, and N is the length of the series investigated.  Let V = 2*pi*se_{d}(0), then V is estimated by

V = sum(gamma(tau)),

where the summation is over tau = -(k - 1) to (k - 1) for temporal lags k, and gamma are the empirical autocovariances.  

Hering and Genton (2011) propose a modification that employs fitting a parameteric covariance model in determining the standard error for the test statistic (they also propose a spatial extension, see, e.g., \code{spatMLD} from \pkg{SpatialVx}).  If \code{fitmodel} is not NULL, then this is the method applied, and the resulting standard error is calculating by summing up the values of this function up to half the maximum lag.

In either case, asymptotic results suggest that S ~ N(0,1), and the hypothesis test is conducted subsequently.

Discrete time warping can be applied (see examples below) in order to obtain a loss function based on location (in time) and intensity errors similar to the spatial version in Gilleland (2013).

The loss functions supplied by \code{losserr} include:

\code{abserr}: Absolute error loss, defined by abs((xhat - x)/scale),

\code{sqerr}: Square error loss, defined by ((xhat - x)/scale)^2,

\code{simple}: Simple loss, defined by (xhat - x)/scale,

\code{power}: Power loss, defined by ((xhat - x)/scale)^p (same as \code{sqerr} if \code{p}=2),

\code{corrskill}: Correlation skill defined by scale * (x - mean(x)) * (xhat - mean(xhat)),

\code{dtw}: Discrete time warp loss defined by: d1 + d2, where d1 is the absolute distance (ignoring direction) of warp movement, and d2 is one of the above loss functions (except for \code{corrskill}) applied to the resulting intensity errors after warping the series.

The \code{exponentialACV} function takes numeric vector arguments \code{x} and \code{y} and estimates the parameters, \code{c(sigma, theta)}, that optimize

    y = sigma^2*exp(-3*x/theta)

using \code{nls}.  If \code{fitmodel} = \dQuote{\code{exponentialACV}}, then this exponential function is fit to the data set where \code{x} are lags and \code{y} are empirical autocovariances (found by a call to \code{acf}).
}
\value{
\code{predcomp.test} returns a list object of class c(\dQuote{predcomp.test}, \dQuote{htest}) with components:
  \item{call }{the calling string}
  \item{method }{character string giving the full name of the method (Diebold-Mariano or Hering-Genton) used.}
  \item{fitmodel}{character naming the function used to fit the parametric model to the autocovariances or \dQuote{none}.}
  \item{fitmodel.args}{If fitmodel is used, then this will be a list of any arguments passed in for it.}
  \item{loss.function}{character string naming the loss function called.}
  \item{statistic}{numeric giving the value of the statistic.}
  \item{alternative}{character string naming which type of hypothesis test was used (i.e., two-sided or one of the one-sided possibilities).}
  \item{p.value}{numeric giving the p-value for the test.}
  \item{data.name}{character vector naming the verification and competing forecast series applied to the test.}

\code{losserr} returns a numeric vector of loss values.

\code{exponentialACV} returns a list object of class \dQuote{nls} as returned by \code{nls}.
}
\references{
Diebold, F. X. and Mariano, R. S. (1995) Comparing predictive accuracy. \emph{Journal of Business and Economic Statistics}, \bold{13}, 253--263.

Gilleland, E. (2013) Testing competing precipitation forecasts accurately and efficiently: The spatial prediction comparison test. \emph{Mon. Wea. Rev.}, \bold{141} (1), 340--355, \url{http://dx.doi.org/10.1175/MWR-D-12-00155.1}.

Gilleland, E. and Roux, G. (2014) A New Approach to Testing Forecast Predictive Accuracy. Submitted to \emph{Meteorol. Appl.} Temporarily available at: \url{http://www.ral.ucar.edu/~ericg/GillelandRoux_metapps.pdf}

Hering, A. S. and Genton, M. G. (2011) Comparing spatial predictions.  \emph{Technometrics}, \bold{53}, (4), 414--425, doi:10.1198/TECH.2011.10136.
}
\author{
Eric Gilleland
}

\seealso{
\code{\link{print.htest}}, \code{\link{nls}}, \code{\link{dtw}}, \code{\link{acf}}
}
\examples{
z0 <- arima.sim(list(order=c(2,0,0), ar=c(0.8,-0.2)), n=1000)
z1 <- c(z0[10:1000], z0[1:9]) + rnorm(1000, sd=0.5)
z2 <- arima.sim(list(order=c(3,0,1), ar=c(0.7,0,-0.1), ma=0.1), n=1000) + 
    abs(rnorm(1000, mean=1.25))

test <- predcomp.test(z0, z1, z2)
summary(test)

\dontrun{
test2 <- predcomp.test(z0, z1, z2, fitmodel="exponentialACV")
summary(test2)

test2.2 <- predcomp.test(z0, z1, z2, alternative="less", fitmodel="exponentialACV")
summary(test2.2)

test3 <- predcomp.test(z0, z1, z2, lossfun.args=list(method="dtw"), fitmodel="exponentialACV")
summary(test3)

test3.2 <- predcomp.test(z0, z1, z2, alternative="less", lossfun.args=list(method="dtw"),
    fitmodel="exponentialACV")
summary(test3.2)

test4 <- predcomp.test(z0, z1, z2, lossfun.args=list(method="corrskill"),
    fitmodel="exponentialACV")
summary(test4)

test5 <- predcomp.test(z0, z1, z2, lossfun.args=list(method="dtw", dtw.interr="sqerr"),
    fitmodel="exponentialACV")
summary(test5)

test5.2 <- predcomp.test(z0, z1, z2, alternative="less",
    lossfun.args=list(method="dtw", dtw.interr="sqerr"), fitmodel="exponentialACV")
summary(test5.2) 
}

}
\keyword{ htest }
