% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2004 
% ** University Corporation for Atmospheric Research(UCAR) 
% ** National Center for Atmospheric Research(NCAR) 
% ** Research Applications Program(RAP) 
% ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
% ** 2004/1/7 11:31:8 
% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
\name{int.scale.verify}
       \alias{int.scale.verify}
       \title{Intensity-Scale Verification Model }
       \description{For a spatial forecast, evaluates the forecast skill
	 as a function of precipitaion rate intesity and spatial scale of the error.}
       \usage{
    int.scale.verify(frcs, obs, thres = quantile(frcs, p = seq(0,0.9,0.1)),  ... )
       }
      \arguments{
	\item{frcs}{Forecast matrix.  Must be of $2^n$ dimensions.}
	\item{obs}{Observation matrix. Must be of $2^n$ dimensions.}
	\item{thres}{A vector of thresholds to be considered.  By
	  default, the percentiles 0, 90 are used. }
	\item{\dots}{Optional arguments may be passed to the image plot}
       }
       \value{
	 \item{SSul}{Skill score as matrix.  The rownames are the
	   thresholds, the colnames are $n$ where $2^n$ is the spatial
	   scale of the skill score decomposition.}
	 \item{MSE}{A matrix with the mean squared error of the
	   forecast}
	 \item{l.frcs}{Number of rows in forecast.  Used in plotting
	   routine.}
	 \item{thres}{Thresholds used in model}
             }  

\note{ This function creates an image plot of the intensity plot of the
  skill scores as a function of spatial scale and threshold.  The top
  row is equivalent to the bias of the forecast.}
\examples{

## simulated example
n<- 5
set.seed(10)
forecast1 <- matrix( log(rlnorm(n = (2^n *2^n) )) , nrow = 2^n)
obs1      <- matrix(log( rlnorm(n = (2^n *2^n) )) , nrow = 2^n)
int.scale.verify(forecast1, obs1, main = "Test Case")  

## real example.  Data source referenced below.


data(analysis.dat)
data(forecast.dat)

require(waveslim)
require(fields)


A<- int.scale.verify(forecast.dat, analysis.dat,
thres = c(0, 2^seq(-5,6)),  main = "NIMROD example" )

plot(A)

}

\seealso{\url{http://www.met.rdg.ac.uk/~swr00bc/} }
\references{
B.Casati, D.B. Stephenson, G. Ross. \emph{A new intensity-scale
  approach for the verification of spatial precipitation forecasts}.
Meteorological Application (RMS), in press. }

\author{Barabara Casati <barbara.casati  (at) ec.gc.ca>}

       \keyword{file}
