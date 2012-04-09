% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2004 
% ** University Corporation for Atmospheric Research(UCAR) 
% ** National Center for Atmospheric Research(NCAR) 
% ** Research Applications Program(RAP) 
% ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
% ** 2004/1/7 11:31:8 
% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
    \name{reliability.plot}
    \alias{reliability.plot}
    \alias{reliability.plot.default}
    \alias{reliability.plot.verify}

       \title{Reliability Plot}
       \description{A reliability plot is a simple form of an attribute
	 diagram that depicts the performance of a probabilistic forecast
       for a binary event.  In this diagram, the forecast probability is
       plotted against the observed relative frequency.  Ideally, this
       value should be near to each other and so points falling on the
       1:1 line are desirable.  For added information, if one or two
       forecasts are being verified, sharpness diagrams are presented in
       the corners of the plot.  Ideally, these histograms should be
       relatively flat, indicating that each bin of probabilities is use
       an appropriate amount of times.  }
       \usage{
   \method{reliability.plot}{default}(x, obar.i, prob.y, titl = NULL, legend.names = NULL, \dots )
\method{reliability.plot}{verify}(x, \dots)
}
\arguments{
         \item{x}{Forecast probabilities.(\eqn{y_i}) or a ``prob.bin''
	   class object from \code{verify}.}
	 \item{obar.i}{Observed relative frequency \eqn{\bar{o}_i}.}
      	 \item{prob.y}{Relative frequency of forecasts}
	 \item{titl}{Title}
	 \item{legend.names}{Names of each model that will appear in the
	   legend.  }
	 \item{\dots}{Graphical parameters.}
	 }
 	 \details{This function works either by entering vectors or
	   on a verify class object.}
	 \note{If a single prob.bin class object is used, a reliability
	   plot along with a sharpness diagram is displayed.  If two
	   forecasts are provided in the form of a matrix of predictions,
	   two sharpness diagrams are provided.  If more forecasts are
	   provided, the sharpness diagrams are not displayed.}
	 \examples{
## Data from Wilks, table 7.3 page 246.
 y.i   <- c(0,0.05, seq(0.1, 1, 0.1))
 obar.i <- c(0.006, 0.019, 0.059, 0.15, 0.277, 0.377, 0.511, 0.587, 0.723, 0.779, 0.934, 0.933)
 prob.y<- c(0.4112, 0.0671, 0.1833, 0.0986, 0.0616, 0.0366, 0.0303,  0.0275, 0.245, 0.022, 0.017, 0.203) 
 obar<- 0.162

reliability.plot(y.i, obar.i, prob.y, titl = "Test 1", legend.names =
c("Model A") )


## Function will work with a ``prob.bin'' class object as well.
## Note this is a very bad forecast.
obs<- round(runif(100))
pred<- runif(100)

A<- verify(obs, pred, frcst.type = "prob", obs.type = "binary")

reliability.plot(A, titl = "Alternative plot")
 

}
\references{
  Wilks, D. S. (1995) \emph{Statistical Methods in the Atmospheric Sciences }
  Chapter 7, San Diego: Academic Press.
}
\author{Matt Pocernich}

       \keyword{file}
