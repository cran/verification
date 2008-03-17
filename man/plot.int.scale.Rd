% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2004 
% ** University Corporation for Atmospheric Research(UCAR) 
% ** National Center for Atmospheric Research(NCAR) 
% ** Research Applications Program(RAP) 
% ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
% ** 2004/1/7 11:31:8 
% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
\name{plot.int.scale}
       \alias{plot.int.scale}
       \title{Plot Intensity Scale Objects.}
       \description{Plots objects from the
	 \code{\link{int.scale.verify}}  using the \code{image} function
	 from the base package and \code{image.plot} from the fields package..}
       \usage{
     \method{plot}{int.scale}(x, y = NULL, plot.mse = FALSE, main = NULL, \dots)
       }
      \arguments{
	\item{x}{A object from the \code{int.scale.verify} function that
	  has the class int.scale.}
	\item{y}{NULL}
	\item{plot.mse}{Should a plot be created of the mean squared
	  errors?  By default the skill scores are plotted.}
	\item{main}{Plot title}  
	\item{\dots}{Optional arguments}
      }
      
       \seealso{\code{\link{int.scale.verify}},
	 \code{\link[base]{image}}, 
	 \code{\link[fields]{image.plot} } } 
       \examples{
data(analysis.dat)
data(forecast.dat)

A<- int.scale.verify(forecast.dat, analysis.dat, thres = c(0, 2^seq(-5,6)))
plot(A)
plot(A, plot.mse = TRUE)
plot(A, main = "Test case")
}


\author{Matt Pocernich <pocernic@rap.ucar.edu>}

       \keyword{file}
