% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2004 
% ** University Corporation for Atmospheric Research(UCAR) 
% ** National Center for Atmospheric Research(NCAR) 
% ** Research Applications Program(RAP) 
% ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
% ** 2004/1/7 11:31:8 
% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
\name{leps}
       \alias{leps}
       \alias{leps.default}
       \title{Linear Error in Probability Space (LEPS) }
       \description{Calculates the linear error in probability spaces.
	 This is the mean absolute difference between the forecast
	 cumulative distribution value (cdf) and the observation.  This
	 function creates the empirical cdf function for the observations
	 using the sample population.  Linear interpretation is used to
	 estimate the cdf values between observation values.  Therefore;
	 this may produce awkward results with small datasets.}
       \usage{
    \method{leps}{default}(x, pred, plot = TRUE, \dots )
                         }
\arguments{
         \item{x}{A vector of observations or a verification object with ``cont.cont'' properties.}
	 \item{pred}{A vector of predictions.}  
	 \item{plot}{Logical to generate a plot or not.}
	 \item{\dots}{Additional plotting options.}
       }
       \value{
	 If assigned to an object, the following values are reported.
	 \item{leps.0}{Negatively oriented score on the [0,1] scale, where 0
	   is a perfect score.}
	 \item{leps.1}{Positively oriented score proposed by Potts.}
	 }

       \references{
DeQue, Michel. (2003) ``Continuous Variables'' \bold{Chapter 5},
\emph{Forecast Verification: A Practitioner's Guide in Atmospheric
  Science.}

Potts, J. M., Folland, C.K., Jolliffe, I.T. and Secton, D. (1996)
``Revised `LEPS' scores fore assessing climate model simulations and
long-range forecasts.'' \emph{J. Climate}, \bold{9}, pp. 34-54.

%Palmer, T.N., Brankovic, C. and Richadson, D. S. (2000), ``A probability
%and decision-model analysis of PROVOST seasonal multi-model
	}
       
 	\examples{
 obs <- rnorm(100, mean = 1, sd = sqrt(50))
 pred<-  rnorm(100, mean = 10, sd = sqrt(500))

 leps(obs, pred) 

 }
\author{Matt Pocernich <pocernic@rap.ucar.edu>}

       \keyword{file}
