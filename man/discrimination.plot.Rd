% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2004 
% ** University Corporation for Atmospheric Research(UCAR) 
% ** National Center for Atmospheric Research(NCAR) 
% ** Research Applications Program(RAP) 
% ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
% ** 2004/1/7 11:31:8 
% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
\name{discrimination.plot}
\alias{discrimination.plot}
\title{Discrimination  plot}
       \description{This function creates a plot of overlayed
	 histograms.   In the context of verification, this is often
	 used to compare the distribution of event and no-event forecasts.}
       \usage{discrimination.plot(obs, pred, breaks = 11, main = "Comparison of Distributions", xlim = c(0,1), median = TRUE, \dots }
}

\arguments{
          \item{obs}{A binary observation (coded \{0, 1 \} ).}
	 \item{pred}{A vector of predictions.}
	 \item{breaks}{Number of breaks in the x-axis of the histogram.}
	 \item{main}{Title for plot.}
	 \item{xlim}{Range of histogram - x axis.}
	 \item{median}{If TRUE, the median values for the event and the
	   no-event will be vertically plotted.  This is intended as a
	   non-paramtric indicator of the forecast's ability to
	   discriminate.  If FALSE, these vertical lines
	   will be surpressed.}
	 \item{\dots}{Additional plotting options.}
       }

        \examples{
 #  A sample forecast.  

 a<- rnorm(100, mean = -1); b<- rnorm(100, mean = 1)

 A<- cbind(rep(0,100), pnorm(a)); B<- cbind(rep(1,100), pnorm(b)) 

 dat<- rbind(A,B); dat<- as.data.frame(dat)
 names(dat)<- c("obs", "pred")

 discrimination.plot(dat$obs, dat$pred)
 }

\author{Matt Pocernich <pocernic@rap.ucar.edu>}

       \keyword{file}
