%% Contributed by  Author: Ronald Frenette, Severe Weather Lab, Quebec region
%% #         Jun 2009
  
\name{rcrv}
       \alias{rcrv}
       \title{Reduced centered random variable}
       \description{The RCRV provides information on the reliability of an ensemble system
in terms of the bias and the dispersion. A perfectly reliable system as no bias and a 
dispersion equal to 1. The observational error is taken into account }
       \usage{
      rcrv(obs,epsMean,epsVariance,obsError)
       }
      
      \arguments{
	 \item{obs}{A vector of observations}
	 \item{epsMean}{A vector of the means of the ensemble}
     \item{epsVariance}{A vector of the variances of the ensemble}
	 \item{obsError}{Observational error}
       }
       \value{
	 \item{bias}{The weighted bias between the ensemble and the observation.  A value equal to 0 indicates no bias.  A positive (negative) value indicates a positive (negative) bias}
	 \item{disp}{The dispersion of the ensemble.
                     A value equal to 1 indicates no dispersion.
                     A value greater (smaller) then 1 indicates underdispersion (overdispersion)}
	 \item{y}{Vector of y. Mean of y equals bias and standard deviation of y equals dispersion}
	 \item{obsError}{Observational error (passed to function)}
       }
 \examples{

data(precip.ensemble) 
#Observations are in the column
obs<-precip.ensemble[,3] 

#Forecast values of ensemble are in the column 4 to 54
eps<-precip.ensemble[,4:54]   

#Means and variances of the ensemble
mean<-apply(eps,1,mean)
var<-apply(eps,1,var)

#observation error of 0.5mm 
sig0 <- 0.5 

rcrv(obs,mean,var,sig0)

}


\references{
       G. Candille, C. P. L. Houtekamer, and G. Pellerin: Verification of an Ensemble Prediction System against Observations, \emph{Monthly Weather Review},\bold{135}, pp. 2688-2699
	}

\author{Ronald Frenette <Ronald.Frenette@ec.gc.ca>}