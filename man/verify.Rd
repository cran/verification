% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2004 
% ** University Corporation for Atmospheric Research(UCAR) 
% ** National Center for Atmospheric Research(NCAR) 
% ** Research Applications Program(RAP) 
% ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
% ** 2004/1/7 11:31:8 
% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
\name{verify}
       \alias{verify}
       \title{Verification function}
       \description{Based on the type of inputs, this function
	 calculates a range of verification statistics and skill scores.
       Additionally, it creates a verify  class object that can be
       further analyzed.}
       \usage{
    verify(obs, pred,  tseries= NULL, baseline = NULL, % sample.baseline = FALSE,
           frcst.type = "prob", obs.type = "binary",  thresholds = seq(0,1,0.1) )
       }
       \arguments{
	 \item{obs}{The value with which the verifications are verified.}
	 \item{pred}{Prediction of event.  The prediction may be in the form of the a point
	   prediction or the probability of a forecast.}
	 \item{tseries}{Optional time series vector.  This allows
	   certain types of plots to be made.}
	 \item{baseline}{In meteorology, climatology is the baseline
	   that represents the
	   no-skill forecast.  In other fields this field would
	   differ.  This field is used to calculate certain skill
	   scores.  If left NULL, these statistics are calculated using
	   sample climatology.}
%	 \item{sample.baseline}{Logical indicating whether the data
%	   provided is sample baselinetology or not.}
	 \item{frcst.type}{Forecast type.  Either "prob", "binary", or
	   "cont".  Defaults to "prob"}
	 \item{obs.type}{Observation type.  Either  "binary", or
	   "cont".  Defaults to "binary"}
	 \item{thresholds}{Thresholds to be considered for point
	   forecasts of continuous events.}
       }
       \value{
	 An object of the verify class.  Depending on the type of
	  data used, the following information may be returned.  The
	  following notation is used to describe which values are
	  produced for which type of forecast/observations. (BB =
	  binary/binary, PB = probablistic/binary, CC = continuous/continuous.)   

	  \item{BS}{Brier Score (PB)}
           \item{BSS}{Brier Skill Score(PB)}
	  \item{SS}{Skill Score (BB)}
	  \item{hit.rate}{Hit rate, aka PODy, $h$ (PB)}
	  \item{false.alarm.rate}{False alarm rate, PODn,
	    $f$ (PB)}
	  \item{TS}{Threat Score or Critical Success Index (CSI)(BB) }
          \item{ETS}{Equitable Threat Score (BB)}
	  \item{BIAS}{Bias (BB) }
	  \item{PC}{Percent correct or hit rate (BB)}
	  \item{Cont.Table}{Contingency Table (BB)}
          \item{HSS}{Heidke Skill Score(BB) }
	  \item{KSS}{Kuniper Skill Score (BB)}
	  \item{ME}{Mean error (CC) }
	  \item{MSE}{Mean-squared error (CC)}
	  \item{MAE}{Mean absolute error (CC)}
	 
	}
      }
      \references{
	
  Wilks, D. S. (1995) \emph{Statistical Methods in the Atmospheric Sciences }
  Chapter 7, San Diego: Academic Press.

  WMO Joint WWRP/WGNE Working Group on Verification Website
  
  \url{http://www.bom.gov.au/bmrc/wefor/staff/eee/verif/verif_web_page.html}

World Meteorological Organization Verification Website
  
  \url{http://www.wmo.ch/web/aom/pwsp/Verification.html}
}
	 \examples{
# binary/binary example
obs<- round(runif(100))
pred<- round(runif(100))

A<- verify(obs, pred, frcst.type = "binary", obs.type = "binary")
summary(A)
#  probabilistic/ binary example
pred<- runif(100)
A<- verify(obs, pred, frcst.type = "prob", obs.type = "binary")
summary(A)
# continuous/ continuous example
obs<- rnorm(100)
pred<- rnorm(100)
baseline <- rnorm(100, sd = 0.5) 

A<- verify(obs, pred, baseline = baseline,  frcst.type = "cont", obs.type = "cont")
summary(A)
}

\author{Matt Pocernich <pocernic@rap.ucar.edu>}

       \keyword{file}
