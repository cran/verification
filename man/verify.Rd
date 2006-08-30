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
    verify(obs, pred, baseline = NULL, % sample.baseline = FALSE,
           frcst.type = "prob", obs.type = "binary",  thresholds =
seq(0,1,0.1), show = TRUE, bins = TRUE, \dots )
       }
       \arguments{
     \item{obs}{The values with which the verifications are
       verified. May be a vector of length 4 if the forecast and
       predictions are binary data summarized in a contingency
       table.  In this case, the value are entered in the order of
       c(n11, n01, n10, n00). }
     \item{pred}{Prediction of event.  The prediction may be in the
       form of the a point prediction or the probability of a
       forecast.  Let pred = NULL if obs represents a contingency
       table. }
     \item{baseline}{In meteorology, climatology is the baseline
       that represents the
       no-skill forecast.  In other fields this field would
       differ.  This field is used to calculate certain skill
       scores.  If left NULL, these statistics are calculated using
       sample climatology.}
%    \item{sample.baseline}{Logical indicating whether the data
%      provided is sample baselinetology or not.}
     \item{frcst.type}{Forecast type.  Either "prob", "binary",
       "norm.dist", "cat" or "cont".  Defaults to "prob".
       "norm.dist" is used when the forecast is in the form of a
       normal distribution.  See crps for more details. }
     \item{obs.type}{Observation type.  Either  "binary", "cat" or
       "cont".  Defaults to "binary"}
     \item{thresholds}{Thresholds to be considered for point
       forecasts of continuous events.}
     \item{show}{Binary; if TRUE (the default), print warning message}
     \item{bins}{Binary; if TRUE (default), the probabilistic forecasts
       are placed in bins defined by the sequence defined in threshold
       and assigned the midpoint value.}
     \item{\dots}{Additional options.}
       }
       \note{ For the categorical forecast and verification, the Gerrity
     score only makes sense for forecast that have order, or are
     basically ordinal. It is assumed that the forecasts are listed
     in order.  For example, low, medium and high would get
     translated into 1,2 and 3.}  
       \value{
     An object of the verify class.  Depending on the type of
      data used, the following information may be returned.  The
      following notation is used to describe which values are
      produced for which type of forecast/observations. (BB =
      binary/binary, PB = probablistic/binary, CC =
      continuous/continuous, CTCT = categorical/categorical)   

      \item{BS}{Brier Score (PB)}
           \item{BSS}{Brier Skill Score(PB)}
      \item{SS}{Skill Score (BB)}
      \item{hit.rate}{Hit rate, aka PODy, $h$ (PB, CTCT)}
      \item{false.alarm.rate}{False alarm rate, PODn,
        $f$ (PB, CTCT)}
      \item{TS}{Threat Score or Critical Success Index (CSI)(BB, CTCT) }
          \item{ETS}{Equitable Threat Score (BB, CTCT)}
      \item{BIAS}{Bias (BB, CTCT) }
      \item{PC}{Percent correct or hit rate (BB, CTCT)}
      \item{Cont.Table}{Contingency Table (BB)}
          \item{HSS}{Heidke Skill Score(BB, CTCT) }
      \item{KSS}{Kuniper Skill Score (BB)}
      \item{PSS}{Pierce Skill Score (CTCT) }
      \item{GS}{Gerrity Score (CTCT) }
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
}
     \examples{
# binary/binary example
obs<- round(runif(100))
pred<- round(runif(100))

# binary/binary Finley tornado data.

obs<- c(28, 72, 23, 2680)
A<- verify(obs, pred = NULL, frcst.type = "binary", obs.type = "binary")


summary(A)

# categorical/categorical example

obs <- round(runif(100, 1,5) )
pred <- round(runif(100, 1,5) )

A<- verify(obs, pred, frcst.type = "cat", obs.type = "cat" )
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
