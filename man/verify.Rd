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
       used in further analysis or with other methods such as plot and summary.}
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
       c(n11, n01, n10, n00).
       If  obs is a matrix, it is assumed to be a contingency table with observed values summarized in the columns and forecasted values summarized in the rows.
        }
     \item{pred}{Prediction of event.  The prediction may be in the
       form of the a point prediction or the probability of a
       forecast.  Let pred = NULL if obs is a contingency
       table. }
     \item{baseline}{In meteorology, climatology is the baseline
       that represents the
       no-skill forecast.  In other fields this field would
       differ.  This field is used to calculate certain skill
       scores.  If left NULL, these statistics are calculated using
       sample climatology.  If this is not NULL, the mean of these values is used as the baseline forecast.  This interpretation is not appropriate for all applications.  For example, if a baseline forecast is different for each forecast this will not work appropriately.  }
%    \item{sample.baseline}{Logical indicating whether the data
%      provided is a representative sample of the baseline climatology or not.}
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
	\details{ See Wilks (2006) and the WMO Joint WWRP/WGNE Working Group web site on verification for more details about these verification statistics.  See Stephenson et al. (2008) and Ferro and Stephenson (2011) for more on the extreme dependence scores and indices.  For information on confidence intervals for these scores, see Gilleland (2010).}
       \note{ 
       	There are other packages in R and Bioconductor which are usefull for verification tasks.  This includes the \bold{ROCR}, \bold{ROC}, package and the "limma" package (in the Bioconductor repository.)  Written by people in different fields, each provides tools for verification from  different perspectives.
       	
       	For the categorical forecast and verification, the Gerrity
     score only makes sense for forecast that have order, or are
     basically ordinal. It is assumed that the forecasts are listed
     in order.  For example, if the rows of a contigency table were summarized as "medium, low, high", the Gerrity score will be incorrectly summarized.}  
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
      \item{theta}{Odds Ratio (BB)}
     \item{log.theta}{Log Odds Ratio}
     \item{n.h}{Degrees of freedom for log.theta (BB)}
     \item{orss}{Odds ratio skill score, aka Yules's Q (BB)}
     \item{eds}{Extreme Dependency Score (BB)}
     \item{eds.se}{Standard Error for Extreme Dependence Score (BB)}
     \item{seds}{Symmetric Extreme Dependency Score (BB)}
     \item{seds.se}{Standard Error for Symmetric Extreme Dependency Score (BB)}
     \item{EDI}{Extremal Dependence Index (BB)}
     \item{EDI.se}{Standard Error for Extremal Dependence Index (BB)}
     \item{SEDI}{Symmetric Extremal Dependence Index (BB)}
     \item{SEDI.se}{Standard Error for Symmetric Extremal Dependence Index (BB)}
    }
      \seealso{\code{table.stats} }
      \references{
  Ferro, C. A. T. and D. B. Stephenson, 2011.  Extremal dependence indices: Improved verification measures for deterministic forecasts of rare binary events.  \emph{Wea. Forecasting}, \bold{26}, 699 - 713.

  Gilleland, E., 2010. Confidence intervals for forecast verification. NCAR Technical Note NCAR/TN-479+STR, 71pp.  Available at: http://nldr.library.ucar.edu/collections/technotes/asset-000-000-000-846.pdf
 
  Stephenson, D. B., B. Casati, C. A. T. Ferro, and C. A. Wilson, 2008.  The extreme dependency score: A non-vanishing measure for forecasts of rare events.  \emph{Meteor. Appl.}, \bold{15}, 41 - 50.
 
  Wilks, D. S., 2006. \emph{Statistical Methods in the Atmospheric Sciences }, San Diego: Academic Press., 627 pp. (2nd Editiion).

  WMO Joint WWRP/WGNE Working Group on Verification Website
  
  \url{http://www.cawcr.gov.au/projects/verification/}
}
     \examples{
# binary/binary example
obs<- round(runif(100))
pred<- round(runif(100))

# binary/binary example
# Finley tornado data.

obs<- c(28, 72, 23, 2680)
A<- verify(obs, pred = NULL, frcst.type = "binary", obs.type = "binary")

summary(A)

# categorical/categorical example
# creates a simulated 5 category forecast and observation.
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

\author{Matt Pocernich <pocernic@ucar.edu>}

       \keyword{file}
