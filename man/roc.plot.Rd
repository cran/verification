% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2004 
% ** University Corporation for Atmospheric Research(UCAR) 
% ** National Center for Atmospheric Research(NCAR) 
% ** Research Applications Program(RAP) 
% ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
% ** 2004/1/7 11:31:8 
% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
\name{roc.plot}
       \alias{roc.plot}
       \alias{roc.plot.default}
       \alias{roc.plot.prob.bin}
       \title{Relative operating characteristic curve.}
       \description{This function creates Receiver Operating
     Characteristic (ROC) plots for one or more models.  A ROC curve
       plots the false alarm rate against the hit rate for a
       probablistic forecast for a range of thresholds. The area under
       the curve is viewed as a measure of a forecast's accuracy.  A
       measure of 1 would indicate a perfect model.  A measure of 0.5
       would indicate a random forecast. }
       \usage{
    \method{roc.plot}{default}(x, pred, thresholds = NULL, binormal =
FALSE,   legend = FALSE, leg.text = NULL,  plot = "emp", CI = FALSE,
n.boot = 1000, alpha = 0.05, tck = 0.01, plot.thres = seq(0,
1, 0.1), show.thres = TRUE, main = "ROC Curve", xlab = "False Alarm Rate", ylab = "Hit Rate", extra = FALSE,  \dots)
\method{roc.plot}{prob.bin}(x, \dots) }
\arguments{
        \item{x}{A binary observation (coded \{0, 1 \} ) or a verification
          object.}
         \item{pred}{A probability prediction on the interval [0,1].  If
       multiple models are compared, this may be a matrix where each
           column represents a different prediction.}
    \item{thresholds}{Thresholds may be provided. These thresholds
      will be used to calculate the hit rate ($h$) and false alarm
      rate ($f$).  If thresholds is NULL, all unique thresholds are
      used as a threshold. Alternatively, if the number of bins is
      specified,  thresholds will be calculated using the
      specified numbers of quantiles. }
%        \item{all.thres}{Should all unique predictions be used as a
%     threshold? Defaults to FALSE.  By default, n.thres.bin will be
%   used by calculating percentiles for each threshold.  } 
    \item{binormal}{ If TRUE, in addition to the empirical ROC curve, the
  binormal ROC curve will be calculated.  To get a plot draw, plot must
  be either ``binorm'' or ``both''.  }
          \item{legend}{Binomial.  Defaults to FALSE indicating whether a legend
            should be displayed.}
          \item{leg.text}{Character vector for legend.  If NULL, models are
          labeled  ``Model A", ``Model B",...}
     \item{plot}{Either ``emp'' (default), ``binorm'' or ``both'' to
       determine which plot is shown.  If set to NULL, a plot is not
       created}
     \item{CI}{Confidence Intervals.  Calculated by bootstrapping
       the observations and prediction, then calculating PODy and
       PODn values.  }
    \item{n.boot}{Number of bootstrap samples.}
    \item{alpha}{ Confidence interval.  By default = 0.05}
    \item{tck}{Tick width on confidence interval whiskers.}
        \item{plot.thres}{By default, displays the threshold levels on
      the ROC diagrams.  To surpress these values, set it equal to
      NULL.  If confidence intervals (CI) is set to TRUE, levels
      specified here will determine where confidence interval boxes
      are placed. }
    \item{show.thres}{ Show thresholds for points indicated by
      plot.thres.  Defaults to TRUE.}
    \item{main}{Title for plot.}
        \item{xlab, ylab}{Plot axes labels.  Defaults to ``Hit Rate''
      and ``False Alarm Rate'', for the y and x axes respectively.} 
        \item{extra}{ Extra text describing binormal and empirical lines. }
    \item{\dots}{Additional plotting options.}
       }
       \value{
     If assigned to an object, the following values are reported.
     \item{plot.data}{The data used to generate the ROC plots.  This
       is a array. Column headers are thresholds, empirical hit and
       false alarm rates, and binormal hit and false alarm rates.  Each
       model is depicted on an array indexed by the third dimension.}
     \item{roc.vol}{The areas under the ROC curves.  By default,this
       is printed on the plots.  Areas and p-values are
       calculated with and without adjustments for ties along with
       the p-value for the area.  These
       values are calculated using \code{\link{roc.area}}.  The
       fifth column contains the area under the binormal curve, if
       binormal is selected.}
       }
       \note{There is not a minimum size required to create confidence
     limits or show thresholds.  When there are few data points, it
     is possilbe to make some pretty unattractive graphs. }

       \references{
Mason, I. (1982) ``A model for assessment of weather forecasts,''
\emph{Aust. Met. Mag} \bold{30} (1982) 291-303.  

     Mason, S.J. and N.E. Graham. (2002) ``Areas beneath the
   relative operating characteristics (ROC) and relative operating
   levels (ROL) curves: Statistical significance and interpretation, ''
   \emph{Q. J. R. Meteorol. Soc.} \bold{128} pp. 2145-2166.  
 
 Swets, John A. (1996) \emph{Signal Detection Theory and ROC Analysis
   in Psychology and Diagnostics}, Lawrence Erlbaum Associates, Inc.
       }
     \examples{
# Data from Mason and Graham article.

a<- c(0,0,0,1,1,1,0,1,1,0,0,0,0,1,1)
b<- c(.8, .8, 0, 1,1,.6, .4, .8, 0, 0, .2, 0, 0, 1,1)
c<- c(.928,.576, .008, .944, .832, .816, .136, .584, .032, .016, .28, .024, 0, .984, .952)

A<- data.frame(a,b,c)
names(A)<- c("event", "p1", "p2")

## for model with ties
roc.plot(A$event, A$p1)

## for model without ties
roc.plot(A$event, A$p2)

### show binormal curve fit.

roc.plot(A$event, A$p2, binormal = TRUE)

# icing forecast

data(prob.frcs.dat)
A <- verify(prob.frcs.dat$obs, prob.frcs.dat$frcst/100)
roc.plot(A, main = "AWG Forecast")


# plotting a ``prob.bin'' class object.
obs<- round(runif(100))
pred<- runif(100)

A<- verify(obs, pred, frcst.type = "prob", obs.type = "binary")

roc.plot(A, main = "Test 1", binormal = TRUE, plot = "both")

## show confidence intervals.  MAY BE SLOW
roc.plot(A, threshold = seq(0.1,0.9, 0.1), main = "Test 1", CI = TRUE,
alpha = 0.1)
}
\author{Matt Pocernich <pocernic@rap.ucar.edu>}

       \keyword{file}
