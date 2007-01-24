% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2004 
% ** University Corporation for Atmospheric Research(UCAR) 
% ** National Center for Atmospheric Research(NCAR) 
% ** Research Applications Program(RAP) 
% ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
% ** 2004/1/7 11:31:8 
% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
\name{value}
       \alias{value}
       \title{Forecast Value Function}
       \description{Calculates the economic value of a forecast based on
       a cost/loss ratio.}
     \usage{value(obs, pred= NULL, baseline = NULL, cl = seq(0.05, 0.95, 0.05), plot = TRUE, all = FALSE, thresholds = seq(0.05, 0.95, 0.05), ylim = c(-0.05, 1), xlim = c(0,1), ...)}  
\arguments{
         \item{obs}{A vector of binary observations or a contingency
       table summary of values in the form c(n11, n01, n10, n00)
       where in  nab a = obs, b = forecast.} 
     \item{pred}{A vector of probabilistic predictions.}  
     \item{baseline}{Baseline  or naive forecast.  Typically climatology.}
     \item{cl}{Cost loss ratio.  The relative value of being
       unprepared and taking a loss to that of un-necessarily
       preparing.  For example,  cl = 0.1 indicates it
       would cost \$ 1 to prevent a \$10 loss.  This defaults to the
       sequence 0.05 to 0.95  by 0.05.  }
     \item{plot}{ Should a plot be created? Default is TRUE}
     \item{all}{In the case of probabilistic forecasts, should value
       curves for each thresholds be displayed.}
     \item{thresholds}{Thresholds considered for a probabilistic
       forecast.}
     \item{ylim, xlim}{ Plotting options.}
     \item{\dots}{Options to be passed into the plotting function.}
       }
       \value{
     If assigned to an object, the following values are reported.
     \item{vmax}{Maximum value}
     \item{V}{Vector of values for each cl value}
     \item{F}{Conditional false alarm rate.}
     \item{H}{Conditional hit rate}
     \item{cl}{Vector of cost lost ratios.}
     \item{s}{Base rate}
         }

       \references{
Jolliffe, Ian and David B. Stephensen (2003) \emph{Forecast
  Verification: A Practioner's Guide in Atmospheric Science}, Chapter
8. Wiley
}
       
\examples{
## value as a contingency table
## Finley tornado data
obs<- c(28, 72, 23, 2680) 
value(obs)
aa <- value(obs)
aa$Vmax # max value

## probabilistic forecast example
 obs  <- round(runif(100) )
 pred <-  runif(100)

value(obs, pred, main = "Sample Plot",
             thresholds = seq(0.02, 0.98, 0.02) ) 
##########
data(pop)
pop.convert()

value(obs = d$obs_rain, pred = d$p24_rain, all = TRUE)

 }
\author{Matt Pocernich <pocernic@rap.ucar.edu>}

       \keyword{file}
