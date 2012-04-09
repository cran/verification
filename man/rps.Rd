% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2004 
% ** University Corporation for Atmospheric Research(UCAR) 
% ** National Center for Atmospheric Research(NCAR) 
% ** Research Applications Program(RAP) 
% ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
% ** 2004/1/7 11:31:8 
% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
\name{rps}
       \alias{rps}
       \title{Ranked Probability Score}
       \description{Calculates the ranked probability score (rps) and
     ranked probability skill score (rpss) for probabilistic
     forecasts of ordered events.}
       \usage{ rps(obs, pred, baseline=NULL) }
      \arguments{
    \item{obs}{A vector of observed outcomes.  These values correspond to
      columns of prediction probabilities.   }
    \item{pred}{A matrix of probabilities for each outcome
      occurring.  Each column represents a category of prediction.}
    \item{baseline}{If NULL (default) the probability based on the
      sample data of each event to occur.    Alternatively,  a
      vector the same length of the as the number categories  can be
      entered. }
    }
       \value{
     \item{rps}{Ranked probability scores}
     \item{rpss}{Ranked probability skill score.  Uses baseline or
       sample climatology as a references score.}
     \item{rps.clim}{Ranked probability score for baseline forecast.}
           }
       \note{ Perhaps the format of the data is best understood in the
     context of an example.  Consider a probability of
     precipitation forecast of "none", "light" or  "heavy".  This
     could be [0.5, 0.3, 0.2].  If heavy rain occurred, the observed
       value would be 3, indicating event summarized in the third column
       occurred.

     \bold{The RPS value is scaled to a [0,1 ] interval by dividing by (number
     of categories -1 .  There is a discrepancy in the way this is
     explained in Wilks (2005) and the WWRF web page. } }

       \seealso{\code{\link{crps}}}
       \examples{

###  Example from Wilks, note without a baseline and only one
### forecast, the rpss and ss are not too meaningfull.



rps( obs = c(1), pred = matrix(c(0.2, 0.5, 0.3), nrow = 1))

}


\references{

WWRP/WGNE Joint Working Group on Verification - Forecast Verification - Issues, Methods and FAQ
\url{http://www.cawcr.gov.au/projects/verification/verif_web_page.html#RPS}

 Wilks, D. S. (2005) \emph{Statistical Methods in the Atmospheric Sciences }
 Chapter 7, San Diego: Academic Press.
 
}

\author{Matt Pocernich <pocernic@rap.ucar.edu>}

       \keyword{file}
