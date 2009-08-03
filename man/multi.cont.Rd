% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2009 
% ** University Corporation for Atmospheric Research(UCAR) 
% ** National Center for Atmospheric Research(NCAR) 
% ** Research Applications Program(RAP) 
% ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
% ** 2004/1/7 11:31:8 
% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
\name{multi.cont}
       \alias{multi.cont}
       \title{Multiple Contingency Table Statistics }
       \description{Provides a variety of statistics for a data
     summarized in a contingency table.  This will work for a 2 by 2
       table, but is more useful for tables of greater dimensions.}
       \usage{multi.cont(DAT, baseline = NULL)}
\arguments{
  \item{DAT}{A contingency table in the form of a matrix.  It is
    assumed that columns represent observation, rows represent
    forecasts.}
    \item{baseline}{A vector indicating the baseline probabilities of each category.  By default, it the baseline or naive forecasts is based on teh }
}
\value{
  \item{pc}{Percent correct - events along the diagonal.}
  \item{bias}{Bias}
    \item{ts}{Threat score a.k.a. Critical success index (CSI) }
  \item{hss}{Heidke Skill Score}
  \item{pss}{Peirce Skill Score}
  \item{gs}{Gerrity Score}
  \item{pc2}{Percent correct by category (vector)}
 \item{h}{Hit Rate by category (vector)}
 \item{false.alarm.ratio}{False alarm ratio by category (vector)}
     }
     \seealso{\code{binary.table}}
       \references{
Gerrity, J.P. Jr (1992).  A  note on Gandin and Murphy's equitable skill 
score. \emph{Mon. Weather Rev.}, \bold{120}, 2707-2712.
     
     Jolliffe, I.T. and D.B. Stephenson (2003). Forecast verification: a 
practitioner's guide in atmospheric science. John Wiley and Sons.  See
chapter 4 concerning categorical events, written by R. E. Livezey.
       }
       \note{Some verification statistics for a contingency table assume that the forecasts and observations are ordered, while others do not.  An example of an ordered or ordinal forecast is "low, medium and high".  An example of an unordered or nominal forecast is "snow, rain, hail, and none."  If the forecasts are ordered, it is possible to account for forecasts which are close to the the observed value.  For example, the Gerrity score takes this closeness into account.  The Pierce Skill Score does not.  
       	
       	For ordered forecast, it is assumed that the columns and rows of the input matrix are ordered sequentially. 
       	
       	When multiple values are returned, as in the case of pc2, h, f and false.alarm.ratio, these values are conditioned on that category having occurred.  For example, in the example included in Jolliffe, given that a below average temperature was observed, the forecast had a bias of 2.3 and had a 0.47 chance of being detected.
       	}
       
    \examples{
DAT<- matrix(c(7,4,4,14,9,8,14,16,24), nrow = 3) # from p. 80 - Jolliffe
multi.cont(DAT)

DAT<- matrix(c(3,8,7,8,13,14,4,18,25), ncol = 3) ## Jolliffe JJA
multi.cont(DAT)

DAT<- matrix(c(50,47,54,91,2364,205,71,170,3288), ncol = 3) # Wilks p. 245
multi.cont(DAT)

DAT<- matrix(c(28, 23, 72, 2680 ), ncol = 2) ## Finley
multi.cont(DAT)
## Finnish clouds
DAT<- matrix(c(65, 10, 21, 29,17,48, 18, 10, 128), nrow = 3, ncol = 3, byrow = TRUE)
multi.cont(DAT)  
 ### alternatively, the verify function and summary can be used.
 
 mod <- verify(DAT, frcst.type = "cat", obs.type = "cat")
 summary(mod)
 
 }
 \author{Matt Pocernich <pocernic@rap.ucar.edu>}

       \keyword{file}
