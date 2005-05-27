% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2004 
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
       \usage{multi.cont(DAT)}
\arguments{
  \item{DAT}{A contingency table in the form of a matrix.  It is
    assumed that columns represent observation, rows represent
    forecasts.
  }
}
\value{
  \item{pc}{Percent correct - events along the diagonal.}
  \item{bias}{Bias}
  \item{pod}{Probability of detecting an event.}
  \item{far}{False Alarm Rate}
  \item{ts}{Threat score a.k.a. Critical success index (CSI) }
  \item{hss}{Heidke Skill Score}
  \item{pss}{Pierce Skill Score}
  \item{gs}{Gerrity Score}
	 }

       \references{
Gerrity, J.P. Jr (1992).  A  note on Gandin and Murphy's equitable skill 
score. \emph{Mon. Weather Rev.}, \bold{120}, 2707-2712.
	 
	 Joliffe, I.T. and D.B. Stephenson (2003). Forecast verification: a 
practitioner's guide in atmospheric science. John Wiley and Sons.  See
chapter 4 concerning categorical events, written by R. E. Livezey.
       }
       
 	\examples{
DAT<- matrix(c(7,4,4,14,9,8,14,16,24), nrow = 3) # from p. 80 - Jolliffe
multi.cont(DAT)

DAT<- matrix(c(3,8,7,8,13,14,4,18,25), ncol = 3) ## jolliffe JJA
multi.cont(DAT)

DAT<- matrix(c(50,47,54,91,2364,205,71,170,3288), ncol = 3) # wilks p. 245
multi.cont(DAT)

DAT<- matrix(c(28, 23, 72, 2680 ), ncol = 2) ## finley
multi.cont(DAT)

 }
\author{Matt Pocernich <pocernic@rap.ucar.edu>}

       \keyword{file}
