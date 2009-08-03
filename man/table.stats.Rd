% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2009 
% ** University Corporation for Atmospheric Research(UCAR) 
% ** National Center for Atmospheric Research(NCAR) 
% ** Research Applications Program(RAP) 
% ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
% ** 2004/1/7 11:31:8 
% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
\name{table.stats}
       \alias{table.stats}
       \title{Verification statistics for a 2 by 2  Contingency Table }
       \description{Provides a variety of statistics for a data
     summarized in a 2 by 2 contingency table. }
       \usage{
       	table.stats(obs, pred,silent = FALSE)
       	}
\arguments{
	    \item{obs}{ Either a vector of contingency table counts, a vector of binary observations, or a 2 by 2 matrix in the form of a contingency table. (See note below.)}
    \item{pred}{Either null or a vector of binary forecasts. }
    \item{silent}{Should warning statements be surpressed.}
}
\value{
	\item{tab.out}{ Contingency table}
	\item{TS}{Threat score a.k.a. Critical success index (CSI) }
	\item{POD}{Hit Rate aka probability of detection}
	\item{M}{Miss rate}
	\item{F}{False Alarm RATE}
	\item{FAR}{False Alarm RATIO}
    \item{HSS}{Heidke Skill Score}
   \item{PSS}{Peirce Skill Score}
   \item{KSS}{Kuiper's Skill Score }  
   \item{PC}{Percent correct - events along the diagonal.}
   \item{BIAS}{Bias}
   \item{ETS}{Equitable Threat Score}
       }
     \seealso{\code{verify} and \code{multi.cont}}
\references{
     
     Jolliffe, I.T. and D.B. Stephenson (2003). Forecast verification: a 
practitioner's guide in atmospheric science. John Wiley and Sons.  See
chapter 3 concerning categorical events. }
       
       \note{Initially, this was an internal function used by verify for binary events and multi.cont for categorical events. But occassionally, it is nice to use it directly.}
       
    \examples{

DAT<- matrix(c(28, 23, 72, 2680 ), ncol = 2) ## Finley
table.stats(DAT)


 }
 \author{Matt Pocernich <pocernic@rap.ucar.edu>}

       \keyword{file}
