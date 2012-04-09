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
	\item{TS.se}{Standard Error for TS}
	\item{POD}{Hit Rate aka probability of detection}
	\item{POD.se}{Standard Error for POD}
	\item{M}{Miss rate}
	\item{F}{False Alarm RATE}
	\item{F.se}{Standard Error for F}
	\item{FAR}{False Alarm RATIO}
	\item{FAR.se}{Standard Error for FAR}
    \item{HSS}{Heidke Skill Score}
   \item{HSS.se}{Standard Error for HSS}
   \item{PSS}{Peirce Skill Score}
   \item{PSS.se}{Standard Error for PSS}
   \item{KSS}{Kuiper's Skill Score }  
   \item{PC}{Percent correct - events along the diagonal.}
   \item{PC.se}{Standard Error for PC}
   \item{BIAS}{Bias}
   \item{ETS}{Equitable Threat Score}
   \item{ETS.se}{Standard Error for ETS}
   \item{theta}{Odds Ratio}
   \item{log.theta}{Log Odds Ratio}
   \item{LOR.se}{Standard Error for Log Odds Ratio}
   \item{n.h}{Degrees of freedom for log.theta}
   \item{orss}{Odds ratio skill score, aka Yules's Q}
   \item{ORSS.se}{Standard Error for Odds ratio skill score}
   \item{eds}{Extreme Dependency Score}
   \item{esd.se}{Standard Error for EDS}
   \item{seds}{Symmetric Extreme Dependency Score}
   \item{seds.se}{Standard Error for Symmetric Extreme Dependency Score}
   \item{EDI}{Extreme Dependency Index}
   \item{EDI.se}{Standard Error for EDI}
   \item{SEDI}{Symmetric EDI}
   \item{SEDI.se}{Standard Error for SEDI}
       }
     \seealso{\code{verify} and \code{multi.cont}}
\references{
     
     Jolliffe, I.T. and D.B. Stephenson (2003). Forecast verification: a 
practitioner's guide in atmospheric science. John Wiley and Sons.  See
chapter 3 concerning categorical events. 

Stephenson, D.B. (2000).  "Use of `Odds Ratio for Diagnosing Forecast Skill." \emph{Weather and Forecasting} \bold{15} 221-232.  

Hogan, R.J., O'Connor E.J. and Illingworth, 2009.  "Verification of cloud-fraction forecasts." \emph{Q.J.R. Meteorol. Soc.} \bold{135}, 1494-1511.

}
       
       
       
       \note{Initially, table.stats was an internal function used by verify for binary events and multi.cont for categorical events. But occassionally, it is nice to use it directly.}
       
    \examples{

DAT<- matrix(c(28, 23, 72, 2680 ), ncol = 2) ## Finley
table.stats(DAT)


 }
 \author{Matt Pocernich <pocernic@rap.ucar.edu>}

       \keyword{file}
