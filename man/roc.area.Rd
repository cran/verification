\name{roc.area}
       \alias{roc.area}
        \title{Area under curve (AUC) calculation for Response Operating Characteristic curve.}
       \description{This function calculates the area underneath a ROC
	 curve following the process outlined in Mason and Graham (2002).  The
	 p-value produced is related to the Mann-Whitney U statistics.
	 The p-value is calculated using the wilcox.test function which
	 automatically handles ties and makes approximations for large values.

The p-value addresses the null hypothesis $H_o:$ The area under the ROC
curve is 0.5 i.e. the forecast has no skill. 
       }
       \usage{    roc.area(obs, pred)
       }
       \arguments{
	 \item{obs}{A binary observation (coded \{0, 1 \} ).}
	 \item{pred}{A probability prediction on the interval [0,1].}
       }

       \value{
	 \item{A}{Area under ROC curve, adjusted for ties in
	   forecasts, if present}
	 \item{n.total}{Total number of records}
	 \item{n.events}{Number of events}
	 \item{n.noevents}{Number of non-events}
	 \item{p.value}{Unadjusted p-value}
	 	 }
 \note{This function is used internally in the \code{roc.plot} command
   to calculate areas.}
 \seealso{\code{\link{roc.plot}}, \code{\link{verify}},
   \code{\link{wilcox.test} } }
 \references{Mason, S. J. and Graham, N. E. (2002) Areas beneath the
   relative operating characteristics (ROC) and relative operating
   levels (ROL) curves: Statistical significance and interpretation,
   \emph{Q. J. R. Meteorol. Soc.} \bold{128}, 2145--2166.}
 
	   \examples{# Data used from Mason and Graham (2002).
a<- c(1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990,
 1991, 1992, 1993, 1994, 1995)
b<- c(0,0,0,1,1,1,0,1,1,0,0,0,0,1,1)
c<- c(.8, .8, 0, 1,1,.6, .4, .8, 0, 0, .2, 0, 0, 1,1)
d<- c(.928,.576, .008, .944, .832, .816, .136, .584, .032, .016, .28, .024, 0, .984, .952)

A<- data.frame(a,b,c, d)
names(A)<- c("year", "event", "p1", "p2")

## for model with ties
roc.area(A$event, A$p1)

## for model without ties
roc.area(A$event, A$p2)


}
\author{Matt Pocernich}

       \keyword{file}
