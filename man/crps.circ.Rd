% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2004 
% ** University Corporation for Atmospheric Research(UCAR) 
% ** National Center for Atmospheric Research(NCAR) 
% ** Research Applications Program(RAP) 
% ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
% ** 2004/1/7 11:31:8 
% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
\name{crps.circ}
       \alias{crps.circ}
       \title{CRPS Statistics for circular data}
       \description{Computes the circular CRPS when the verifying
	 direction is 'x'    and the probabilistic forecast is a von
	 Mises distribution on the circle with mean 'mu' and
	 concentration parameter 'kappa'. 
       }
       \usage{crps.circ(x, mu, kappa )}
\arguments{
  \item{x}{A single direction in angles (0 <= x < 2 pi) }
  \item{mu}{A single direction in angles (0 <= mu < 2 pi) }
  \item{kappa}{A single positive number (kappa >= 0) }
  }
}
\value{
  \item{crps.circ}{Circular crps}
 }

       \references{
	 Grimit, E.P., T. Gneiting, V.J. Berrocal and N.A. Johnson
	 (2006). The continuous  ranked probability score for circular
	 variables and its application to mesoscale forecast ensemble
	 verification. , \bold{132}, pp. 1-17,
	 \emph{Q.J.R. Meteorolo. Soc.}.
       }

	 
\examples{

 x                     <- pi/3
  mu                    <- pi/3.2
  kappa                 <- 10000
  ES.VM.Comp2.Max.Kappa <- 1500

crps.circ( x = pi/3,  mu = pi/3.2, kappa = 10000)

 }
\author{ Nick Johnson<nickaj@gmail.com>
}

       \keyword{file}
