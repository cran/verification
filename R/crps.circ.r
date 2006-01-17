# computes the circular CRPS when the verifying direction is 'x',  
# and the probabilistic forecast is a von Mises distribution on
# the circle with mean 'mu' and concentration parameter 'kappa'
#
# PARAMETERS:
# x: a single direction in angles (0 <= x < 2 pi) 
# mu: a single direction in angles (0 <= mu < 2 pi) 
# kappa: a single positive number (kappa >= 0) 
#
# DEPENDENCIES:
# requires the CircStats package
# ES.VM.comp1 requires the existence of a lookup table whose path is set in the
# variable 'GLOBAL.lookup.path' in the code file ES.VM.comp1.r
#
# ACCURACY:
# ~ 10^-3


#load("ES.VM.comp1.lookup.dat")
# write(t(GLOBAL.ES.VM.comp1.lookup),"lookup.dat.txt",ncolumns=2)

crps.circ <- function(x, mu, kappa)
{
   ES.VM.Comp2.Max.Kappa = 1500 ## default parameter

  data(lookup)
  
if (  kappa >= lookup[nrow(lookup),1] &&
      kappa >= ES.VM.Comp2.Max.Kappa )
     {
     sigma <- 1/sqrt(kappa)
     alpha <- pmin(abs(x-mu),2*pi-abs(x-mu))
     tmp.x <- alpha/sigma
     return( list(circ.crps = - ( sigma * (1/sqrt(pi) - tmp.x * (2 *
      pnorm(tmp.x) - 1) - 2 * dnorm(tmp.x) ) ) ) )
     }
else {return(list (circ.crps = ES.VM.comp2(x, mu, kappa)-
      0.5*ES.VM.comp1(kappa)) )} ### 
}
