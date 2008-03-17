% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2004 
% ** University Corporation for Atmospheric Research(UCAR) 
% ** National Center for Atmospheric Research(NCAR) 
% ** Research Applications Program(RAP) 
% ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
% ** 2004/1/7 11:31:8 
% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
\name{verify-internal}
\alias{table.stats}
\alias{summary.prob.bin}
\alias{summary.bin.bin}
\alias{summary.cont.cont}
\alias{summary.norm.dist.cont}
\alias{summary.cat.cat}
\alias{ranked.hist}
\alias{plot.prob.bin}
\alias{plot.cont.cont}
\alias{calc.ES.VM.comp1}
\alias{angle.dist}
\alias{ES.VM.comp1}
\alias{ES.VM.comp1.array}
\alias{approx.comp2}
\alias{dvm2}
\alias{ES.VM.comp2}
\alias{ES.VM.comp2.array}
\alias{roc.int}
\alias{pop.convert}
\title{
  Verification internal and secondary functions
}
\description{
 Listed below are either internal functions or secondary functions.
 }
 \usage{
pop.convert()
 table.stats(obs, pred = NULL)
 \method{summary}{prob.bin}(object, \dots)
 \method{summary}{bin.bin}(object, \dots)
 \method{summary}{cont.cont}(object, \dots)
 \method{summary}{norm.dist.cont}(object, \dots)
 \method{summary}{cat.cat}(object, \dots)
 ranked.hist(frcst, nbins = 10, titl = NULL) 
 \method{plot}{prob.bin}(x, \dots)
 \method{plot}{cont.cont}(x, \dots)
 roc.int(x, pred, thres, binormal)
% tilmans internal functions
angle.dist(a, b)
calc.ES.VM.comp1(kappa, mc.size)
% build.lookup(kappa.array, mc.size, save.progress=FALSE, graph.lookup=TRUE)
ES.VM.comp1(kappa)
ES.VM.comp1.array(kappa)

%%
approx.comp2(x, mu, kappa)
dvm2(theta, mu, kappa)
ES.VM.comp2(x, mu, k)
ES.VM.comp2.array(x, mu, k)
}

\keyword{internal}
% docclass is function
% Converted by Sd2Rd version 1.21.
