# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
# ** Copyright UCAR (c) 1992 - 2004 
# ** University Corporation for Atmospheric Research(UCAR) 
# ** National Center for Atmospheric Research(NCAR) 
# ** Research Applications Program(RAP) 
# ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
# ** 2004/1/7 11:29:42 
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
leps.default <- function(x, pred,  plot = TRUE, ...){
## leps function
# x<- rnorm(100, mean = 1, sd = sqrt(50))
# pred<- rnorm(100, mean = 10, sd = sqrt(500)) 

old.par <- par(no.readonly = TRUE) # all par settings which
                                      # could be changed.
on.exit(par(old.par) )

emp.prob <- rank(x)/( length(x)+ 1)
# add points to emp.prob to make it a full ecdf 0, min(c(pred, x)  ) and
# 1, max( c(pred, x) )
obs.a<- c(obs, min(c(pred, x)) , max(c(pred, x) ) )
emp.prob.a<- c(emp.prob, 0, 1 )

ecdf.obs <- approxfun(obs.a, emp.prob.a) ## function returns ecdf of obs

leps.0 <- mean(abs( ecdf.obs(pred) - ecdf.obs(x) ) )

leps.1 <- 2 - 3*(leps.0 + mean(ecdf.obs(pred)*(1-ecdf.obs(pred) ) )
                        + mean(ecdf.obs(x)* (1-ecdf.obs(x)  ) ) )

if(plot){
#  if(is.null(titl)){titl<- "LEPS plot"} 
plot(x, ecdf.obs(x), ylim = c(0,1),
     ylab = expression(paste("Empirical CDF ", F[o](o)) ),
         xlab = "Observation", ... )
}

r <- list(leps.0 = leps.0, leps.1 = leps.1)

invisible(r)

} # end of function.


