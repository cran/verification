leps <- function(x, pred,  plot = TRUE, ...){
## leps function
## Corrections made to errors pointed out by Marin Mittermaier.
## 1/9/06  
  old.par <- par(no.readonly = TRUE) # all par settings which
                                      # could be changed.
 on.exit(par(old.par) )

Fn <- ecdf(x) ## empirical cdf
 
leps.0 <- mean(abs( Fn(pred) - Fn(x) ) )

leps.1 <- 2 - 3*(leps.0 + mean(Fn(pred)*(1-Fn(pred) ) )
                        + mean(Fn(x)* (1-Fn(x)  ) ) )
 
if(plot){
#  if(is.null(titl)){titl<- "LEPS plot"} 
plot(x, Fn(x), ylim = c(0,1),
     ylab = expression(paste("Empirical CDF ", F[o](o)) ),
         xlab = "Observation", ... )
}

r <- list(leps.0 = leps.0, leps.1 = leps.1)

invisible(r)

} # end of function.


