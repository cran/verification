# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
# ** Copyright UCAR (c) 1992 - 2004 
# ** University Corporation for Atmospheric Research(UCAR) 
# ** National Center for Atmospheric Research(NCAR) 
# ** Research Applications Program(RAP) 
# ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
# ** 2004/1/7 11:29:42 
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
summary.bin.bin <- function(object, ...){
  ## print function for binary forecast, binary outcome
  cat("\nThe forecasts are binary, the observations are binary.\n")
  cat("The contingency table for the forecast \n")
  print(object$tab)
  cat("\n")
 
  cat(paste("PODy = ", formatC(object$POD, digits = 4), "\n"))
  cat(paste("TS   = ", formatC(object$TS, digits = 4), "\n"))
  cat(paste("ETS  = ", formatC(object$ETS, digits = 4), "\n"))
  cat(paste("FAR  = ", formatC(object$FAR, digits = 4), "\n"))
  cat(paste("HSS  = ", formatC(object$HSS, digits = 4), "\n"))
  cat(paste("PC   = ", formatC(object$PC, digits = 4), "\n"))
  cat(paste("BIAS = ", formatC(object$BIAS, digits = 4), "\n"))
  }

