# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
# ** Copyright UCAR (c) 1992 - 2004 
# ** University Corporation for Atmospheric Research(UCAR) 
# ** National Center for Atmospheric Research(NCAR) 
# ** Research Applications Program(RAP) 
# ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
# ** 2004/1/7 11:29:42 
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
summary.cat.cat <- function(object, ...){

  cat("\nThe forecasts are categorical, the observations are categorical.\n")

  cat(paste("Percent Correct (PC) = ", formatC(object$PC, digits = 2), "\n"))
  cat(paste("Hit Rate             = ", formatC(object$hit.rate, digits = 3), "\n"))
  cat(paste("False Alarm Rate     = ", formatC(object$false.alarm.rate, digits = 3), "\n"))
  cat(paste("Bias                 = ", formatC(object$BIAS, digits = 3), "\n"))
  cat(paste("Threat Score         = ", formatC(object$TS, digits = 3), "\n"))
  cat(paste("Heidke Skill Score   = ", formatC(object$HSS, digits = 3), "\n"))
  cat(paste("Pierce Skill Score   = ", formatC(object$PSS, digits = 3), "\n"))
  cat(paste("Gerrity Score        = ", formatC(object$PSS, digits = 3), "\n"))

}
