summary.norm.dist.cont <- function(object, ...){

  cat("\nThe forecasts are a normal probability distribution. \n")
  cat("The observations are continuous.\n\n")
  cat(paste("Average crps score      = ", formatC(object$CRPS, digits = 4), "\n"))
  cat(paste("Average ignorance score = ", formatC(object$IGN, digits = 4), "\n"))
}
