# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
# ** Copyright UCAR (c) 1992 - 2004 
# ** University Corporation for Atmospheric Research(UCAR) 
# ** National Center for Atmospheric Research(NCAR) 
# ** Research Applications Program(RAP) 
# ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
# ** 2004/1/7 11:29:42 
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
verify<- function(obs, pred,
                  baseline = NULL, # sample.baseline = FALSE, ? 
                  frcst.type = "prob", obs.type = "binary", 
                  thresholds = seq(0,1,0.1), show = TRUE ){

if(frcst.type == "binary" & obs.type == "binary" & is.null(pred) ){

  A <- table.stats(obs)
class(A)<- c("verify", "bin.bin")
} else
if(frcst.type == "binary" & obs.type == "binary"){
if(length(unique(obs))>2 | length(unique(pred))>2 ) {warning("Prediction or observation may not be binary \n")}
A <- table.stats(obs, pred)
class(A)<- c("verify", "bin.bin")

} else

if(frcst.type == "prob" & obs.type == "binary"){
if(show){
cat("If baseline is not included, baseline values  will be calculated from the  sample obs. \n") }

A<- brier(obs, pred, baseline, thresholds )
class(A)<- c("verify", "prob.bin")
} else

if(frcst.type == "norm.dist" & obs.type == "cont"){
A<- crps(obs,pred)
class(A)<- class(A)<- c("verify", "norm.dist.cont")
  
}else

if(frcst.type == "cont" & obs.type == "cont"){

A<- c()
if(is.null(baseline)){baseline <- mean(obs); A$baseline.tf <- FALSE} else {A$baseline.tf <- TRUE}
A$MAE       <- mean(abs(pred - obs))
A$MSE       <- mean( (pred - obs)^2 )
A$ME        <- mean( (pred - obs) )
A$MSE.baseline <- mean( (mean(baseline) - obs)^2)
# mse persistance only valid if data is presented in chronological order.
A$MSE.pers  <- mean( (obs[-length(obs)]- obs[-1])^2)
A$SS.baseline  <- 1 - (A$MSE - A$MSE.baseline)

class(A)<- c("verify", "cont.cont")

}else

if(frcst.type == "cat" & obs.type == "cat"){
a<- sort(unique(c(obs, pred) ) )
obs.a <- c(a, obs)
pred.a <- c(a, pred)

DAT<- table(pred.a, obs.a) 
diag(DAT)<- diag(DAT) - 1

A <- multi.cont(DAT)


  class(A)<- c("verify", "cat.cat")

}else { cat("This combination of predictions \n and observations is not \n currently supported. \n") }

## attach original data to be used in plot functions.

A$obs   <- obs
A$pred <- pred
A$baseline <- baseline


return(A)

} # close function
  

