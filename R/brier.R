# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
# ** Copyright UCAR (c) 1992 - 2004 
# ** University Corporation for Atmospheric Research(UCAR) 
# ** National Center for Atmospheric Research(NCAR) 
# ** Research Applications Program(RAP) 
# ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
# ** 2004/1/7 11:29:42 
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
brier<- function(obs, pred, baseline = NULL, thresholds  = seq(0,1,0.1), ... ){
## internal function used in verify
## used with a probablistic forecast with a binary outcome.
  
  
if(max(pred)>1 | min(pred)<0) {

cat("Predictions outside [0,1] range.  \n Are you certain this is a probability forecast? \n")}

bs<- mean( (pred - obs)^2)

## baseline ave if not provided.

if(is.null(baseline)){obar <- mean(obs); baseline.tf <- FALSE}else
{obar<- baseline; baseline.tf <- TRUE}

bs.baseline <- mean( (obar - obs)^2)

N0    <- sum(pred == 0)
N1    <- sum(pred == 1)
bins  <- thresholds  ### this will only work if an internal function.

pred.bins<- cut(pred, breaks = bins, labels = FALSE, include.lowest = TRUE )

N<-  aggregate(pred.bins, by = list(pred.bins), length)$x # number of preds in each bin
obar.1<- aggregate(obs, by = list(pred.bins), sum)$x # number of preds in each bin

obar.i<- obar.1/N

y.i <- bins[-length(bins)] + diff(bins)/2 # mid point of each bin
  
n<- length(obs)

ss <- 1 - bs/bs.baseline


bs.rel <- (sum(N*(y.i -obar.i)^2)) /n   ## reliability
bs.res <- (sum(N*(obar.i -obar)^2)) /n  ## resolution
bs.uncert<- obar*(1- obar)
check <- bs.rel - bs.res + bs.uncert 

prob.y <- N/n

bs.discrete <-  mean( ( obs - y.i[pred.bins])^2 )## for comparison, a bs score based on binned categories.

return(list(baseline.tf = baseline.tf, bs = bs, bs.baseline = bs.baseline, ss = ss,
            bs.reliability = bs.rel, bs.resol = bs.res, bs.uncert = bs.uncert,
            y.i = y.i, obar.i = obar.i, prob.y = prob.y, obar = obar) )
  
}
