# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
# ** Copyright UCAR (c) 1992 - 2004 
# ** University Corporation for Atmospheric Research(UCAR) 
# ** National Center for Atmospheric Research(NCAR) 
# ** Research Applications Program(RAP) 
# ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
# ** 2004/1/7 11:29:42 
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
brier<- function(obs, pred, baseline = NULL,
                 thresholds  = seq(0,1,0.1), bins = TRUE, ... ){
################
id <- is.finite(obs) & is.finite(pred)
obs <- obs[id]
pred <- pred[id]


## internal function used in verify
## used with a probablistic forecast with a binary outcome.

  pred <- round(pred, 8)
##   thresholds <- seq(0,1,1/n.members ) if ensembles allowed
if(max(pred)>1 | min(pred)<0) {
 
cat("Predictions outside [0,1] range.  \n Are you certain this is a probability forecast? \n")}

## baseline ave if not provided.

if(is.null(baseline)){obar <- mean(obs); baseline.tf <- FALSE}else
{obar<- baseline; baseline.tf <- TRUE}

bs.baseline <- mean( (obar - obs)^2)

### if bins = TRUE, thresholds define bins in which predictions are
### sorted and assigned the centered value.  If false, it is assumed
### that thresholds specifiy the exact levels of probability.

if(bins){XX <- probcont2disc(pred, bins = thresholds)
       pred <- XX$new
       new.mids <- XX$mids} else{
         
## count number of discrete probabilities.  If greater than 20, issue warning.
         if( length(unique(pred)) > 20 ){
    warning("More than 20 unique probabilities. This could take awhile.")}
} ## close else


N.pred <- aggregate(pred, by = list(pred), length) ## number of
                                        # times each prediction used.

N.obs <- aggregate(obs, by = list(pred), sum) ## number of
                                        # times each prediction used.

if(bins){XX<- data.frame(Group.1 = new.mids)  ## make certain all bins are represented.
N.pred <- merge(XX, N.pred, all.x = TRUE)
N.obs <- merge(XX, N.obs, all.x = TRUE)} ## close bins


obar.i <- N.obs$x/N.pred$x  # change int to numerics

y.i  <- as.numeric(as.character(N.obs$Group.1)) 

bs <- mean( (pred - obs)^2)

    n <- length(obs)
    ss <- 1 - bs/bs.baseline
    bs.rel <- sum(N.pred$x *(y.i - obar.i)^2, na.rm = TRUE)/n
    bs.res <- sum(N.pred$x * (obar.i - obar)^2, na.rm = TRUE)/n
    bs.uncert <- obar * (1 - obar)
    check <- bs.rel - bs.res + bs.uncert
    prob.y <- N.pred$x/n

 return(list(baseline.tf = baseline.tf, bs = bs, bs.baseline = bs.baseline, 
        ss = ss, bs.reliability = bs.rel, bs.resol = bs.res, 
        bs.uncert = bs.uncert, y.i = y.i, obar.i = obar.i, prob.y = prob.y, 
        obar = obar, thres = thresholds, check = check))
}
