# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
# ** Copyright UCAR (c) 1992 - 2004 
# ** University Corporation for Atmospheric Research(UCAR) 
# ** National Center for Atmospheric Research(NCAR) 
# ** Research Applications Program(RAP) 
# ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
# ** 2004/1/7 11:29:42 
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 

"roc.plot.default" <- function(x, pred, thresholds = NULL, binormal = FALSE,  leg = NULL,  
          plot = "emp", plot.thres = seq(0.1, 0.9, 0.1),
               main = "ROC Curve",  xlab = "False Alarm Rate", ylab = "Hit Rate",  ...){
#

#  old.par <- par(no.readonly = TRUE) # all par settings which
#  on.exit(par(old.par) )
if(( plot=="binorm"| plot == "both")  & binormal == FALSE){
  stop("binormal must be TRUE in order to create a binormal plot")}
  
pred<- as.matrix(pred)
n.forc<- dim(pred)[2] ## number of forecasts

########### threshold vector

if(is.null(thresholds) ){ ## if thresholds are not provided, calculate thresholds so that the
  # are approximately the same number in each bin.
  thresholds <- sort(as.numeric( unique(pred) ) ) }
if(length(thresholds) == 1){
  n.thres.bins<- thresholds
t          <- seq(0, 1, 1/n.thres.bins)
  thresholds <-quantile(pred, t) # pred needs to be all preds
}

####################################################### internal roc function

roc <- function(x, pred, thres){ # internal function that returns plot points
pody     <- numeric()
podn     <- numeric()
n        <- length(x)
n.thres  <- length(thres)  # number of unique thresholds, thres =  
lng      <- length(x)            # 

a         <- x > 0 #
a.sum     <- sum(a)
a.not.sum <- sum(!a)

for(i in 1:(n.thres) ){
  
  b      <- pred >= thres[i]
  pody[i]<- sum(  b * a  )/ a.sum ## hit rate
  podn[i]<- sum(  (!b)  * (!a)  )/ a.not.sum  ## FAR
 
} ## close for loop 1:n.thres


thres[n.thres +1 ]<- 1 # plot 0,0 point
pody[n.thres  + 1] <- 0
podn[n.thres  + 1] <- 1 

    if(binormal){
       zH <- c(NA ,qnorm( pody[-c(1, n.thres + 1)] ),  NA )  # NA are for top and bottom value
       zF <- c(NA ,qnorm(1 - podn[-c(1, n.thres + 1) ]), NA )# NA are for top and bottom value
       } else {
         zH <- rep(NA, n.thres + 1)
         zF <- rep(NA, n.thres + 1)
       } ## close if binormal
    return(cbind(thres, pody, podn, zH, zF))
       }    # closs roc function

##############################################################

### roc.area function


DAT  <- array(NA, dim = c(length(thresholds) + 1, 5, n.forc))  ## adj to 5 cols to cal area under.
VOLS <- matrix(nrow = n.forc, ncol = 5)
binormal.pltpts<- list()

for(j in 1:n.forc){  ## n.forc = number of forecasts = number of columns
DAT[, , j] <- roc(x, pred[, j], thresholds)
#############################

if(binormal){
  dat  <- as.data.frame( DAT[,,j] )
  names(dat) <- c("thres", "proby", "probn", "zH", "zF")
  dat <- dat[is.finite(dat$zH) & is.finite(dat$zF), ] ## reduce dat, get rid of nans and inf   
  new <-  as.data.frame( matrix(qnorm(seq(0.005, 0.995, 0.005 ) ), ncol = 1) )
  names(new) <- "zF"
  A <- lm(zH ~ zF, data = dat)$fitted.values
  B <- predict(lm(zH ~ zF, data = dat), newdata = new)

binormal.pltpts[[j]]<- data.frame( t = new$zF, x = pnorm(new$zF), y = pnorm(B) )
binormal.area  <- sum(0.005*pnorm(B) , na.rm = TRUE) } else {binormal.area <- NA}

#############################
## vol calcs
v <- roc.area(x, pred[,j])
VOLS[j,1]<- v$A.tilda
VOLS[j,2]<- v$p.adj
VOLS[j,3]<- v$A
VOLS[j,4]<- v$p
VOLS[j,5]<- binormal.area
} ## close for j in 1:n.forc

VOLS<- data.frame(paste("Model ", seq(1, n.forc) ), VOLS )  
names(VOLS)<- c("Model", "Area.adj", "p.adj", "Area", "p-value", "binorm.area")
  
## stuff to return
r<- structure(list( plot.data = DAT, roc.vol = VOLS, binormal.ptlpts = binormal.pltpts), class = "roc.data")

####################################################  
###### plot if required  

if(!is.null(plot) ){  ## if plot is not false, then make frame.
 par(mar = c(4,4,4,1))

plot( 1 - DAT[,3 ,], DAT[,2,], type = 'n', xlim = c(0,1), ylim = c(0,1),
     main = main,  xlab = xlab, ylab = ylab, ... ) # points don't matter, plot is type 'n'

abline(h=seq(0,1,by=.1),v=seq(0,1,by=.1),lty=3, lwd = 0.5, col = "grey")
abline(0,1)
 
if(length(thresholds)< 16){L <- "b" }else{L<-"l"} # if less than 12 point show points
}
### if empirical or both

if(plot == "emp" | plot == "both" ){ 
 for(i in 1:n.forc){
    points(1  - DAT[,3,i], DAT[ ,2,i] , col = i, lty = i, type = "l", lwd = 2)

## plot threshold points on graph   
if(!is.null(plot.thres)){  ## does this need an else statement ?  ## by default, these match

    ind <- match(round(plot.thres,2),  round( DAT[,1,i], 2) )
    points(1 - DAT[ind,3, i], DAT[ind,  2, i], col = 1, pch = 19)
    text(1 - DAT[ind,3, i], DAT[ind,  2, i], plot.thres, pos = 4, offset = 1 )
} # close plot thres
 } ## close 1:n.thres
 
### general info

} ## close empirical

if(plot == "binorm" | plot == "both" ){

for(i in 1:n.forc){
  dat<- binormal.pltpts[[i]]
    points(dat$x, dat$y , col = 2, lty = i, type = "l", lwd = 2)

  }## close i:n.force loop
}## close binorm plot 

#### text
if(plot == "both"){
 text(0.6, 0.1, "Black lines are the empirical ROC")  
text(0.6, 0.07, "Red lines and symbols are the bi-normal ROC")
 text(0.6, 0.04, "The area under the binormal curve is in parathesis.")

}

if(plot == "emp"){
 text(0.6, 0.1, "Black lines are the empirical ROC")  
}

if(plot == "binorm"){
text(0.6, 0.1, "Red lines  are the bi-normal ROC")
}

########
## make legend text
if(is.null(leg)){
leg.txt<- paste ("Model ", LETTERS[seq(1, n.forc)]) } else {(leg.txt <- leg)}

if(plot == "emp"){
leg.txt<- paste (leg.txt, "  ",formatC(VOLS$Area.adj, digits = 3) ) }

if(plot =="binorm"){ 
leg.txt<- paste (leg.txt, "  ", formatC(VOLS$binorm.area, digits = 3)  )}

if(plot == "both"){
leg.txt<- paste (leg.txt, "  ",formatC(VOLS$Area.adj, digits = 3), " (",
formatC(VOLS$binorm.area, digits = 3) ,")" ) }

  
#if(leg != FALSE){  
legend(list(x=0.6, y=0.4), legend = leg.txt, bg = "white", cex = 0.6,
         lty = seq(1,n.forc), col = c("black", "red","blue"), merge=TRUE) #}
# invisible()
 invisible(r)
 # end function
}
                





