####################################################### internal roc function
roc <- function(x, pred, thres,  binormal,...){ # internal function that returns plot points
H     <- numeric()
F     <- numeric()
n        <- length(x)
n.thres  <- length(thres)  # number of unique thresholds, thres =  
lng      <- length(x)            # 

a         <- x > 0 # event happened
a.sum     <- sum(a) # n*1
a.not.sum <- sum(!a) # n*0

for(i in 1:(n.thres) ){
  
  b      <- pred >= thres[i] # predict yes 
  H[i]<- sum(  b * a  )/ a.sum ## hit rate
  F[i]<- sum(  b  * (!a)  )/ a.not.sum  ## False alarm rate
 
} ## close for loop 1:n.thres


thres[n.thres +1 ] <- 1 # plot 0,0 point
H[n.thres  + 1]    <- 0
F[n.thres  + 1]    <- 0 

    if(binormal){
       zH <- c(NA ,qnorm( H[-c(1, n.thres + 1)] ),  NA )  # NA are for top and bottom value
       zF <- c(NA ,qnorm(F[-c(1, n.thres + 1) ]), NA )# NA are for top and bottom value
       } else {
         zH <- rep(NA, n.thres + 1)
         zF <- rep(NA, n.thres + 1)
       } ## close if binormal
    return(cbind(thres, H, F, zH, zF))
       }    # closs roc function

##############################################################
