observation.error <- function(obs, gold.standard = NULL, ...){

if(is.null(gold.standard) & length(obs) ==4 ){
## assume data entered as c(n11, n10, n01, n00)
n11<- obs[1]
n10<- obs[2]
n01<- obs[3]
n00<- obs[4]
  return( list( t = n11/(n11+n01), u = n10/(n10+n00) ))
} else {
A   <- table(data.frame( obs, gold.standard) )
n11 <- A[2,2]
n00 <- A[1,1]
n10 <- A[2,1]
n01 <- A[1,2]

return( list(t = n11/(n11+n01), u = n10/(n10+n00) ) )
}

}
