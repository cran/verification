# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
# ** Copyright UCAR (c) 1992 - 2004 
# ** University Corporation for Atmospheric Research(UCAR) 
# ** National Center for Atmospheric Research(NCAR) 
# ** Research Applications Program(RAP) 
# ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
# ** 2004/9/1 14:13:55 
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
measurement.error <- function( obs, frcs = NULL, theta = 0.5, t = 1, u = 0, h = NULL, ...){
### if frcs is null, its assumed that obs is a vector with
### assume data entered as c(n11, n10, n01, n00)
if(is.null(frcs) & length(obs) ==4 ){
## assume data entered as c(n11, n10, n01, n00)
n11<- obs[1]
n10<- obs[2]
n01<- obs[3]
n00<- obs[4]} else{

### check to see if frcs is [0,1] if not convert
if( prod(unique(obs) %in% c(0,1) ) == 1 ){ ## if obs is not binomial

  if(is.null(h)){ frcs <- as.numeric(frcs > theta)  } else {frcs <- as.numeric(frcs > h) }
}# close if not unique 

A<- table(data.frame(obs, frcs) )

n11 <- A[2,2]
n00 <- A[1,1]
n10 <- A[2,1]
n01 <- A[1,2]
}# close is.null else

## check p > theta else transform
n  <- n11 + n10 + n01 + n00
p  <- (n10 + n11) / n # prob obs = yes
## transform it p > theta

if(p > theta){
n00.old <- n00
n11.old <- n11
n10.old <- n10
n01.old <- n01
n11     <- n00.old
n10     <- n01.old
n01     <- n10.old
n00     <- n11.old
theta   <- 1-theta  
}## close if vector

 
n  <- n11 + n10 + n01 + n00
p<- (n10 + n11) / n # prob obs = yes
px1  <- (n11 + n01)/ n
px0  <- (n10 + n00)/ n
p11 <- (n11*(1-u) - n01*u) / ( (n11+n01)*(t-u) )
p00 <- ( n00*t - n10*(1-t)) / ( (n10+n00)*(t-u))
p10 <- (n10*(1-u)- n00*u ) /
  ( ( n10+ n00)*(t-u) )

# K <- px1*(p11 - theta)/(p*(1-theta))

K <- (n11*(1 - u - theta*(t-u)) - n01 * (u + theta*(t - u)) ) /
  ( (n11+ n10)*(1-theta) - n*(u *(1-theta)) )

L1 <- log(n11/( (n01 + n11 )  *(theta*(t - u)+ u  )) )
L2 <-   log(n01/( (n01 + n11)   * ( 1 -u - theta*(t-u) ) ) )

G  <- 2*n11* L1 + 
              2*n01* L2
  p.val <- (1-pchisq(G, 1)) / 2
q11<- n11/(n11+n01)

return(list(G = G, p = p.val, K = K))
}
