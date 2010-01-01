# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
# ** Copyright UCAR (c) 1992 - 2004 
# ** University Corporation for Atmospheric Research(UCAR) 
# ** National Center for Atmospheric Research(NCAR) 
# ** Research Applications Program(RAP) 
# ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
# ** 2004/1/7 11:29:42 
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
table.stats<- function(obs, pred = NULL, silent = FALSE){
## internal function used in verify
## used with a binary forecast and a binary outcome.
if(is.null(pred) & length(obs) ==4 ){
  if(!silent){ print(" Assume data entered as c(n11, n01, n10, n00) Obs*Forecast") }
a <- obs[1]
b <- obs[2]
c <- obs[3]
d <- obs[4]
tab.out <- matrix(c(a,c,b,d), nrow = 2)
} 
if(is.null(pred) & is.matrix(obs) & prod(dim(obs)) ==4 ){
if(!silent){print(" Assume contingency table has observed values in columns, forecasts in rows")}
obs <- as.numeric(obs)
a <- obs[1]
b <- obs[3]
c <- obs[2]
d <- obs[4]
tab.out <- matrix(c(a,c,b,d), nrow = 2)
} 
if(!is.null(pred)& !is.null(obs)){

  tab.out <- table(as.numeric(obs), as.numeric(pred))
  
  a <-tab.out["1","1"]
  b <-tab.out["0","1"]
  c <-tab.out["1","0"]
  d <-tab.out["0","0"]
} ## close else


###
n <- a + b + c + d

TS  <- a /(a+b+c)
POD<- H <- a /(a+c)  ## hit rate
F   <- b /(b+d)  ## false alarm rate
M   <- c /(a+c)  ## miss rate
FAR <- b/(a+b)  ## false alarm ratio
HSS <- 2*(a*d - b*c)/ (1.0*(a+c)*(c+d) + 1.0*(a+b)*(b+d))  
PSS <- 1 - M  - F ## Pierce skill score
KSS <- (a*d - b*c)/((a+c)*(b + d)) ## similar to Pierc
PC <- (a+d)/(a+b+c+d)
BIAS <- (a+b)/(a+c)
OR   <- a*d/(b*c) ## odds ratio
ORSS <- (a*d - b*c)/ (a*b + b*c ) ## odds ratio skill score
HITSrandom <- 1.0* (a+c)*(a+b)/(a+b+c+d)
p <- (a+c)/n

ETS <- (a-HITSrandom)/(a+b+c-HITSrandom)
theta <- (a*d)/(b*c)
log.theta <- log(a) + log(d) - log(b) - log(c) 
n.h <- 1/( 1/a + 1/b + 1/c + 1/d)
yules.q <- (theta - 1)/(theta + 1)  
eds <- 2*log((a+c)/n)/log(a/n) - 1
seds <- (log((a+b)/n)+log((a+c)/n)) /log(a/n) - 1
seds.se <- sqrt(H*(1-H)/(n*p)) *(-log(BIAS*p^2)/(H*log(H*p)^2))

return(list(tab = tab.out, TS = TS,
            POD = POD, M = M,  F = F,  FAR = FAR , HSS = HSS,
            PSS = PSS, KSS = KSS,
            PC = PC, BIAS = BIAS, ETS = ETS, theta = theta, log.theta = log.theta, n.h = n.h, orss = yules.q, eds = eds, seds = seds, seds.se = seds.se))
}


