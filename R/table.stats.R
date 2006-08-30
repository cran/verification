# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
# ** Copyright UCAR (c) 1992 - 2004 
# ** University Corporation for Atmospheric Research(UCAR) 
# ** National Center for Atmospheric Research(NCAR) 
# ** Research Applications Program(RAP) 
# ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
# ** 2004/1/7 11:29:42 
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
table.stats<- function(obs, pred = NULL){
## internal function used in verify
## used with a binary forecast and a binary outcome.
if(is.null(pred) & length(obs) ==4 ){
  print(" Assume data entered as c(n11, n01, n10, n00) Obs*Forecast")
a <- obs[1]
b <- obs[2]
c <- obs[3]
d <- obs[4]
tab.out <- matrix(c(a,c,b,d), nrow = 2)
} else{

  tab.out <- table(obs, pred)
  
  a <-tab.out["1","1"]
  b <-tab.out["0","1"]
  c <-tab.out["1","0"]
  d <-tab.out["0","0"]
} ## close else

### data from Thornes
#a<- 29
#b<- 6
#c <- 4
#d<- 38
#cl <- c(0.1, 0.125, 0.2, 0.4, 0.6, 0.8, 1)##

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
ETS <- (a-HITSrandom)/(a+b+c-HITSrandom)
  
return(list(tab = tab.out, TS = TS,
            POD = POD, M = M,  F = F,  FAR = FAR , HSS = HSS,
            PSS = PSS, KSS = KSS,
            PC = PC, BIAS = BIAS, ETS = ETS))
}


