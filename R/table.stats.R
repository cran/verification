# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
# ** Copyright UCAR (c) 1992 - 2004 
# ** University Corporation for Atmospheric Research(UCAR) 
# ** National Center for Atmospheric Research(NCAR) 
# ** Research Applications Program(RAP) 
# ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
# ** 2004/1/7 11:29:42 
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
table.stats<- function(obs, pred){
## internal function used in verify
## used with a binary forecast and a binary outcome.
  
  tab.out <- table(obs, pred)

  a<-tab.out["0","0"]
  b<-tab.out["0","1"]
  c<-tab.out["1","0"]
  d<-tab.out["1","1"]

    
TS<- a/(a+b+c)
POD<- a/(a+c)
FAR <- b/(a+b)
HSS <- 2*(a*d - b*c)/ ((a+c)*(c+d) + (a+b)*(b+d))  
KSS <- (a*d - b*c)/((a+c)*(b + d))
PC <- (a+d)/(a+b+c+d)
BIAS <- (a+b)/(a+c)

HITSrandom <- (a+c)*(a+b)/(a+b+c+d)
ETS <- (a-HITSrandom)/(a+b+c-HITSrandom)

return(list(tab = tab.out, TS = TS,
            POD = POD, FAR = FAR , HSS = HSS,KSS = KSS,
            PC = PC, BIAS = BIAS, ETS = ETS))
}


