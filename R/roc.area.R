# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
# ** Copyright UCAR (c) 1992 - 2004 
# ** University Corporation for Atmospheric Research(UCAR) 
# ** National Center for Atmospheric Research(NCAR) 
# ** Research Applications Program(RAP) 
# ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
# ** 2004/1/7 11:28:18 
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
roc.area <- function(obs, pred){ 

  A<- data.frame(obs, pred)
  names(A)<- c("obs", "pred")

####

e   <- sum(A$obs == 1)
e.p <- sum(A$obs == 0)
n   <- length(A$obs)

####

  o2      <- order(A$pred, A$obs, decreasing = TRUE) # order for f
  DAT     <- A[o2,]
  DAT$ind <- seq(1,n)
  ind.2<- DAT$ind[DAT$obs == 1]

f<- 0 # no ties. 

for(i in 1:e){
  d<-   sum(DAT[1:ind.2[i],]$obs == 0)
  f<-  f +  d
}

#f.tilda - ties include
  o1 <- order(A$pred, -A$obs, decreasing = TRUE) # order for f
  DAT<- A[o1,]
  DAT$ind<- seq(1,n)
  ind.2<- DAT$ind[DAT$obs == 1]
  f.tilda <- 0

for(i in 1:e){
  d<-   sum(DAT[1:ind.2[i],]$obs == 0)
  f.tilda <-  f.tilda  +  d
}

A.tilda<- 1 - 1/(e*e.p)*f - 1/(2*e*e.p)*(f.tilda - f)
### create alternative p-value using wilcox.test
stats <- wilcox.test(pred[obs==1], pred[obs==0], alternative = "great")  
  
return(list(A = A.tilda, n.total = n, n.events = e, n.noevents = e.p,   p.value = stats$p.value) )
     }  # close function
