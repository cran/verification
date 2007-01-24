multi.cont <- function(DAT){ # assume DAT is a contingency table with col = obs
                             # and rows = forcasts.
  
P.DAT  <- DAT/sum(DAT)
p.diag <- diag(P.DAT)

PC     <- sum(diag(P.DAT)) ## percent correct

p.forc <- apply(P.DAT, 1, sum)
p.obs  <- apply(P.DAT, 2, sum)

######################### Gerrity
K<- nrow(DAT)
S<- matrix(NA, ncol = nrow(DAT), nrow = nrow(DAT) )
kap <- 1/(K-1)

D<- function(n){
out <- numeric()  
for(i in  1:length(n) ){ 
out[i]<- if(n[i] ==0) 0 else
         (1-sum(p.obs[1:n[i]])) / sum(p.obs[1:n[i]])  }
out
}

R<- function(n){
out <- numeric()
for(i in  1:length(n) ){ 
out[i]<-1/D(n[i])}
out
}


ind<- 1
for(i in 1:K ){
for(j in ind:K ){

  if(1 > (i-1) ) aa<- 0 else aa<- sum(R(1:(i-1) ))
  if(i > (K-1) ) bb<- 0 else bb<- sum(D(i:(K-1) ))
if(i == j){
  S[i,j] <- kap*(aa + bb )
}
if(i != j) {
if(j > (K-1) ) bb<- 0 else bb<- sum(D(j:(K-1) ))  
S[i,j] <- kap*(aa - (j - i) +  bb )
}
} # close j
ind<- ind + 1
} # close i

## fill in bottom
ind<- K- 1

for(i in K:2){
  for(j in 1:ind){
S[i,j] <- S[j,i]
  }
ind <- ind - 1
}

GS<- sum(P.DAT * S )


#########################

bias <- p.forc/p.obs

pod <- diag(P.DAT)/p.obs
far <- (p.forc - p.diag) / p.forc

d<- numeric()
for(i in 1:nrow(P.DAT) ){
  d[i] <- sum(P.DAT[-i,-i])
}
ts <- p.diag/(1 - d)

hss <- (sum(p.diag) - sum (p.obs*p.forc)  ) / (1 - sum (p.obs*p.forc)  )
pss <- (sum(p.diag) - sum (p.obs*p.forc)  ) / (1 - sum (p.obs*p.obs)  )

return(list(PC = PC, BIAS = bias, hit.rate = pod, false.alarm.ratio = far, TS = ts,
            HSS = hss, PSS = pss, GS = GS))
}


