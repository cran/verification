# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
# ** Copyright UCAR (c) 1992 - 2004 
# ** University Corporation for Atmospheric Research(UCAR) 
# ** National Center for Atmospheric Research(NCAR) 
# ** Research Applications Program(RAP) 
# ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
# ** 2004/1/7 11:29:42 
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
"discrimination.plot" <- function(obs, pred, breaks = 11, main = "Comparison of Distributions",  xlim = c(0,1), median = TRUE, ... ){
 old.par <- par(no.readonly = TRUE) # all par settings which
                                      # could be changed.
 #  on.exit(par(old.par))


  y<- pred[obs==1]
n<- pred[obs==0]


Y<- hist(y, plot = FALSE,
         xlim = xlim, breaks = breaks)

N<- hist(n, plot = FALSE,
         xlim = xlim, breaks = breaks)


plot(Y$mids, Y$counts/sum(Y$counts), type = "b", col = "red", pch = 16,
     xlab = "Forecast", ylab = "Density", xlim = c(0,1), ylim = c(0, 1), main = main, ...)

#axis(4, at= c(0, length(y))   )

#plot(Y$mids, Y$counts, type = "b", col = "red", pch = 16,
#     xlab = "Forecast", ylab = "Density", xlim = c(0,1), ylim = c(0,1) )


points(N$mids, N$counts/sum(N$counts), type = "b", col = "blue", pch = 17)


abline(h = 0); abline(v=0)

leg.txt <- c("Event", "No Event")
  legend(list(x=.8,y=.8), legend = leg.txt, col= c("red", "blue"), pch = c(16, 17),
         lty=1, merge=TRUE, cex = 0.6)
if(median){
abline(v = median(y), col =2)
abline(v = median(n), col ="blue")
}

}
