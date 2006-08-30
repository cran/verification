# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
# ** Copyright UCAR (c) 1992 - 2004 
# ** University Corporation for Atmospheric Research(UCAR) 
# ** National Center for Atmospheric Research(NCAR) 
# ** Research Applications Program(RAP) 
# ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
# ** 2004/1/7 11:29:42 
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
plot.int.scale <- function(x,y = NULL, plot.mse = FALSE, main = NULL,...){

  ## x is an object of int.scale class
  ## stupid manipulation to plot matrix

if(plot.mse != TRUE){DAT <- x$SSul
                   if(is.null(main)) {main <- "Intensity Scale Verification" } }
              else {DAT<- x$MSE

                   if(is.null(main)) {main <- "MSE Intensity Scale Verification" }             }

  P<- t(DAT)

m <- dim(P)[1]
n <- dim(P)[2]
o<- x$l.frcs
  
# image of the skill score as function of threshold and scale
	
par(mar = c(5,5,4,4))          
          image(x =seq(1,m ) , y = seq(1, n), P, axes = FALSE, zlim = range(P), 
			xlab="Threshold",ylab="spatial scale:  2^n", xaxs = "i", yaxs = "i",
                col = topo.colors(256), main = main, ...)

box()
		axis(1,at=seq(1, m),labels=round(x$thres,2))
		axis(2,at=seq(1,log2(o)+1),labels=c(seq(1,log2(o)),"bias"))

image.plot( zlim = range(P), legend.only=TRUE, offset = 0.02, legend.width = 0.03,
           horizontal=FALSE, add=TRUE)

}
