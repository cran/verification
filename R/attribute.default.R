# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
# ** Copyright UCAR (c) 1992 - 2004 
# ** University Corporation for Atmospheric Research(UCAR) 
# ** National Center for Atmospheric Research(NCAR) 
# ** Research Applications Program(RAP) 
# ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
# ** 2004/1/7 11:29:42 
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
 attribute.default<- function(x, obar.i,  prob.y=NULL, obar = NULL, titl = NULL, ...){
## attribute plot as displayed in Wilks, p 264.
## If the first object is a prob.bin class, information derived from that.

   old.par <- par(no.readonly = TRUE) # all par settings which
                                      # could be changed.
   on.exit(par(old.par))


plot(x, obar.i,  col = 2, lwd = 2, type = "n",
     xlim = c(0,1), ylim = c(0,1),
     xlab =  expression( paste("Forecast probability, ", y[i] ) ),
     ylab = expression( paste("Observed relative frequency, ", bar(o)[1] ))
     )


###################  need to put down shading before anything else.

if(!is.null(obar)){
a <- (1-obar)/2 + obar
b <- obar / 2
x.p<- c(obar, obar, 1, 1, 0, 0)
y.p<- c(0, 1, 1, a, b, 0)

polygon(x.p, y.p, col = "gray")

text(0.6, obar + (a-b)*(0.6 - obar), "No skill", pos = 1,
     srt = atan( a - b )/(2*pi)*360 )

}


###########

points(x, obar.i, type = "b", col = 2, lwd = 2)


## plot relative frequency of each forecast
ind<- x< 0.5
text(x[ind], obar.i[ind], formatC(prob.y[ind], format = "f", digits = 3),
          pos = 3, offset = 2, srt = 90)
text(x[!ind], obar.i[!ind], formatC(prob.y[!ind], format = "f", digits = 3),
          pos = 1, offset = 2, srt = 90)
if(is.null(titl)){title("Attribute Diagram")}else
{title(titl)}

abline(0,1)

## resolution line
if(!is.null(obar)){
abline(h = obar, lty = 2)
abline(v = obar, lty = 2)
text( 0.6, obar, "No resolution", pos = 3)
}

invisible()
}
