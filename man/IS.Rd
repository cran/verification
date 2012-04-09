% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2004 
% ** University Corporation for Atmospheric Research(UCAR) 
% ** National Center for Atmospheric Research(NCAR) 
% ** Research Applications Program(RAP) 
% ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
% ** 2004/1/7 11:31:8 
% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
\name{IS}
       \alias{IS}
       \title{Alternative Intensity Scale Function}
       \description{Beta version of intensity scale function created by 
       Barbara Casati.}
       \usage{ IS(frcs, obs, thres) }
      \arguments{
      \item{frcs}{Forecast matrix.  Must be of $2^n$ dimensions.}
    \item{obs}{Observation matrix. Must be of $2^n$ dimensions.}
    \item{thres}{A vector of thresholds to be considered.  By
      default, the percentiles 0, 90 are used. }
    }
\value{
     SSul = SSul, MSEul = MSEul, l.frcs = dim(frcs)[1],
            thres = thres, Bias = Bu, BR = BRu
     \item{SSul}{Skill score as matrix.  The rownames are the
       thresholds, the colnames are $n$ where $2^n$ is the spatial
       scale of the skill score decomposition.}
     \item{MSEul}{A matrix with the mean squared error of the
       forecast}
     \item{l.frcs}{Number of rows in forecast.  Used in plotting
       routine.}
     \item{thres}{Thresholds used in model}
     \item{Bias}{Bias}
     \item{BR}{BR}
           }
\note{ THIS IS A DRAFT FORM OF THIS FUNCTION.  IT MAY CHANGE AND REPLACE int.scale.verify }

       \seealso{\code{\link{int.scale.verify}} and \code{\link{plot.int.scale}}}
       \examples{
#####################################################
# files.dat: read, create and write
######################################################

IS.NIMROD.case <- IS(forecast.dat, analysis.dat,c(0, 2^seq(-5,6)))
NIMROD.SSul <- IS.NIMROD.case$SSul
colnames(NIMROD.SSul) <- paste(c("0","1/32", "1/16", "1/8", "1/4", "1/2", "1",
                "2","4", "8", "16", "32", "64"),"mm/h")
rownames(NIMROD.SSul) <- paste(5*2^seq(0,8),"km")
# write.table(NIMROD.SSul,file="NIMROD.SSul.dat")

NIMROD.MSEul <- IS.NIMROD.case$MSEul
colnames(NIMROD.MSEul) <- paste(c("0","1/32", "1/16", "1/8", "1/4", "1/2", "1",
                "2","4", "8", "16", "32", "64"),"mm/h")
rownames(NIMROD.MSEul) <- paste(5*2^seq(0,8),"km")

###################################################
# colorbars for the images
###################################################

Nimrod.colorbar <- function(){
colors = c(0,8,8,8,8,5,5,4,4,4,2,2,2)
xlimbar = c(7,8)
ylimbar = c(46.5,59.5)
barlabels = c("0","1/32", "1/16", "1/8", "1/4", "1/2", "1", "2", "4", "8", "16",
"32", "64","128")
ycoord = seq(ylimbar[1],ylimbar[2],length=length(colors)+1)
for(i in seq(1,length(colors))){
polygon(x=c(xlimbar[1],xlimbar[1],xlimbar[2],xlimbar[2],xlimbar[1]),
y=c(ycoord[i],ycoord[i+1],ycoord[i+1],ycoord[i],ycoord[i]),
col = colors[i])}
axis(4,at=ycoord,labels=barlabels,las=TRUE)
mtext("mm/h",line=1,at=c(7.5,61),cex=1.5)
}

colorbar <- function(xlimbar,ylimbar,colors,barlabels){
ycoord = seq(ylimbar[1],ylimbar[2],length=(length(colors)+1))
for(i in seq(1,length(colors))){
polygon(x=c(xlimbar[1],xlimbar[1],xlimbar[2],xlimbar[2],xlimbar[1]),
y=c(ycoord[i],ycoord[i+1],ycoord[i+1],ycoord[i],ycoord[i]),
col = colors[i])}
axis(4,at=ycoord,labels=barlabels,las=TRUE)
}

#  images
#
   par(oma=c(3,3,3,3), mfrow = c(1,1) )
   image(seq(-12,8,length=256), seq(46.5,59.5,length=256), analysis.dat,
       xlim = c(-12,8), ylim = c(46.5,59.5), zlim = c(0,128),
       xlab = "longitude", ylab = "latitude",main="Nimrod analysis 29/05/99 15:00",
       col=c(0,8,8,8,8,5,5,4,4,4,2,2,2),breaks=c(0,2^seq(-5,7,1)))
   world(xlim=c(-12,8),ylim=c(46.5,59.5), add = TRUE, lwd = 3)
   Nimrod.colorbar()

#
   par(oma=c(3,3,3,3))
   image(seq(-12,8,length=256), seq(46.5,59.5,length=256), forecast.dat,
       xlim = c(-12,8), ylim = c(46.5,59.5), zlim = c(0,128),
       xlab = "longitude", ylab = "latitude",main="Nimrod forecast T+3h",
       col=c(0,8,8,8,8,5,5,4,4,4,2,2,2),breaks=c(0,2^seq(-5,7,1)))
   world(xlim=c(-12,8),ylim=c(46.5,59.5), add = TRUE, lwd = 3)
   Nimrod.colorbar()
#
    par(oma=c(3,3,3,3))
    image(seq(-6,6,1),seq(1,9),t(NIMROD.SSul),xlim=c(-6.5,8.5),zlim=c(-4,1),
        xlab="threshold (mm/h)",ylab="scale (km)",axes=FALSE,
        main="Intensity-Scale Skill Score", col=c(4,4,4,4,5,5,5,5,7,7))
    axis(1,at = seq(-6,6,1), labels = c("0","1/32", "1/16", "1/8", "1/4", "1/2",
"1", "2","4", "8", "16", "32", "64"))
    axis(2,at = seq(1,9),labels=5*2^seq(0,8,1))
    rect(xleft=-6.5, ybottom=0.5, xright=6.5, ytop=9.5)

colorbar(xlimbar = c(7.5,8.5), ylimbar = c(0.5,9.5),
    colors = c(4,4,4,4,5,5,5,5,7,7), barlabels = seq(-4,1,0.5))
}


\references{
Casati et al (2004), A new intensity-scale approach for 
the verification of spatial precipitation forecasts, Meteorol. Appl, vol 
11, 141-154 pp. 
}

\author{Barbara Casati <barbara.casati@ec.gc.ca>}

       \keyword{file}
