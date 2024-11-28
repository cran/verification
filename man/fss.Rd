\name{fss}
       \alias{fss}
       \title{Fractional Skill Score}
       \description{Calculates the fractional skill score for spatial forecasts and spatial observations.}
       \usage{
    fss(obs, pred,w = 0, FUN = mean, ...)
       }
      \arguments{
	\item{obs}{A matrix of binomial observed values.}
	\item{pred}{A matrix of binomial forecasted values}
	\item{w}{ Box width.  When w = 0, each pixel is considered alone.  w = 2 creates a box with a length of 5  units.}
	\item{FUN}{Function to be applied to each subgrid.  By default, the mean of each box is used to calculate the fraction of each subgrid. }
	\item{\dots}{Optional arguments}
       }
       \value{
	 Return the fraction skill score.}
       \note{This function contain several loops and consequently is not particularly fast.}
      
	 \examples{
grid<- list( x= seq( 0,5,,100), y= seq(0,5,,100)) 
obj<-Exp.image.cov( grid=grid, theta=.5, setup=TRUE)
look<- sim.rf( obj)
look[look < 0] <- 0

look2 <- sim.rf( obj)
look2[look2<0] <- 0

fss(look, look2, w=5)


\dontrun{
#  The following example replicates Figure 4 in Roberts and Lean (2008).
####      examples

LAG <- c(1,3,11,21)
box.radius <- seq(0,24,2)

OUT <- matrix(NA, nrow = length(box.radius), ncol = length(LAG) )

for(w in 1:4){

FRCS <- OBS <- matrix(0, nrow = 100, ncol = 100)

obs.id        <- 50
OBS[,obs.id]  <- 1

FRCS[, obs.id + LAG[w]] <- 1

for(i in 1:length(box.radius)){

OUT[i, w] <- fss(obs = OBS, pred = FRCS, w = box.radius[i] )

} ### close i
} ### close w

b <- mean(OBS) ## base rate

fss.uniform  <- 0.5 + b/2
fss.random   <- b

matplot(OUT, ylim = c(0,1), ylab = "FSS", xlab = "grid squares", 
type = "b", lty = 1, axes = FALSE , lwd = 2)

abline(h = c(fss.uniform, fss.random), lty = 2)  
axis(2)
box()
axis(1, at = 1:length(box.radius), lab = 2*box.radius + 1)
grid()

legend("bottomright", legend = LAG, col = 1:4, pch = as.character(1:4), 
 title = "Spatial Lag", inset = 0.02, lwd = 2 )
}

}


\references{Roberts, N.M and H.W. Lean: 2008: Scale-Selective Verification of Rainfall Accumulations from High-Resolution Forecasts of Convective Events.  \emph{Monthly Weather Review} \bold{136}, 78-97.}
  
  \author{Matt Pocernich}

       \keyword{file}
