% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2004 
% ** University Corporation for Atmospheric Research(UCAR) 
% ** National Center for Atmospheric Research(NCAR) 
% ** Research Applications Program(RAP) 
% ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
% ** 2004/1/7 11:31:8 
% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
\name{performance.diagram}
       \alias{performance.diagram}
       \title{Performance Diagram}
       \description{Creates plot displaying multiple skill scores on a single plot}
       \usage{ performance.diagram(...) }
      \arguments{
    \item{...}{Optional plotting parameters.}
    }
 \note{Currently this function just produces the base plot.  Points summarizing model performance can be added using the points function. 
 	
 	}
       \examples{
performance.diagram(main = "Sample Plot")
RB1 <- matrix(c(95, 55, 42, 141), ncol = 2)
## at point
pts     <- table.stats(RB1)
boot.pts <- table.stats.boot(RB1, R = 1000   )
## add confidence intervals
segments(x0 = 1 - pts$FAR, y0 = boot.pts["up","pod"], x1 = 1 - pts$FAR, y1 = boot.pts["dw", "pod"], col = 2, lwd = 2)

segments(x0 = 1  - boot.pts["up" ,"far"], y0 = pts$POD, x1 = 1 - boot.pts["dw", "far"], y1 = pts$POD, col = 2, lwd = 2)
points(1 - pts$FAR, pts$POD, col = 2, cex = 2)


}


\references{

Roebber, P.J. (2009). Visualizing Multiple Measures of Forecast Quality, \emph{Weather and Forecasting}. \bold{24}, pp - 601 - 608.
 
}

\author{Matt Pocernich <pocernic@rap.ucar.edu>}

       \keyword{file}
