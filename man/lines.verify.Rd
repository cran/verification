\name{lines.verify}
       \alias{lines.roc}
       \alias{lines.attrib}
       \title{Add lines to ROC or attribute diagrams}
       \description{Add lines to attribute and verification diagrams from verify.prob.bin objects.}
       \usage{
       \method{lines}{roc}(x,binormal = FALSE, ...)
        \method{lines}{attrib}(x,...)
       }     
       
       \arguments{
    \item{x}{An object created by the verify function with the prob.bin class}
    \item{binormal}{Logical value indicating whether the lines to be added to a ROC plot are empirical or a binormal fit. }
    \item{...}{Optional arguments for the lines function.  These include color, line weight (ltw) and line stype (lty) }
    }
       \note{ This will soon be replaced the a lines command constructed using S4 class properites. }

       \seealso{\code{\link{verify}}  }
       

\author{Matt Pocernich}

       \keyword{file}
