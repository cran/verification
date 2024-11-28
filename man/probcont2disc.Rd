\name{probcont2disc}
       \alias{probcont2disc}
       \title{Converts continuous probability values into binned
     discrete probability forecasts.}
       \description{Converts continuous probability values into binned
     discrete probability forecasts.  This is useful in calculated
     Brier type scores for values with continuous probabilities.
     Each probability is assigned the value of the midpoint. }
       \usage{
    probcont2disc(x, bins = seq(0,1,0.1) )
       }
      \arguments{
    \item{x}{A vector of probabilities}
    \item{bins}{Bins.  Defaults to 0 to 1 by 0.1 .}
       }
       \value{
A vector of discrete probabilities.  E
       }
       \note{This function is used within \code{brier}.}
     \examples{

#  probabilistic/ binary example

set.seed(1)
x <- runif(10) ## simulated probabilities.

probcont2disc(x)
probcont2disc(x, bins = seq(0,1,0.25) )

## probcont2disc(4, bins = seq(0,1,0.3)) ## gets error

}
\author{Matt Pocernich}

       \keyword{file}
