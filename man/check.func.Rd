\name{check.func}
\alias{check.func}
\title{check loss function}
\description{Calculates the check loss function.}
\usage{check.func(u, p)}
\arguments{
  \item{u}{Value to be evaluated}
  \item{p}{Probability level [0,1]}
}
\details{
The check loss is calculated as \eqn{\rho_{p} (u) = (abs(u) + (2*p-1)*u)/2}.
}
\value{
The check loss for value u and probability level p.
}
\author{Sabrina Bentzien}
\note{This function is used within \code{quantileScore}.}

\examples{
## The function is currently defined as
function (u, p) 
{
    rho <- (abs(u) + (2 * p - 1) * u) * 0.5
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
