# For any value of the concentration parameter larger than this, we need to use a simple approximation
# because the numerical integration routine does not converge



dvm2 <- function(theta, mu, kappa)
{
	return( dvm(theta, mu, kappa) )
 
	out <- vector(length=length(theta), mode="numeric")
	for(i in 1:length(theta))
	{
		out[i] <- dvm(theta[i], mu, kappa)
	}
	return(out)
}

angle.dist <- function(a, b)
{
	d <- abs(b - a)
	return(pmin(d, 2*pi - d))
}

# An approximation for large values of the concentration parameter.

approx.comp2 <- function(x, mu, kappa)
{
  sigma <- 1/sqrt(kappa)
  alpha <- pmin(abs(x-mu),2*pi-abs(x-mu))
  return( 2*sigma*dnorm(alpha/sigma) + alpha*(2*pnorm(alpha/sigma)-1) )
}

# PARAMETERS:
# x: a single number or a numeric vector
# mu: a single number
# k: a single, positive number
#
# RETURN VALUE:
# E alpha(X,x) where X is an independent draw from a VonMises distribution with concentration parameter
# k and mean mu.

ES.VM.comp2 <- function(x, mu, k,...)
{
	if(is.array(x) || is.vector(x))
	{
		return(ES.VM.comp2.array(x, mu, k))
	}
	
	x <- x %% (2*pi) ## ensure between 0,2*pi
	mu <- mu %% (2*pi)
    
	if(k > ES.VM.Comp2.Max.Kappa){
		return(approx.comp2(x, mu, k))
        }
 
	foo1 <- function(z)
	{
		tmp <- dvm2(theta=z, mu=mu, kappa=k)*angle.dist(x, z)
		return(tmp)
	}
 
	res <- integrate(f=foo1, lower=0, upper=2*pi)
	return( res$value ) 
}

ES.VM.comp2.array <- function(x, mu, k, ...)
{
	mu <- mu %% (2*pi)
	x <- x %% (2*pi)

	if(k > ES.VM.Comp2.Max.Kappa){
		return(approx.comp2(x, mu, k))
	}
 
	foo1 <- function(z, ind)
	{
		tmp <- dvm2(theta=z, mu=mu, kappa=k)*angle.dist(x[ind], z)
		return(tmp)
	}
       
	out <- vector(mode="numeric", length=length(x))
	for(j in 1:length(x))
	{
		res <- integrate(f=foo1, lower=0, upper=2*pi, ind=j)
		out[j] <- res$value
	}
	return(out)
}
