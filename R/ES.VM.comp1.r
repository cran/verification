## aux functions for crps.circ

angle.dist <- function(a, b)
{
	d <- abs(b - a)
	return(pmin(d, 2*pi - d))
}
### 
calc.ES.VM.comp1 <- function(kappa, mc.size)
{
	score.comp1 <- mean(angle.dist(
          rvm(mc.size, mean=pi, k=kappa),
                                       rvm(mc.size, mean=pi, k=kappa) ))
	return( score.comp1 ) 
}

# PARAMETERS:
# kappa: a single, positive number
#
# RETURN VALUE:
# E alpha(X,X') where X and X' are independent draws from a VonMises distribution with concentration parameter
# kappa and mean of 0.  Opens a lookup table with path set in global variable 'GLOBAL.lookup.path'
# Uses linear interpolation between values in lookup table.  For values beyond the range of the lookup table,
# uses the normal approximation that  E|X-X'| ~ 2/sqrt(pi*kappa), which is accurate to three decimal places
# for kappa greater than 200.

ES.VM.comp1 <- function(kappa)
{
#	if(ES.VM.table.loaded == FALSE)
#        {
#		load(file=GLOBAL.lookup.path )
#		ES.VM.table.loaded <- TRUE
#	}

  data(lookup)
        
	len <- nrow(lookup)
	low <- 0
	high <- len
       

          if(kappa >= lookup[len,1])
	{
		return(2/sqrt(pi*kappa))
	}
	else if(kappa <= lookup[1,1])
	{
		rem <- (lookup[1,1] - kappa)/(lookup[1,1])
		return( lookup[1,2]*(1-rem) + (pi/2)*rem )
	}
	while(1 == 1)
	{
		if(high - low <= 1)
		{
			rem <- (kappa - lookup[low,1])/(lookup[low + 1,1] - lookup[low,1])
			return( lookup[low,2]*(1-rem) + lookup[low + 1,2]*rem )
		}
		i <- floor((low + high)/2)
		if(kappa < lookup[i,1])
			high <- i
		else
			low <- i
	}

}

# PARAMETERS:
# kappa: a numeric vector of positive numbers
#
# DEPENDENCIES:
# calls the function 'ES.VM.comp1' for each element of the parameter, kappa
#
# RETURN VALUE:
# returns a numeric vector whose ith element is ES.VM.comp1(kappa[i])

ES.VM.comp1.array <- function(kappa)
{
	out.v <- vector(mode="numeric", length=length(kappa))
	for(i in 1:length(kappa))
	{
		out.v[i] <- ES.VM.comp1(kappa[i])
	}
	return( out.v )
}
