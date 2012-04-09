
\name{table.stats.boot}
       \alias{table.stats.boot}
       \title{Percentile bootstrap for 2 by 2 table}
       \description{ Performs a bootstrap on data from a 2 by 2 contingency table returning verification statistics.  Potentially useful in creating error bars for performance diagrams.
       	 }
       \usage{
      table.stats.boot(CT, R = 100, alpha = 0.05)
       }
     \seealso{\code{table.stats}}
      \arguments{
	 \item{CT}{Two by two contingency table.  Columns summarize observed values.  Rows summarize forecasted values.}
	 \item{R}{Number of resamples}
     \item{alpha}{Confidence intervals.}
	        }
       \value{
	 2 row matrix with upper and lower intervals for bias, pod, far, ets.
	 }
 \examples{
 	
### example from Roebber. 	
RB1 <- matrix(c(95, 55, 42, 141), ncol = 2)
table.stats.boot(RB1, R = 1000   )

}

\author{Matt  Pocernich}
