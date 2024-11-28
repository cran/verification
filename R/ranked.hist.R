## ranked histogram plot.
ranked.hist <- function(frcst, nbins = 10, titl = NULL){

if( min(frcst) < 0 | max(frcst) > 1 ){warning("Observations outside of [0,1] interval. \n")}

brks<-  seq(0,1, length = nbins + 1)
hist(frcst, breaks = brks, main = "")
if(is.null(titl)){title("Ranked Histogram")}else
{title(titl)}
abline(h = length(frcst)/nbins, lty = 2)
invisible()

} # close function
