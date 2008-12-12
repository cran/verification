% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2004 
% ** University Corporation for Atmospheric Research(UCAR) 
% ** National Center for Atmospheric Research(NCAR) 
% ** Research Applications Program(RAP) 
% ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
% ** 2004/1/7 11:31:8 
% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
\name{discrimination.plot}
\alias{discrimination.plot}
\title{Discrimination  plot}
       \description{This function creates a plot of discrimination plots
     (overlay histograms).   In the context of verification, this is often
     used to compare the distribution of event and no-event
     forecasts.  This may be useful in comparing any set of
     observations. By default, boxplots of groups appear as upper
     marginal plots.  These may be surpressed. }
       \usage{discrimination.plot(group.id, value, breaks = 11, main =
"Discrimination Plot", xlim = NULL, ylim = NULL,  legend =
FALSE, leg.txt = paste("Model", unique(group.id) ),   marginal = TRUE, cols =
seq(2, length(unique(group.id)) + 1), xlab = "Forecast",  \dots )}

\arguments{
         \item{group.id}{A vector identifying groups.  A histogram is
       created for each unique value.}
     \item{value}{A vector of values corresponding to the group.id
       vector used to create the histograms}
     \item{breaks}{Number of breaks in the x-axis of the histogram.
     The range of values is taken to be the range of prediction values.}
     \item{main}{Title for plot.}
     \item{xlim}{Range of histogram - x axis - main plot coordinates.}
     \item{ylim}{Range of histogram - y axis - main plot coordinates.}
     \item{legend}{Should there be a legend? Default = FALSE}
     \item{leg.txt}{Legend text.  If FALSE or if a marginal plot is
       created, no legend is added.}
     \item{cols}{A vector showing the colors to be used in the
       histograms and in the marginal boxplots}
     \item{marginal}{Should a boxplots be placed in the top margin?
       Defaults to TRUE}
     \item{xlab}{ Label of the x-axis on the main plot.}
     \item{\dots}{Additional plotting options.}
       }

        \examples{
 #  A sample forecast.  

data(disc.dat)
discrimination.plot(disc.dat$group.id, disc.dat$frcst, main = "Default  Plot")

discrimination.plot(disc.dat$group.id, disc.dat$frcst, main = "New Labels", cex = 1.2, leg.txt = c("Low", "Med", "High" ) )

discrimination.plot(disc.dat$group.id, disc.dat$frcst, main = "Without Marginal Plots ", marginal = FALSE)


 }

\author{Matt Pocernich <pocernic@rap.ucar.edu>}

       \keyword{file}
