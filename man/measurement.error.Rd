% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
% ** Copyright UCAR (c) 1992 - 2004 
% ** University Corporation for Atmospheric Research(UCAR) 
% ** National Center for Atmospheric Research(NCAR) 
% ** Research Applications Program(RAP) 
% ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
% ** 2004/9/1 14:13:29 
% *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
\name{measurement.error}
       \alias{measurement.error}
        \title{Skill score with measurement error.}
       \description{Skill score that incorporates measurement error.
	 This function allows the user to incorporate measurement error
	 in an observation in a skill score.}
       \usage{measurement.error( obs, frcs = NULL, theta = 0.5, t = 1, u
= 0, h = NULL, \dots)
       }
       \arguments{
	 \item{obs}{Information about a forecast and observation can be
	   done in one of two ways.  First,  the results of a
	   contingency table can be entered as a vector containing
	   c(n11, n10, n01, n00), where n11 are the number of correctly
	   predicted events and n01 is the number of incorrectly
	   predicted non-events.  Actual forecasts and observations can
	   be used.  In this case, obs is a vector of binary outcomes
	   [0,1].}
	 \item{frcs}{If obs is entered as a contingency table, this
	   agrument is null.  If obs is a vector of outcomes, this
	   column is a vector of probablistic forecasts.}
	 \item{theta}{Loss value (cost) of making a incorrect forecast
	   by a non-event. Defaults to 0.5.}
	 \item{t}{Probability of forecasting an event, when an event
	   occurs.  A perfect value is 1.}
	 \item{u}{Probability of forecasting that no event will occur,
	   when and event occurs.  A perfect value is 0.}
	 \item{h}{Threshold for converting a probablistic forecast into
	   a binary forecast.  By default, this value is NULL and the
	   theta is used as this threshold.}
	 \item{\dots}{Optional arguments.}
       }
       \value{
	 \item{G}{Likelihood ratio statistic}
	 \item{p}{p-value for the null hypothesis that the forecast
	   contains skill.}
	 \item{K}{Skill score.}
       }
       \examples{
DAT<- data.frame( obs = round(runif(50)), frcs = runif(50))

 measurement.error(DAT$obs, DAT$frcs)

### Finley Data

measurement.error(c(28, 23, 72, 2680)) ## assuming perfect observation,
                                       
	 }

	 
	 \references{
W.M. Briggs, 2004.  \emph{Incorporating Cost in the  Skill Score}
Technical Report, wm-briggs.com/public/skillocst.pdf. 

W.M. Briggs and D. Ruppert, 2004. \emph{ Assessing the skill
  of yes/no forecasts. Submitting to Biometrics}.

J.P. Finley, 1884. Tornado forecasts. {\emph Amer. Meteor. J.}
85-88. (Tornado data used in example.)
}


\author{Matt Pocernich <pocernic@rap.ucar.edu> (R - code)

  W.M Briggs <wib2004(at)med.cornell.edu> (Method questions) }

       \keyword{file}
