%% Contributed by  Author: Ronald Frenette, Severe Weather Lab, Quebec region
%% #         Jun 2009
  
\name{crpsDecompostion}
       \alias{crpsDecomposition}
       \alias{crpsFromAlphaBeta}
       \title{Decompostion of Continuous Ranked Probability Score}
       \description{The CRPS measures the distance between the predicted and the observed cumulative density functions (CDFs) of scalar variables. Furthermore, the crpsDecomposition function provides the reliability and resolution terms obtained by the CRPS decomposition proposed by Hersbach. The alpha, beta matrices and Heavisides vectors of outliers calculated in the CRPS decomposition are also returned. To speed up calculation time, these matrices/vectors can then be used to recalculate the CRPS's in a bootstrap by using the crpsFromAlphaBeta function.}
       \usage{
      crpsDecomposition(obs, eps)
      crpsFromAlphaBeta(alpha,beta,heaviside0,heavisideN)
       }
      
      \arguments{
	 \item{obs}{Vector of observations}
	 \item{eps}{Matrix of ensemble forecast. Each column represent a member.}
     \item{alpha}{Matrix of alpha (returned from crpsDecomposition)}
	 \item{beta}{Vector of beta (returned from crpsDecomposition)}
	 \item{heaviside0}{Vector of Heaviside for outlier i=0 (returned from crpsDecomposition)}
	 \item{heavisideN}{Vector of Heaviside for outlier i=N (returned from crpsDecomposition)}
       }
       \value{
	 \item{CRPS}{CRPS score}
	 \item{CRPSpot}{The potential CRPS (Resolution - Uncertainty)}
	 \item{Reli}{The Reliability term of the CRPS}
	 \item{alpha}{Matrix (Nobservation rows x Nmember +1 columns) of alpha used in the CRPS decomposition.}
	 \item{beta}{Matrix (Nobservation rows x Nmember +1 columns) of beta used in the CRPS decomposition.}
	 \item{heaviside0}{Vector (Nobservation length) of Heaviside for outlier i=0 used in the CRPS decomposition}
	 \item{heavisideN}{Vector (Nobservation length) of Heaviside for outlier i=N used in the CRPS decomposition}
	 
       }
 \examples{
data(precip.ensemble)
x <- precip.ensemble[seq(5,5170,5),]

#Observations are in the column
obs<-x[,3]

#Forecast values of ensemble are in the column 4 to 54
eps<-x[,4:54]

#CRPS calculation 
c<-crpsDecomposition(obs,eps)

#CRPS with alpha and beta
#Resampling indices
nObs<-length(obs)
i<-sample(seq(nObs),nObs,replace=TRUE)
crps2<-crpsFromAlphaBeta(c$alpha[i,],c$beta[i,],c$heaviside0[i],c$heavisideN[i])

}


\references{
        G. Candille,  P. L. Houtekamer, and G. Pellerin: Verification of an Ensemble Prediction System against Observations, \emph{Monthly Weather Review},\bold{135}, pp. 2688-2699 

	Hershcach, Hans, 2000. Decomposition of the Continuous Ranked Probability Score for Ensemble Prediction Systems. \emph{Weather and Forecasting}, \bold{15}, (5) pp. 559-570.
	}

\author{Ronald Frenette <Ronald.Frenette@ec.gc.ca>}

       \keyword{file}
