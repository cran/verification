# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
# ** Copyright UCAR (c) 1992 - 2004 
# ** University Corporation for Atmospheric Research(UCAR) 
# ** National Center for Atmospheric Research(NCAR) 
# ** Research Applications Program(RAP) 
# ** P.O.Box 3000, Boulder, Colorado, 80307-3000, USA 
# ** 2004/9/1 14:13:55 
# *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=* 
int.scale.verify <- function(frcs, obs, thres = quantile(frcs, p= seq(0,0.9,0.1) ),  ... )
{
# controls on inputs and error messages
# check that dimensions are equal and factor of the power of two
if(dim(frcs)[1]!=dim(obs)[1]){
stop("Input matrices must have the same dimensions")}
if(dim(frcs)[2]!=dim(obs)[2]){
stop("Input matrices must have the same dimensions")}
if(dim(frcs)[1]!=dim(frcs)[2]){
stop("Input matrices must be squared")}
if(log2(dim(frcs)[1])-floor(log2(dim(frcs)[1]))!=0){
stop("Input matrices must have dimensions equal to a power of 2")}

## check dimensions
if(	(dim(frcs)[1]==dim(obs)[1]) & 
	(dim(frcs)[2]==dim(obs)[2]) &
	(dim(frcs)[1]==dim(frcs)[2]) & 
	(log2(dim(frcs)[1])-floor(log2(dim(frcs)[1]))==0) ) {
  
	SSul = c()  ## stores ??
        N<- log2( dim(frcs)[1]) ## 2^N dimensions for forecast and obs.
        MSE.out <- matrix(NA, ncol = length(thres), nrow = (N+1) )
        ind <- 1 ## MSE.out index  
        
for(t in thres){  ## check each threshold and create loop.
	
		E <-  matrix(0,  nrow = dim(frcs)[1], ncol = dim(frcs)[2] )
		E[(frcs>t)&(obs<=t)]<-  1  # false positives
		E[(frcs<=t)&(obs>t)]<- -1  # false negatives
                # discrete haar wavelet decomposition from package waveslim
		E.dwt <-  dwt.2d(E,wf="haar",J=log2(dim(frcs)[1]))
		
		# evaluation of MSE of each wavelet component
		#
MSE<- numeric()
 for(i in 1:N){ 
       MSE[i] <- mean( (E.dwt[[ 1 + 3*(i - 1)]]/2^i)^ 2 )+
                 mean( (E.dwt[[ 2 + 3*(i - 1)]]/2^i)^ 2 )+
                 mean( (E.dwt[[ 3 + 3*(i - 1)]]/2^i)^ 2 ) 
               }  ## close 1:N loop
		MSE <-  c(MSE, mean(E)^2)
                MSE.out[,ind]<- MSE 
ind <- ind + 1
                                        #
		# evaluation of skill score
		#
		br <-  length(obs[obs>t])/length(obs) ## br = base rate 
		B <-  length(frcs[frcs>t])/length(obs[obs>t])## bias              
		MSE.random <-  B*br*(1-br)+br*(1-B*br) ## MSE from a random forecast with bias and base rate
		SSul <-  c(SSul, 1- (MSE * (N+1) )/MSE.random) ## Skil score thres = u,  scale = l
		} ## close threshold loop
		SSul <-  matrix(SSul, nrow = N + 1)

        colnames(SSul) <- round(thres,2)
        rownames(SSul) <- c(seq(1,log2(dim(frcs)[1])),"bias")
        
        colnames(MSE.out) <- round(thres,2)
        rownames(MSE.out) <- c(seq(1,log2(dim(frcs)[1])),"bias")

           # image of the skill score as function of threshold and scale
	z<- list(SSul = SSul, MSE = MSE.out, l.frcs = dim(frcs)[1], thres = thres )	

class(z)<- "int.scale"
        return(z)
      } ## closes check function
} ### closes function



