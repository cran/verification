######################################################## #
### Intensity-Scale technique
########################################################

IS <- function (frcs, obs, thres) {
    if (dim(frcs)[1] != dim(obs)[1]) {
        stop("Input matrices must have the same dimensions")
    }
    if (dim(frcs)[2] != dim(obs)[2]) {
        stop("Input matrices must have the same dimensions")
    }
    if (dim(frcs)[1] != dim(frcs)[2]) {
        stop("Input matrices must be squared")
    }
    if (log2(dim(frcs)[1]) - floor(log2(dim(frcs)[1])) != 0) {
        stop("Input matrices must have dimensions equal to a power of 2")
    }
    if ((dim(frcs)[1] == dim(obs)[1]) & (dim(frcs)[2] == dim(obs)[2]) &
        (dim(frcs)[1] == dim(frcs)[2]) & (log2(dim(frcs)[1]) -
        floor(log2(dim(frcs)[1])) == 0)) {
    
        SSul = c()
    MSEul = c()
    Bu = c()
    BRu = c()
        N <- log2(dim(frcs)[1])
        for (t in thres) {
            E <- matrix(0, nrow = dim(frcs)[1], ncol = dim(frcs)[2])
            E[(frcs > t) & (obs <= t)] <- 1
            E[(frcs <= t) & (obs > t)] <- -1
            E.dwt <- dwt.2d(E, wf = "haar", J = log2(dim(frcs)[1]))
            MSE <- numeric()
            for (i in 1:N) {
                MSE[i] <- mean((E.dwt[[1 + 3 * (i - 1)]]/2^i)^2) +
                  mean((E.dwt[[2 + 3 * (i - 1)]]/2^i)^2) + mean((E.dwt[[3 +
                  3 * (i - 1)]]/2^i)^2)
            }
            MSE <- c(MSE, mean(E)^2)
# # control sum(MSE)== mean(E^2) #
            MSEul = c(MSEul,MSE)
            br <- length(obs[obs > t])/length(obs)
        BRu = c(BRu,br)
            B <- length(frcs[frcs > t])/length(obs[obs > t])
        Bu = c(Bu,B)
            MSE.random <- B * br * (1 - br) + br * (1 - B * br)
            SSul <- c(SSul, 1 - (MSE * (N + 1))/MSE.random)
# # control HSS = mean(SSul) #
        }
        SSul <- matrix(SSul, nrow = N + 1)
    MSEul <- matrix(MSEul, nrow = N + 1)
        z <- list(SSul = SSul, MSEul = MSEul, l.frcs = dim(frcs)[1],
            thres = thres, Bias = Bu, BR = BRu)
        class(z) <- "int.scale"
        return(z)
    }
}
