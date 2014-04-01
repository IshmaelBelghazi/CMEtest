## Bouchaud and Gatheral 2008 adhoc argument

## Normalizing method
norm.meth <- "full"

data(sp500.subset)
returns <- sp500.subset
N <- dim(returns)[1]
M <- dim(returns)[2]
Q <- N/M
retCorr <- cor(returns)
lambdas <- eigen(retCorr, symmetric = TRUE)
eigVals <- lambdas$values
eigVecs <- lambdas$vectors

specVar <- mean(eigVals)
stopifnot(specVar == 1)

densEig <- density(eigVals, bw = "SJ", kernel = "rectangular")
## getting fit
eigFit <- fitdist(densEig$y, distr = dmarpasFit, start = list(sigma = 1, Q = Q), ...)
fitSigma <- eigFit$estimate[1] ## Matbe I should add the finiteness correction
fitQ <- eigFit$estimate[2]

lambdaMax <- marpasEig(sigma = fitSigma, fitQ)[2]

## flatening noisy eigenValues
fEigVals <- eigVals
noiseIdx <- (eigVals <= lambdaMax)
fEigVals[noiseIdx] <- 1
## Renormalizing
if(norm.meth == "full") {
    ## Renormalizing method I: All eigenvalues
    fEigVals <- fEigVals * 1/mean(fEigVals)
} else {
    ## Renormalizing method II: only filtered EigenVals
    nNoiseEigs <- length(fEigVals[noiseIdx])
    fEigVals[noiseIdx] <- (M - sum(fEigVals[!noiseIdx])) / nNoiseEigs
}

## Reforming correlation
filteredCorr <- eigVecs %*% t(eigVecs)
## reforming normalization
diag(filteredCorr) <- rep(1, M)
