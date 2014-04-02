
#############################################
## Marchenko-Pastur based matrix filtering ##
#############################################
## Following Bouchaud 2004 and Gatheral 2008
##'@import fitdistrplus
##'@export
MPfilter <- function(shrinkEst, ...){

    if(class(shrinkEst) != "CMEshrinkEst")
        stop("data should be CMEshrinkEst object")

    ## Retrieving returns
    R <- shrinkEst$smoothData
    ## retrieving correlation
    cov <- if(is.null(shrinkEst$shrunkScatter))
        shrinkEst$scatter
    else
        shrinkEst$shrunkScatter

    ## checking if the matrix is correlation or covariance
    corr <- shrinkEst$corr
    ## Filtering correlation Matrix
    filteredObj <- .FilterMP(R, cov, corr, ...)

    return(filteredObj)

}
.FilterMP <- function(R,
                      cov,
                      corr,
                      norm.meth = "full",
                      fit.type = "analogic",
                      breaks = "FD",...) {

    Q <- GetQ(R)
    if(!corr) cov <- cov2cor(cov) 
    lambdas <- eigen(cov, symmetric = TRUE)
    eigVals <- lambdas$values
    eigVecs <- lambdas$vectors

    ## Getting empirical EigenValues density
    ## Should also try with kernel estimators
    eigHist <- hist(eigVals, probability = TRUE, plot = FALSE, breaks = breaks)
    densEig <- eigHist$density
    ## Fitting EigenValues density.. this needs work...MLE optimization is difficult
    if(fit.type %in% c("best", "median", "average")) {
        marpasEsts <- .FitEigDensMLE(densEig,Q, ...)
    } else {
        marpasEsts <- .FitEigDensAnalogic(eigVals, Q, ...)
    }
    
    fitSigma <- marpasEsts[1]
    fitQ <- marpasEsts[2]

    lambdaMax <- marpasEig(fitSigma, fitQ)[2]

    ## Flatening Noisy egeinvalues
    fEigVals <- eigVals
    noiseIdx <- (eigVals <= lambdaMax)
    fEigVals[noiseIdx] <- 1
    ## Renormalizing
    M <- length(eigVals)
    if(norm.meth == "full") {
    ## Renormalizing method I: All eigenvalues
        fEigVals <- fEigVals * 1/mean(fEigVals)
    } else {
    ## Renormalizing method II: only filtered EigenVals
        nNoiseEigs <- length(fEigVals[noiseIdx])
        fEigVals[noiseIdx] <- (M - sum(fEigVals[!noiseIdx]))/nNoiseEigs
    }
    
    ## Reforming correlation
    filteredCorr <- eigVecs %*% diag(fEigVals) %*% t(eigVecs)
    ## reforming normalization
    diag(filteredCorr) <- rep(1, M)
    ## passing all relevant parameters
    filteredScatter <- if(corr) filteredCorr else corr2cov(filteredCorr, R)
    filteredEstim <- list(filteredScatter = filteredScatter,
                          filterEstim = list(eigVals = eigVals,
                              eigVecs = eigVecs,
                              noiseEigVecs = eigVecs[, seq(1, M)[noiseIdx]],
                              noiseEigVals = eigVals[noiseIdx],
                              signalEigVecs = eigVecs[, seq(1, M)[!noiseIdx]],
                              signalEigVals = eigVals[!noiseIdx],
                              eigDens = eigDens,
                              eigHist = eigHist,
                              mpEstimates = marpasEsts,
                              lambdaMax = lambdaMax))

    return(filteredEstim)

}

.FitEigDensMLE <- function(empirEigDens,
                           Q,
                           initial.points = 100,
                           Q.mult = 1,
                           fit.type = "median", ...){

    
    ## generating initial point matrix
    start <- matrix(c(runif(initial.points, 0.05, 0.95),
                      runif(initial.points, 0.05, Q * Q.mult)),
                    ncol = 2)
    ## Changing variables to turn constrained optimization problem to un constrained
    ## We write \sigma = Log(1 + exp(\theta_S)) and \Q = Log(1 + exp(\theta_Q))
    ## change of variable. Note change of variables removed for now.


    ## Specifying lower bound
    lower <- c(0, 0)
    multiFit <- function(X) tryCatch(fitdist(empirEigDens,
                                             dmarpasFit,
                                             start = list(sigma = X[1], Q = X[2]),
                                             lower = lower),
                                     error = function(c) list(loglik = Inf, err = c),
                                     silent = TRUE)
    
    densFit <- apply(start, 1, multiFit)

    ## Getting Index
    logLiks <- sapply(densFit, function(X) X[['loglik']])
    bestFitIdx <- which.min(logLiks)
    goodFitsIdx <- which(logLiks != Inf)
    goodFits <- densFit[goodFitsIdx]
    goodLogLiks <- sapply(goodFits, function(X) X[['loglik']])
    medianFitIdx <- order(goodLogLiks)[floor(length(goodLogLiks)/2)]


    ## Getting Estimators
    bestFit <- densFit[[bestFitIdx]][['estimate']]
    averageFit <- rowMeans(sapply(goodFits, function(X) X[['estimate']]))
    medianFit <- goodFits[[medianFitIdx]][['estimate']]
    
    result <- get(paste0(fit.type, "Fit"))
    names(result) <- paste0(names(result),"(", fit.type,")")
    ## Transfer standard deviation.

    return(result)

} 

.FitEigDensAnalogic <- function(eigVals, Q, ...){
    ## Adjusted analogic estimate following bouchaud 2000.
    ## The idea is to substract the market eigenValue of the explained variance

    estimates <- c(sqrt(1 - max(eigVals)/length(eigVals)), Q)
    names(estimates) <- c("sigmaA", "QA")
    return(estimates)
}
