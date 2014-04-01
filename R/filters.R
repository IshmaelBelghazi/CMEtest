
#############################################
## Marchenko-Pastur based matrix filtering ##
#############################################
## Following Bouchaud 2004 and Gatheral 2008
##'@import fitdistrplus
##'@import RMTstat
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

    if(corr == FALSE)
        stop("Marchenko-Filtering for covariance not yet implemented")
    ## Filtering correlation Matrix
    filteredObj <- .FilterMP(R, cov, corr, ...)

    return(filteredObj)





}
.FilterMP <- function(R, cov, corr, norm.meth = "full", bw = "SJ", kernel = "gaussian", ...) {

    Q <- GetQ(R)
    lambdas <- eigen(cov, symmetric = TRUE)
    eigVals <- lambdas$values
    eigVecs <- lambdas$vectors

    ## Getting empirical EigenValues density
    densEig <- density(eigVals, bw = bw, kernel = kernel, ...)
    ## Fitting EigenValues density.. this needs work...

    marpasEsts <- .FitEigDens(R = densEig$y, Q = Q, ...)
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
        fEigVals[noiseIdx] <- (M - sum(fEigVals[!noiseIdx])) / nNoiseEigs
    }

    ## Reforming correlation
    filteredCorr <- eigVecs %*% t(eigVecs)
    ## reforming normalization
    diag(filteredCorr) <- rep(1, M)
    ## Getting noisy eigenVectors

    ## Getting noisy eigenValues


    ## passing all relevant parameters
    filteredEstim <- list(filteredScatter = filteredCorr,
                          filterEstim = list(eigVals = eigVals,
                              eigVecs = eigVecs,
                              noiseEigVecs = eigVecs[, seq(1, M)[noiseIdx]],
                              noiseEigVals = eigVals[noiseIdx],
                              signalEigVecs = eigVecs[, seq(1, M)[!noiseIdx]],
                              signalEigVals = eigVals[!noiseIdx],
                              eigDens = densEig,
                              mpEstimates = marpasEsts,
                              lambdaMax = lambdaMax))


    return(filteredEstim)

}

.FitEigDens <- function(R,
                        Q,
                        initial.points = 100,
                        Q.mult = 1,
                        change.vars = TRUE,
                        fit.type = "average", ...){


                                        # generating initial point matrix
    start <- matrix(c(runif(initial.points, 0.25, 1),
                      runif(initial.points, 0.25, Q * Q.mult)),
                    ncol = 2)
    ## Changing variables to turn constrained optimization problem to un constrained
    ## We write \sigma = Log(1 + exp(\theta_S)) and \Q = Log(1 + exp(\theta_Q))
    ## change of variable
    if (change.vars == TRUE) {
        H <- function(x) log(1 + exp(x))
        InvH <- function(x) log(exp(x) - 1)
    } else {
        H <- function(x) x
        InvH <- function(x) x
    }


    ## Running fits
    dmarpasFitu <- function(x, var, svr) dmp(x = x, var = H(var), svr = H(svr))
    pmarpasFitu <- function(p, var, svr) pmp(p = p, var = H(var), svr = H(svr))
##:ess-bp-start::browser@nil:##
browser(expr=is.null(.ESSBP.[["@4@"]]))##:ess-bp-end:##


    densFit <- apply(start, 1, function(X) tryCatch(fitdist(R,
                                                            dmarpasFitu,
                                                            start = list(thetaV = InvH(X[1]), thetaQ = InvH(X[2])),
                                                            lower = if(change.vars) -Inf else c(0, 0), ...),
                                                    error = function(c) list(loglik = Inf, err = c),
                                                    silent = TRUE)
                     )

    ## Getting Index
    logLiks <- sapply(densFit, function(X) X[['loglik']])
    bestFitIdx <- which.min(logLiks)
    goodFitsIdx <- which(logLiks != Inf)
    goodFits <- densFit[goodFitsIdx]
    goodLogLiks <- sapply(goodFits, function(X) X[['loglik']])
    medianFitIdx <- order(goodLogLiks)[floor(length(goodLogLiks)/2)]


    ## Getting Estimators
    bestFit <- H(densFit[[bestFitIdx]][['estimate']])
    averageFit <- rowMeans(sapply(goodFits, function(X) H(X[['estimate']])))
    medianFit <- H(goodFits[[medianFitIdx]][['estimate']])

    result <- get(paste0(fit.type, "Fit"))
    names(result) <- paste0(names(result),"(", fit.type,")")
    ## Transfer standard deviation.

    return(result)

}
