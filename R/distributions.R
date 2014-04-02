
##########
## TODO ##
##########
## (Ishmael): Consider adding the 1 - Q point mass at zero for Q < 1 to the mp desnity
## (Ishmael): Consider asymptotic approximation for very large k in the mp mom function

######################
## Marchenko-Pastur ##
######################


#,----
#| Marchenko-Pastur density
#`----




dmarpasV2 <- function(x, sigma, Q) {
    x <- as.array(x)
    if(sigma < 0)
        stop("sigma should be positive")
    if(Q < 0)
        stop("Q should be positive")

    lambdaPlus <- sigma^2 * (1 + sqrt(1/Q))^2
    lambdaMinus <- sigma^2 * (1 - sqrt(1/Q))^2
    constant <- Q/(2 * pi * sigma^2)

    supportCond <- (lambdaMinus < x) && (x < lambdaPlus)
    density <- array(0, dim(x))


    density[supportCond] <- constant *
        sqrt((lambdaPlus - x[supportCond]) *
             (x[supportCond] - lambdaMinus)) / x[supportCond]

    cat(density[supportCond])
    return(density)
}
##'@export
dmarpasV1 <- function(x, sigma, Q, fit = FALSE) {
    ##x <- as.array(x)
    if(sigma < 0)
        stop("sigma should be positive")
    if(Q < 0)
        stop("Q should be positive")

    lambdaPlus <- sigma^2 * (1 + sqrt(1/Q))^2
    lambdaMinus <- sigma^2 * (1 - sqrt(1/Q))^2

    supportCond <- (lambdaMinus < x) && (x < lambdaPlus)

    if(supportCond) {
    constant <- Q/(2 * pi * sigma^2)
    density <- constant * sqrt((lambdaPlus - x) * (x - lambdaMinus)) / x
    } else {

    density <- if(fit) .Machine$double.xmin else 0 ## Hack for mle optimization

    }

    return(density)
}



#,----
#| Marchenko-Pastur distribution
#`----
##'@export
dmarpas <- function(x, sigma, Q) {

    density <- Vectorize(function(X) dmarpasV1(X, sigma, Q))

    return(density(x))
}
##'@export
dmarpasFit <- function(x, sigma, Q){
    density <- Vectorize(function(x) dmarpasV1(x, sigma, Q, TRUE))

    return(density(x))
}
##'@export
pmarpas <- function(p, sigma, Q) {

    if(is.infinite(p) && (sign(p) == -1)) {
        cdf <- 0
    } else {
        integrand <- function(x) dmarpas(x, sigma, Q)
        integral <- integrate(integrand, lower = -Inf, upper = p)
        cdf <- integral$value
    }

    return(cdf)
}
##'@export
pmarpasFit <- function(p, sigma, Q) {

    if(is.infinite(p) && (sign(p) == -1)) {
        cdf <- 0
    } else {
        integrand <- function(x) dmarpas(x, sigma, Q, TRUE)
        integral <- integrate(integrand, lower = -Inf, upper = p)
        cdf <- integral$value
    }

    return(cdf)
}

#,----
#| Marchenko-Pastur Moments
#`----
##'@export
marpas <- function(k, sigma, Q, return.all = FALSE) {
    if(sigma < 0)
        stop("sigma should be positive")
    if(Q < 0)
        stop("Q should be positive")
    if(k < 0)
        stop("k should be positive")

    krange <- 0:(k - 1)
    momentsVec <- 1/(krange + 1) *
        choose(k, krange) * choose(k - 1, krange) * (1/Q)^(krange)

    if(return.all) {
        moments <- sigma^(2 * (1:k)) * cumsum(momentsVec)
    } else {
        moments <- sigma^(2 * k) * sum(momentsVec)
    }

    return(moments)

}


#,----
#| Marcenko-Pastur eigenvals
#`----
##'@export
marpasEig <- function(sigma, Q) {

    eigVals <- list(lambdaMinus = sigma^2 * (1 - sqrt(1/Q))^2,
                    lambdaPlus = sigma^2 * (1 + sqrt(1/Q))^2)

    return(eigVals)

}

#,----
#| Marchenko-Pastur Quality
#`----
##'@export
GetQ <- function(data) dim(data)[1]/dim(data)[2]
