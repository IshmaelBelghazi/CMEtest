
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
##'@export
dmarpas <- function(x, sigma, Q) {
  ## Getting Marchenko-Pastur Eigenvalues
  marpasEig <- marpasEig(sigma, Q)
  lambdaMinus <- marpasEig[1]
  lambdaPlus <- marpasEig[2]
  
  density <- ifelse( Q == 1 & x == 0 & 1/x > 0, Inf, 
                     ifelse( x <= lambdaMinus | x >= lambdaPlus, 0,
                             suppressWarnings(
                               Q/( 2 * pi * sigma^2 *x )
                               *
                                 sqrt((lambdaPlus - x) * (x - lambdaMinus)) 
                             ) ) )
  
  return(density)
  
}

#,----
#| Marchenko-Pastur distribution
#`----

pmarpas <- function(q, sigma, Q) {
  ## Getting Marchenko-Pastur Eigenvalues
  marpasEig <- marpasEig(sigma, Q)
  lambdaMinus <- marpasEig[1]
  lambdaPlus <- marpasEig[2]
  
  integrand <- function(x) dmarpas(x, sigma, Q)
  cdf <- ifelse( q <= lambdaMinus, 0, 
               ifelse( q >= lambdaPlus, 1,
                       integrate(integrand, lambdaMinus, q )$value 
               )
  )
  cdf <- ifelse( Q < 1 && q >= 0, cdf + (1 - Q), cdf)
  
  return(cdf) 
}
##'@export
pmarpas <- Vectorize(pmarpas)


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

    eigVals <- c(lambdaMinus = sigma^2 * (1 - sqrt(1/Q))^2,
                 lambdaPlus = sigma^2 * (1 + sqrt(1/Q))^2)

    return(eigVals)

}

#,----
#| Marchenko-Pastur Quality
#`----
##'@export
GetQ <- function(data) dim(data)[1]/dim(data)[2]
