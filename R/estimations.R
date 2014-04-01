
##########
## TODO ##
##########


#################################
## Defining Estimation Objects ##
#################################

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Estimate obj
##' @param obj
##' @param data
##' @return CMEest
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
Estimate <- function(obj, data) UseMethod('Estimate')
##' @export
Estimate.default <- function(obj, data) warning("Unknown Class")

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Estimate smoothing spec obj
##' @param obj a CME smooth spec object
##' @param data an xts or a matrix of returns
##' @return CMEsmoothEst containing data and smoothed data
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
Estimate.CMEsmoothSpec <- function(obj, data) {

    smoothEst <- .MakeSmoothEst(obj, data)

    return(smoothEst)


}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Estimate estim spec obj
##' @param obj a CME smooth spec object
##' @param data a CMsmoothEst object
##' @return CMEestimEst containing data and smoothed data, and the covariance
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
Estimate.CMEestimSpec <- function(obj, data) {

    if(class(data) != "CMEsmoothEst")
        stop("data should be a CMEsmoothEst object")

    estimEst <- .MakeEstimEst(obj, data)

}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Estimate shrink spec object
##' @param obj CMEshrinkSpec
##' @param data CMEestimSpec object contraining a covariance
##' @return CMEshrinkEst object
##' @author Live session user
##' @export
Estimate.CMEshrinkSpec <- function(obj, data) {

    if(class(data) != "CMEestimEst")
        stop("data should be a CMEestimEst object")

    shrinkEst <- .MakeShrinkEst(obj, data)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Estimate shrink spec object
##' @param obj CMEshrinkSpec
##' @param data CMEestimSpec object contraining a covariance
##' @return CMEshrinkEst object
##' @author Live session user
##' @export
Estimate.CMEfilterSpec <- function(obj, data) {

    if(class(data) != "CMEshrinkEst")
        stop("data should be  CMEshrinkEst object")

    filterEst <- .MakeFilterEst(obj, data)

}

#####################################
## Estimating specification object ##
#####################################
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Estimate a covariance specification object
##' @param obj a covariance specification object
##' @param data a valid xts or return object
##' @return Covariance estimation object
##' @author Mohamed Ishmael Diwan
##' @export
Estimate.CMEspec <- function(obj, data) {

    ## TODO (ishmael): improve memory management... although I don t expect it
    ## to get blown for typical uses even if I am copying the
    ## information several times around.
    ## several time around
    smoothed  <- Estimate(obj$smooth, data)
    estimated <- Estimate(obj$estim, smoothed)
    shrunk    <- Estimate(obj$shrink, estimated)
    filtered  <- Estimate(obj$filter, shrunk)
    ## Wrote it at the start using recursion and some grep
    ## and then I remembered that code is twice as hard to debug
    ## than to write...

    ## here we apply any additional transformation
    ## And we add any supplementary slot, such as
    ## the mahalanobis distance...

    ## Additional computation block starts here

    CMEest <- filtered
    ## Additional computation block ends here


    ## Assigning class to return object
    class(CMEest) <- "CMEest"
    return(CMEest)


}
