
##########
## TODO ##
##########
## Ishmael: Extend the getters to to return estimated, shrinked, and
## filtered cov

#########################################
## Defining setter and getters for CME ##
#########################################

## Note to be mistaken with the S4 getCov from rrcov.
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Extact covariance from estimation object
##' @param obj
##' @return covariance
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
GetCov <- function(obj) UseMethod("GetCov")
##' @export
GetCov.default <- function(obj) warning("Unknown class")
##' @export
GetCov.CMEspec <- function(obj) cat("Estimate specfication first")
##' @export
GetCov.CMEest <- function(obj) {

    # Checking if the object contains covariance or correlation. Note that, contrary to the 'robust' package CME passe the data to the object. So it should be possible to return the covariance directly without reestimating the whole object. I am still pondering if it make sense to this here
    if(obj$.estEstim$corr) warning("Returning correlation. Restimate with corr = FALSE")

    return(obj$scatter)
}



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Extracts Corrlation from estimation object
##' @param Obj
##' @return Returns Correlation
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
GetCorr <- function(obj) UseMethod("GetCorr")
##' @export
GetCorr.default <- function(obj) warning("Unknown class")
##' @export
GetCorr.CMEspec <- function(obj) cat("Estimate specfication first")
##' @export
GetCorr.CMEest <- function(obj) {

    if(obj$.estEstim$corr){
        corr <- estObj$scatter
    } else {
        corr <- cov2cor(obj$scatter)
    }

    return(corr)

}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title First moment extraction
##' @param obj
##' @return Location
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
GetLoc <- function(obj) UseMethod("GetLoc")
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title First moment extraction
##' @param Obj
##' @return Location
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
GetLoc.default <- function(obj) warning("Unknown class")
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title First moment extraction
##' @param obj
##' @return Location
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
GetLoc.CMEspec <- function(obj) cat("Estimate specfication first")
##' @export
GetLoc.CMEest <- function(obj) {
  
  return(obj$loc)
  
}

