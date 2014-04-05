
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
GetCov <- function(obj, type = "default") UseMethod("GetCov")
##' @export
GetCov.default <- function(obj, type = "default") warning("Unknown class")
##' @export
GetCov.CMEspec <- function(obj, type = "default") cat("Estimate specfication first")
##' @export
GetCov.CMEest <- function(obj, type = "default") {

    # Checking if the object contains covariance or correlation. Note that, contrary to the 'robust' package CME passe the data to the object. So it should be possible to return the covariance directly without reestimating the whole object. I am still pondering if it make sense to this here
    switch(type,
           shrunk = {scatter <- obj$shrunkScatter},
           filtered = {scatter <- obj$filteredScatter},
           {scatter <- obj$scatter})

    if(!is.null(scatter)) {
        data <- if(is.null(obj$smoothData)) obj$data else obj$smoothData
        if(obj$corr){
            cov <- cor2cov(scatter, data) 
        } else {
            cov <- scatter
        }
    } else {
        cov <- scatter
    }
    
    return(cov)
}



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Extracts Corrlation from estimation object
##' @param Obj
##' @return Returns Correlation
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
GetCor <- function(obj, type = "default") UseMethod("GetCor")
##' @export
GetCor.default <- function(obj, type = "default") warning("Unknown class")
##' @export
GetCor.CMEspec <- function(obj, type = "default") cat("Estimate specfication first")
##' @export
GetCor.CMEest <- function(obj, type = "default") {

    switch(type,
           shrunk = {scatter <- obj$shrunkScatter},
           filtered = {scatter <- obj$filteredScatter},
           {scatter <- obj$scatter})
    if(!is.null(scatter)) {
        if(obj$corr){
            corr <- scatter
        } else {
            corr <- cov2cor(scatter)
        }
    } else {
        corr <- scatter
    }
    return(corr)

}


##' .. content for \description{} (no empty lines) ..
##'
##' .. cont ent for \details{} ..
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
