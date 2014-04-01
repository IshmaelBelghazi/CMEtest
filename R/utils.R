####################################
## Package level helper functions ##
####################################
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title General package information
##' @return void
##' @author stark-three
##' @export
CMEhelp <- function() {

    ## defining method fetcher
    FetchMet <- function(x) names(get(paste0(x, "Dic"), envir = .envDic))
    cat("=======================\n")
    cat("CMEtest avaiable Method\n")
    cat("=======================\n\n")
    cat(sprintf("Smoothing  : %s \n", FetchMet("smooth")))
    cat(sprintf("Estimation : %s \n", FetchMet("estim")))
    cat(sprintf("Shrinking  : %s \n", FetchMet("shrink")))
    cat(sprintf("Filtering  : %s \n", FetchMet("filter")))

    cat("-*-*-*-*-*-*-*-*-*-*-*-*-*-*-\n")
    cat(sprintf("Version: %s\n", packageVersion("CMEtest")))
    invisible()
}

###########################################
## Computation specific helper functions ##
###########################################

corr2cov<- function(corMat, R) {
  sds <- apply(R, 2, sd)
  covMat <- corMat * sds * (rep(sds, each=nrow(corMat)))
  return(covMat)
}
