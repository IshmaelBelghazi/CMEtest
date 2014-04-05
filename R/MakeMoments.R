##########################################################
## Defining Make moment function for PortfolioAnalytics ##
##########################################################

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Generates moment function from spec or estimation object
##' @param obj
##' @return moment function for portfolioanalytics.
##' @author Mohamed Ishmael Diwan Belghazi
##' @import PerformanceAnalytics
##' @export
MakeMomentFUN <- function(obj, type,...) UseMethod('MakeMomentFUN')
##' @export
MakeMomentFUN.default <- function(obj, type,...) cat("Class unknown")

##' @export
MakeMomentFUN.CMEspec <- function(obj, type,...) {

    momFUN <- function(R, portfolio, momentargs = list(), ...) {

        ## Estimating Robust Location and scatter
        if(is.null(momentargs$mu) || is.null(momentargs$sigma))
            CMEmom <- Estimate(obj, R)

        if(is.null(momentargs$mu))
            momentargs$mu <- matrix(GetLoc(CMEmom), ncol = 1)

        if(is.null(momentargs$sigma))
            momentargs$sigma <- GetCov(CMEmom, type)

        if(is.null(momentargs$m3))
            momentargs$m3 <- PerformanceAnalytics:::M3.MM(R)

        if(is.null(momentargs$m4))
            momentargs$m4 <- PerformanceAnalytics:::M4.MM(R)

        return(momentargs)
    }


    return(momFUN)
}
##' @import PerformanceAnalytics
##' @export
MakeMomentFUN.CMEest <- function(obj, type,...) {

    momFUN <- function(R, portfolio, momentargs = list(), ...) {

        ## Estimating Robust Location and scatter

        if(is.null(momentargs$mu))
            momentargs$mu <- matrix(GetLoc(obj), ncol = 1)

        if(is.null(momentargs$sigma))
            momentargs$sigma <-  GetCov(obj, type)

        if(is.null(momentargs$m3))
            momentargs$m3 <- PerformanceAnalytics:::M3.MM(R)

        if(is.null(momentargs$m4))
            momentargs$m4 <- PerformanceAnalytics:::M4.MM(R)

        return(momentargs)
    }


    return(momFUN)
}
