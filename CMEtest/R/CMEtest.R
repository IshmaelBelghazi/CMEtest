
##' Covariance specification object Constructor.
##' .. content for \description{} (no empty lines) ..
##' This function allows specification of a covariance objects.
##' .. content for \details{} ..
##'
##' @title General Interface for covariance matrix specification
##' @param smooth character specifying to smoothing method.
##' @param smoothCtrl list containing smoothing parameters
##' @param estim  character specifying which estimation methods ('mle', 'mcd', 'mve,...)
##' @param estimCtrl list containing estimation parameters
##' @param shrink character specifying which covariance shrinkage methode to choose
##' @param shrinkCtrl list containing shrinkage parameters
##' @param filter character specifying which filtering method to choose
##' @param filterCtrl list containing filtering parameters
##' @return CMEspec: S3 object containing complete specification
##' @author Mohamed Ishmael Diwan Belghazi
##' @import robust
##' @export
CovSpec <- function(smooth = NULL,
                    smoothCtrl = list(),
                    estim = "auto",
                    estimCtrl = list(),
                    shrink = NULL,
                    shrinkCtrl = list(),
                    filter = NULL,
                    filterCtrl = list()) {


    ## Note this is just a quick ugly hack. This will be rewritten to use the building blacks approach

    ## defining dictionary for  methods to  function around different packages
    ## Here will be made all the external functions book keeping



    ## Smoothing methods
    smoothList <- NULL
    smoothDic <- NULL
    names(smoothDic) <- smoothList

    ## Estimation methods
    robEstList <- c("auto",
                    "mcd",
                    "weighted",
                    "donostah",
                    "pairwiseQC",
                    "pairwiseGK")
    robEstDic <- rep('covRob', length(robEstList))
    names(robEstDic) <- robEstList

    clasEstList <- c("mle")
    clasEstDic <- rep('covClassic', length(clasEstList))
    names(clasEstDic) <- clasEstList

    estimDic <- c(robEstDic, clasEstDic)

    ## Shrinkage  methods
    shrinkList <- NULL
    shrinkDic <- NULL
    names(shrinkDic) <- NULL

    ## filtering methods
    filterList <- NULL
    filterDic <- NULL
    names(filterDic) <- filterList

    ## Wrapping all the dictionary in a large "mapping" dictionary
    mapDic <- list(smooth = smoothDic,
                   estim = estimDic,
                   shrink = shrinkDic,
                   filter = filterDic)

    ## Recording call. match.call() is not enough for printing default argument.
    CALL <- mget(names(formals()),sys.frame(sys.nframe()))

    if(!(CALL$estim %in% names(estimDic)))
        stop("Unrecognized Estimation method")

    argVec <- sapply(CALL, deparse)
    specCat <-names(argVec[!(grepl('Ctrl', names(argVec)))])
    ## The following function create a list containing the called function
    ## and the call. A sort of function dispatcher. Formatting of the calls will
    ## be handled here. Of course, the call will depend on the receiving funcion
    ## the function returns a list containing fun, a character representing the function
    ## and call
    MakeAtr <- function(X) {

        switch(X,

               smooth = {
                   fun = NULL
                   funCall = NULL
                   computeMet <- NULL
               },

               estim = {

                   ## Getting function to be used from mapping dictionary
                   ## Getting relevant dictionary
                   estDic <- mapDic[[X]]
                   ## Getting function to be called
                   fun <- as.character(estDic[names(estDic) == estim])
                   ## Modifying function call to fit covrob and covClassic
                   if(fun == 'covRob' || fun == 'covClassic'){
                       funCall <- CALL$estimCtrl
                       funCall[['estim']] <- estim
                       computeMet <- estim

                   }
               },
               shrink = {
                   fun <- NULL
                   funCall <- NULL
                   computeMet <- NULL
               },

               filter ={
                   fun <- NULL
                   funCall <- NULL
                   computeMet <- NULL
               }
               )

        return(list(fun = fun, funCall = funCall, computeMet = computeMet))
    }


    spec <- sapply(specCat, MakeAtr, simplify = FALSE)
    spec[['CALL']] <- CALL
    spec[['categories']] <- specCat
    class(spec) <-'CMEspec'
    return(spec)

}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title summary of specification object
##' @param object
##' @return nothing
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
summary.CMEspec <- function(object, ...) {
    cat('/----------------------------------\\\n')
    cat('|Matrix Covariance Estimators demo |\n')
    cat('\\----------------------------------/\n\n\n')
    cat("\t Specification summary\n\n")
    cat(sprintf('\tSmoothing: %s\n', object$smooth$computeMet))
    cat(sprintf('\tEstimation: %s\n', object$estim$computeMet))
    cat(sprintf('\tShrinking: %s\n', object$shrink$computeMet))
    cat(sprintf('\tFiltering: %s\n', object$smooth$computeMet))
    cat("\n-----------------------------------\n")


}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Estimate specObj
##' @param Obj
##' @param data
##' @return CMEest
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
Estimate <- function(Obj, data) UseMethod('Estimate')
##' @export
Estimate.default <- function(Obj, data) warning("Unknown Class")
##' @export
Estimate.CMEspec <- function(specObj, data) {

    ## Getting back call from specification object

    if(dim(data)[1] <= dim(data)[2])
        stop("n <= p. Witchcraft is not allowed... for now")

    estimFUN <- specObj$estim$fun
    estimArgs <- specObj$estim$funCall
    estimArgs[['data']] <- data
    estimObj <- do.call(estimFUN, estimArgs)


    ## Getting Estimated correlation or covariance matrix
    class(estimObj) <- c('CMEest', class(estimObj))

    return(estimObj)


}

#########################################
## Defining setter and getters for CME ##
#########################################

## Note to be mistaken with the S4 getCov from rrcov.
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Extact covariance from estimation object
##' @param Obj
##' @return covariance
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
GetCov <- function(Obj) UseMethod("GetCov")
##' @export
GetCov.default <- function(Obj) warning("Unknown class")
##' @export
GetCov.CMEspec <- function(specObj) cat("Estimate specfication first")
##' @export
GetCov.CMEest <- function(estObj) {

    # Checking if the object contains covariance or correlation. Note that, contrary to the 'robust' package CME passe the data to the object. So it should be possible to return the covariance directly without reestimating the whole object. I am still pondering if it make sense to this here
    if(estObj$corr) warning("Returning correlation. Restimate with corr = FALSE")

    return(estObj$cov)
}



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Extracts Corrlation from estimation object
##' @param Obj
##' @return Returns Correlation
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
GetCorr <- function(Obj) UseMethod("GetCorr")
##' @export
GetCorr.default <- function(Obj) warning("Unknown class")
##' @export
GetCorr.CMEspec <- function(specObj) cat("Estimate specfication first")
##' @export
GetCorr.CMEest <- function(estObj) {

    if(estObj$corr){
        corr <- estObj$cov
    } else {
        corr <- cov2cor(estObj$cov)
    }

    return(corr)

}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Extract location from Estimation Object
##' @param Obj
##' @return Location
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
GetLoc <- function(Obj) UseMethod("GetLoc")
##' @export
GetLoc.default <- function(Obj) warning("Unknown class")
##' @export
GetLoc.CMEspec <- function(specObj) cat("Estimate specfication first")
##' @export
GetLoc.CMEest <- function(estObj) {

  return(estObj$center)

}
##########################################################
## Defining Make moment function for PortfolioAnalytics ##
##########################################################

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Generates moment function from spec or estimation object
##' @param Obj
##' @return moment function for portfolioanalytics.
##' @author Mohamed Ishmael Diwan Belghazi
##' @import PerformanceAnalytics
##' @export
MakeMomentFUN <- function(Obj) UseMethod('MakeMomentFUN')
##' @export
MakeMomentFUN.default <- function(Obj) cat("Class unknown")

##' @export
MakeMomentFUN.CMEspec <- function(specObj) {

    momFUN <- function(R, portfolio, momentargs = list(), ...) {

        ## Estimating Robust Location and scatter
        if(is.null(momentargs$mu) || is.null(momentargs$sigma))
            CMEmom <- Estimate(specObj, R)

        if(is.null(momentargs$mu))
            momentargs$mu <- matrix(GetLoc(CMEmom), ncol = 1)

        if(is.null(momentargs$sigma))
            momentargs$sigma <- GetCov(CMEmom)

        if(is.null(momentargs$m3))
            momentargs$m3 <- PerformanceAnalytics:::M3.MM(R)

        if(is.null(momentargs$m4))
            momentargs$m4 <- PerformanceAnalytics:::M4.MM(R)

        return(momentargs)
    }


    return(momFUN)
}

##' @export
MakeMomentFUN.CMEest <- function(estObj) {

    momFUN <- function(R, portfolio, momentargs = list(), ...) {

        ## Estimating Robust Location and scatter

        if(is.null(momentargs$mu))
            momentargs$mu <- matrix(GetLoc(estObj), ncol = 1)

        if(is.null(momentargs$sigma))
            momentargs$sigma <-  GetCov(estObj)

        if(is.null(momentargs$m3))
            momentargs$m3 <- PerformanceAnalytics:::M3.MM(R)

        if(is.null(momentargs$m4))
            momentargs$m4 <- PerformanceAnalytics:::M4.MM(R)

        return(momentargs)
    }


    return(momFUN)
}
