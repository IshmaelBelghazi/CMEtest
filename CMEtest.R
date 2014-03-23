
## Define cov-spec object constructor
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param smooth
##' @param smoothCtrl
##' @param estim
##' @param estimCtrl
##' @param shrink
##' @param shrinkCtrl
##' @param filter
##' @param filterCtrl
##' @return
##' @author stark-three
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
##' @title
##' @param specObj
##' @return
##' @author Mohamed Ishmael Diwan Belghazi
summary.CMEspec <- function(specObj) {
    cat('/----------------------------------\\\n')
    cat('|Matrix Covariance Estimators demo |\n')
    cat('\\----------------------------------/\n\n\n')
    cat("\t Specification summary\n\n")
    cat(sprintf('\tSmoothing: %s\n', specObj$smooth$computeMet))
    cat(sprintf('\tEstimation: %s\n', specObj$estim$computeMet))
    cat(sprintf('\tShrinking: %s\n', specObj$shrink$computeMet))
    cat(sprintf('\tFiltering: %s\n', specObj$smooth$computeMet))
    cat("\n-----------------------------------\n")


}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param Obj
##' @param data
##' @return
##' @author Mohamed Ishmael Diwan Belghazi
Estimate <- function(Obj, data) UseMethod('Estimate')

Estimate.default <- function(Obj, data) warning("Unknown Class")

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param specObj
##' @param data
##' @return
##' @author Mohamed Ishmael Diwan Belghazi

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param specObj
##' @param data
##' @return
##' @author Mohamed Ishmael Diwan Belghazi
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
##' @title
##' @param Obj
##' @return
##' @author Mohamed Ishmael Diwan Belghazi
GetCov <- function(Obj) UseMethod("GetCov")

GetCov.default <- function(Obj) warning("Unknown class")
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param specObj
##' @return
##' @author Mohamed Ishmael Diwan Belghazi
GetCov.CMEspec <- function(specObj) cat("Estimate specfication first")
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param estObj
##' @return
##' @author Mohamed Ishmael Diwan Belghazi
GetCov.CMEest <- function(estObj) {

    # Checking if the object contains covariance or correlation. Note that, contrary to the 'robust' package CME passe the data to the object. So it should be possible to return the covariance directly without reestimating the whole object. I am still pondering if it make sense to this here
    if(estObj$corr) warning("Returning correlation. Restimate with corr = FALSE")

    return(estObj$cov)
}



##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param Obj
##' @return
##' @author Mohamed Ishmael Diwan Belghazi
GetCorr <- function(Obj) UseMethod("GetCorr")
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param Obj
##' @return
##' @author Mohamed Ishmael Diwan Belghazi
GetCorr.default <- function(Obj) warning("Unknown class")
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param specObj
##' @return
##' @author Mohamed Ishmael Diwan Belghazi
GetCorr.CMEspec <- function(specObj) cat("Estimate specfication first")
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param estObj
##' @return
##' @author Mohamed Ishmael Diwan Belghazi
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
##' @title
##' @param Obj
##' @return
##' @author Mohamed Ishmael Diwan Belghazi
GetLoc <- function(Obj) UseMethod("GetLoc")
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param Obj
##' @return
##' @author Mohamed Ishmael Diwan Belghazi
GetLoc.default <- function(Obj) warning("Unknown class")
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param specObj
##' @return
##' @author Mohamed Ishmael Diwan Belghazi
GetLoc.CMEspec <- function(specObj) cat("Estimate specfication first")

GetLoc.CMEest <- function(estObj) {

  return(estObj$center)

}
##########################################################
## Defining Make moment function for PortfolioAnalytics ##
##########################################################

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param Obj
##' @return
##' @author Mohamed Ishmael Diwan Belghazi
MakeMomentFUN <- function(Obj) UseMethod('MakeMomentFUN')
MakeMomentFUN.default <- function(Obj) cat("Class unknown")

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param specObj
##' @return
##' @author Mohamed Ishmael Diwan Belghazi
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

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title
##' @param estObj
##' @return
##' @author Mohamed Ishmael Diwan Belghazi
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
