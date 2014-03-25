## Creating environment holding dictionaries
.envDic <- new.env(hash = TRUE, parent = .GlobalEnv)
## Specifying buildingblocks categories

bbCategories <- c("smooth", "estim", "shrink", "filter")
assign("bbCategories", bbCategories, envir= .envDic)

## Internal function to build the dictionaries
dicName <- c("sDic", "eDic", "shDic", "fdic")
## Smoothing methods
sList = c("None", "EWMA")
smoothDic = c("None", "EWMASmooth")
names(smoothDic) <- sList
## Estimation methods
reList <- c("auto",
            "mcd",
            "weighted",
            "donostah",
            "pairwiseQC",
            "pairwiseGK")

reDic <- rep('covRob', length(reList))
names(reDic) <- reList

ceList <- c("mle")
ceDic <- rep('covClassic', length(ceList))
names(ceDic) <- ceList
estimDic <- c(ceDic, reDic)
## Shrinking methods
shList <- c("None", "LW", "NLW")
shrinkDic <- c("None", "LedoitWolf", "NLNledoitWold")
names(shrinkDic) <- shList

## Filtering methods
fList <- c("None", "Marcenko-Pastur")
filterDic <- c("None", "mpfilter")
names(filterDic) <- fList

sapply(get("bbCategories",envir = .envDic),
       function(X) assign(paste0(X,"Dic"), get(paste0(X,"Dic")), envir = .envDic))

########################################################
.MakeSpec <- function(buildingBlock, method, control, ...) {

    ## Reconstructing dic name
    dic <- paste0(buildingBlock, "Dic")
    if(!(method %in% names(get(dic, envir = .envDic))))
        stop(paste("Unrecognized", buildingBlock, "method"))

    CALL <- mget(names(formals()), sys.frame(sys.nframe()))

    specObj <- list() #generic object

    specObj[['.rawCall']] <- CALL # Kepeeping the Raw call
    specObj[['.passCall']] <- list(fun = get(dic, envir = .envDic)[[method]],
                                 funCall = control,
                                 funMethod = method)

    class(specObj) <- c(paste0("CME",buildingBlock,"Spec"), "CMEbbspec") # Building blocks will be considered as sperate objects

    return(specObj)
}

# Defining cov spec function
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
                    filterCtrl = list(), ...) {
  
  ## Recording call. match.call() is not enough for printing default argument.
  CALL <- mget(names(formals()),sys.frame(sys.nframe()))
  ## Creating object
  spec <- list()
 spec <- sapply(get("bbCategories", envir=.envDic), 
                 function(X) {spec[[X]] <- .MakeSpec(X, get(X), get(paste0(X, "Ctrl")))},
                simplify = FALSE)
  
  spec[['CALL']] <- CALL
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
summary.CMEspec <- function(specObj, ...) {
  cat('/----------------------------------\\\n')
  cat('|Matrix Covariance Estimators test |\n')
  cat('\\----------------------------------/\n\n\n')
  cat("\t Specification summary\n\n")
  cat(sprintf('\tSmoothing: %s\n', specObj$smooth$.passCall$funMethod))
  cat(sprintf('\tEstimation: %s\n', specObj$estim$.passCall$funMethod))
  cat(sprintf('\tShrinking: %s\n', specObj$shrink$.passCall$funMethod))
  cat(sprintf('\tFiltering: %s\n', specObj$filter$.passCall$funMethod))
  cat("\n-----------------------------------\n")
  
  
}


#################################
## Defining Estimation Objects ##
#################################

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Estimate specObj
##' @param Obj
##' @param data
##' @return CMEest
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
Estimate <- function(specObj, data) UseMethod('Estimate')
##' @export
Estimate.default <- function(specObj, data) warning("Unknown Class")

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Estimate smoothing spec obj
##' @param specObj a CME smooth spec object
##' @param data an xts or a matrix of returns
##' @return CMEsmoothEst containing data and smoothed data
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
Estimate.CMEsmoothSpec <- function(specObj, data) {
    

    smoothFUN  <- specObj$.passCall$fun # Getting function tocall
    smoothArgs <- specObj$.passCall$funCall # Getting arguments
    smoothArgs[['data']] <- data # All functions will be wrapped such as
                                        # they can be called as data
                                        # plus something else.
    # Calling smoothing function
    smoothData <- if (smoothFUN == 'None') data else do.call(smoothFUN, smoothArgs)

    ####################################
    ## Constructiong smoother object  ##
    ####################################

    smoothObj <- list(data = data,
                      smoothData = smoothData,
                      .smoothSpec = specObj)

    # Assigning class
    class(smoothObj) <- "CMEsmoothEst"

    return(smoothObj)

}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Estimate estim spec obj
##' @param specObj a CME smooth spec object
##' @param data a CMsmoothEst object
##' @return CMEestimEst containing data and smoothed data, and the covariance
##' @author Mohamed Ishmael Diwan Belghazi
##' @export
Estimate.CMEestimSpec <- function(specObj, data) {

    if(class(data) != "CMEsmoothEst")
        stop("data should be a CMEsmoothEst object")

    estimFUN  <- specObj$.passCall$fun # Getting function to call
    estimArgs <- specObj$.passCall$funCall # Getting arguments
    estimArgs[['data']] <- as.matrix(data$smoothData) # using smoothed data
    # Estimating covariance
    covEst <- if (estimFUN == 'None') NULL else do.call(estimFUN, estimArgs)

    ####################################
    ## Constructing Estimation object ##
    ####################################

    data[['loc']] <- covEst$center
    data[['scatter']] <- covEst$cov # Need to handle the case where the
                                        # object is not of class
                                        # robust
    data[['.estSpec']] <- specObj
    data[['.estEstim']] <- covEst
    
    # Assigning class
    class(data) <- "CMEestimEst"

    return(data)

}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Estimate shrink spec object
##' @param specObj CMEshrinkSpec
##' @param data CMEestimSpec object contraining a covariance
##' @return CMEshrinkEst object 
##' @author Live session user
Estimate.CMEshrinkSpec <- function(specObj, data) {

        if(class(data) != "CMEestimEst")
        stop("data should be a CMEestimEst object")


    shrinkFUN  <- specObj$.passCall$fun # Getting function to call
    shrinkArgs <- specObj$.passCall$funCall # Getting arguments
    shrinkArgs[['cov']] <- data$cov
    # Estimating covariance
    shrunkCovEst <- if (shrinkFUN == 'None') NULL else do.call(shrinkFUN, shrinkArgs)

    ########################################
    ## Constructing shrinker data object  ##
    ########################################

    data[['shrunkCov']] <- shrunkCovEst
    data[['.shrinkSpec']] <- specObj
   # Assilass
    class(data) <- "CMEshrinkEst"

    return(data)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Estimate shrink spec object
##' @param specObj CMEshrinkSpec
##' @param data CMEestimSpec object contraining a covariance
##' @return CMEshrinkEst object 
##' @author Live session user
Estimate.CMEfilterSpec <- function(specObj, data) {

        if(class(data) != "CMEshrinkEst")
           stop("data should be  CMEshrinkEst object")


    filterFUN  <- specObj$.passCall$fun # Getting function to call
    filterArgs <- specObj$.passCall$funCall # Getting arguments
        
    filterArgs[['cov']] <- if(is.null(data$shrunk)) data$scatter else data$shrunk
    # Estimating covariance
    filteredCovEst <- if (shrinkFUN == 'None') NULL else do.call(filterFUN, FilterArgs)

    ########################################
    ## Constructing shrinker data object  ##
    ########################################

    data[['filteredCov']] <- covEstfilteredCovEst
    data[['.filterSpec']] <- specObj
   # Assilass
    class(data) <- "CMEfilterEst"

    return(data)
}


