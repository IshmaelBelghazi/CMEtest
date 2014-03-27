
#############################################################
## Meta specification creator for the four building blocks ##
#############################################################
##' @export
.MakeSpec <- function(buildingBlock, method, control, ...) {

    ## Reconstructing dic name
    dic <- paste0(buildingBlock, "Dic")
    if(!(method %in% names(get(dic, envir = .envDic))))
        stop(paste("Unrecognized", buildingBlock, "method"))

    CALL <- mget(names(formals()), sys.frame(sys.nframe()))

    obj <- list() #generic object

    obj[['.rawCall']] <- CALL # Kepeeping the Raw call
    obj[['.passCall']] <- list(fun = get(dic, envir = .envDic)[[method]],
                                 funCall = control,
                                 funMethod = method)

    class(obj) <- c(paste0("CME",buildingBlock,"Spec"), "CMEbbspec") # Building blocks will be considered as sperate objects

    return(obj)
}

#####################################
## Covariance specification object ##
#####################################


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
CMEspec <- function(smooth = NULL,
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
summary.CMEspec <- function(object, ...) {
  cat('/----------------------------------\\\n')
  cat('|Matrix Covariance Estimators test |\n')
  cat('\\----------------------------------/\n\n\n')
  cat("\t Specification summary\n\n")
  cat(sprintf('\tSmoothing: %s\n', object$smooth$.passCall$funMethod))
  cat(sprintf('\tEstimation: %s\n', object$estim$.passCall$funMethod))
  cat(sprintf('\tShrinking: %s\n', object$shrink$.passCall$funMethod))
  cat(sprintf('\tFiltering: %s\n', object$filter$.passCall$funMethod))
  cat("\n-----------------------------------\n")
  
  
}


