.onLoad <- function(libname, pkgname) {

    ##########
    ## TODO ##
    ##########
    ## Ishmael: Move the functions mapping to a safer place
    packageStartupMessage("CMEtest set up has started...")
    ## Creating environment holding dictionaries
    .envDic <- local(new.env())
    ## Specifying buildingblocks categories

    bbCategories <- c("smooth", "estim", "shrink", "filter")
    assign("bbCategories", bbCategories, envir = .envDic)

    ## Internal function to build the dictionaries
    dicName <- c("sDic", "eDic", "shDic", "fdic")
    ## Smoohing methods
    sList = c("None", "Boudt")
    smoothDic = c("None", "Boudt")
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
    shList <- c("None")
    shrinkDic <- c("None")
    names(shrinkDic) <- shList

    ## Filtering methods
    fList <- c("None", "MP")
    filterDic <- c("None", "MPfilter")
    names(filterDic) <- fList

    sapply(get("bbCategories",envir = .envDic),
           function(X) assign(paste0(X,"Dic"), get(paste0(X,"Dic")), envir = .envDic))
    ## For now I ll just leave as is for debugging purpose. Will
    ## be moved to package environememt later
    assign(x=".envDic",value=.envDic,envir=.GlobalEnv)
    packageStartupMessage("CMEtest set up completed\n")

    packageStartupMessage("This is Covariance Matrix Estimators test v0.1\n")

}

.onUnload <- function(libpath) {
  ## Cleaning environements
  rm(.envDic, envir=.GlobalEnv)

}
