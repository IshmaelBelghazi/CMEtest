## Creating environment holding dictionaries
.envDic <- new.env(hash = TRUE, parent = .GlobalEnv)

## Internal function to build the dictionaries
sList = c("Id", "EWMA")
sDic = c("IdSmooth", "EWMASmooth")
names(sDic) <- sList
assign(x = "smoothDic", value = sDic, envir = .envDic)




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

    class(specObj) <- c(paste0("CME",buildingBlock,"Spec"), "CMEspec")

    return(specObj)
}
