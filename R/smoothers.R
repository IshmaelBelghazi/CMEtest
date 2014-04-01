
####################
## Boudt cleaning ##
####################

## This a wrapper around the clean.boudt function from portfolio analytics
##' @export
Boudt <- function(data, alpha = 0.01, trim = 0.001, ...) {
    smoothedData <- Return.clean(R = data,
                                 method = "boudt",
                                 alpha = alpha,
                                 trim = trim)
    return(smoothedData)
    

}
