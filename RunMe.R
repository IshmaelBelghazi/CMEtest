# Cleaning Environment
rm(list = ls())
# Loading packages
suppressMessages(library(robust))
suppressMessages(require(PortfolioAnalytics))
suppressMessages(require(PerformanceAnalytics))
suppressMessages(require(tawny))
# Loading  optimization packages
suppressMessages(require(ROI))
suppressMessages(require(ROI.plugin.glpk))
suppressMessages(require(ROI.plugin.quadprog))

##################
## Loading test ##
##################

source("CMEtest.R")

##################
## Loading data ##
##################

# Loading edhec data
data(sp500.subset)
returns <- sp500.subset[1:50,1:10]
assets <- colnames(returns)

##################################################
## Specifying Global Minimum Variance portfolio ##
##################################################


## Specifying portfolio
port_gmv <- portfolio.spec(assets = assets)
## specifying long only constraing
long_const = 0
## specifying unform upper box constraints
upper_box <- 0.60

## Setting constraints
port_gmv <- add.constraint(portfolio = port_gmv,
                           type = "box",
                           min = long_const,
                           max = upper_box)
## Adding objective function
port_gmv <- add.objective(portfolio = port_gmv,
                          type = "risk",
                          name = "var")

## Showing portfolio specification
print(port_gmv)

###############################
## Specifying CME cov object ##
###############################

## Specifying sample cov

mleCovSpec <- CovSpec(smooth = NULL,
                      estim = 'mle',
                      shrink = NULL,
                      filter = NULL)

## Showing summary
summary(mleCovSpec)

## Specifying robust mcd cov

robCovSpec <- CovSpec(smooth = NULL,
                      estim = 'donostah',
                      shrink = NULL,
                      filter = NULL)

## Showing summary
summary(robCovSpec)

##  Generating Moment functions. These functions will dynamically compute
## location and scatter when passed to optimize.portfolio.

MleMomentFUN <- MakeMomentFUN(mleCovSpec)
RobMomentFUN <- MakeMomentFUN(robCovSpec)

###################################
## Let's Optimize the potfolios! ##
###################################

# mle version
opt_gmv_mle <- optimize.portfolio(R = returns,
                                 portfolio = port_gmv,
                                 optimize_method = "ROI",
                                 momentFUN = "MleMomentFUN",
                                 trace = TRUE)

## Robust version
opt_gmv_rob <- optimize.portfolio(R = returns,
                                 portfolio = port_gmv,
                                 optimize_method = "ROI",
                                 momentFUN = "RobMomentFUN",
                                 trace = TRUE)

extractWeights(opt_gmv_mle)
extractWeights(opt_gmv_rob)


## Recomputing the covariance can be expansive. It is also possible to pass the already
## computed location and scale value to porfolio analytics. It is enough to call
## MakeMomentFun on the computed covariance object instead of the specification object

## Let us compute the covariances
mleCovEst <- Estimate(mleCovSpec, returns)
robCovEst <- Estimate(robCovSpec, returns)

## Altough not required let us check the covariance just for fun
GetCorr(mleCovEst)
GetCorr(robCovEst)


## Now we create Precomputed moment functions
MlePrecompMomentFUN <- MakeMomentFUN(mleCovEst)
RobPrecompMomentFUN <- MakeMomentFUN(robCovEst)

# mle version
opt_gmv_mle2 <- optimize.portfolio(R = returns,
                                 portfolio = port_gmv,
                                 optimize_method = "ROI",
                                 momentFUN = "MlePrecompMomentFUN",
                                 trace = TRUE)

## Robust version
opt_gmv_rob2 <- optimize.portfolio(R = returns,
                                  portfolio = port_gmv,
                                  optimize_method = "ROI",
                                  momentFUN = "RobPrecompMomentFUN",
                                  trace = TRUE)

extractWeights(opt_gmv_mle2)
extractWeights(opt_gmv_rob2)
