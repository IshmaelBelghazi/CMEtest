################################################
# The purpose of this test is to optimize a    #
# Global minimum variance portfolio through    #
# portfolio analytics using custom location    #
# and scatter estimates                        #
# The portfolio is long only with a uniform    #
# reasonable box.                              #
################################################



# Loading packages
suppressMessages(require(PortfolioAnalytics))
# Loading  optimization packages
suppressMessages(require(ROI))
suppressMessages(require(ROI.plugin.glpk))
suppressMessages(require(ROI.plugin.quadprog))

##################
## Loading test ##
##################

suppressMessages(library(CMEtest))
options(width=60, warn = -1)

##################
## Loading data ##
##################

# Loading data
data(sp500.subset)
returns <- sp500.subset[1:50,1:10]
assets <- colnames(returns)

## Specifying portfolio
port_gmv <- portfolio.spec(assets = assets)
## specifying long only constraing
long_const = 0
## specifying unform upper box constraints
upper_box <- 0.60

## Setting constraints
port_gmv <- add.constraint(portfolio = port_gmv, 
                           type="full_investment", 
                           enabled=TRUE)
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

## Showing portfolio specification
print(port_gmv)

## Specifying sample cov

mleCovSpec <- CMEspec(smooth = 'None',
                      estim = 'mle',
                      shrink = 'None',
                      filter = 'None')

## Showing summary
summary(mleCovSpec)

robCovSpec <- CMEspec(smooth = 'None',
                      estim = 'mcd',
                      shrink = 'None',
                      filter = 'None')


## Showing summary
summary(robCovSpec)

##  Generating Moment functions. These functions will dynamically compute
## location and scatter when passed to optimize.portfolio.

MleMomentFUN <- MakeMomentFUN(mleCovSpec)
RobMomentFUN <- MakeMomentFUN(robCovSpec)

###################################
## Let's Optimize the potfolios! ##
###################################

## mle version
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

## Extracting weigths
## MLE
print(extractWeights(opt_gmv_mle))
## Robust
print(extractWeights(opt_gmv_rob))

## Let us compute the covariances by estimating the specification object
mleCovEst <- Estimate(mleCovSpec, returns)
class(mleCovEst)
robCovEst <- Estimate(robCovSpec, returns)
class(robCovEst)

## Now we create Precomputed moment functions
MlePrecompMomentFUN <- MakeMomentFUN(mleCovEst)
RobPrecompMomentFUN <- MakeMomentFUN(robCovEst)

# mle version
opt_gmv_mle <- optimize.portfolio(R = returns,
                                  portfolio = port_gmv,
                                  optimize_method = "ROI",
                                  momentFUN = "MlePrecompMomentFUN",
                                  trace = TRUE)

## Robust version
opt_gmv_rob <- optimize.portfolio(R = returns,
                                  portfolio = port_gmv,
                                  optimize_method = "ROI",
                                  momentFUN = "RobPrecompMomentFUN",
                                  trace = TRUE)

# Exctracting the weigths
print(extractWeights(opt_gmv_mle))
print(extractWeights(opt_gmv_rob))