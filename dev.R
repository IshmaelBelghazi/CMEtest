load_all()
data(sp500.subset)
myCovSpec <- CMEspec(estim = "mle",
                     estimCtrl = list(corr = TRUE),
                     filter = "MP",
                     filterCtrl = list(fit.type = "analogic", norm.meth = "partial"))
myCovEst <- Estimate(myCovSpec, sp500.subset)

plot(myCovEst$.filterEstim$eigHist)
estimates <- myCovEst$.filterEstim$mpEstimates
sigma <- estimates[1]
Q <-estimates[2]
lines(x = seq(0,30,0.001), y = dmarpas(x = seq(0,30,0.001), Vars, Qs))
