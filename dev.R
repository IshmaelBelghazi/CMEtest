load_all()
data(sp500.subset)
myCovSpec <- CMEspec(estimCtrl = list(corr = TRUE), filter = "MP", filterCtrl = list(kernel = "gaussian", bw = "SJ", change.vars = FALSE, fit.type = "median", Q.mult = 1 ))
myCovEst <- Estimate(myCovSpec, sp500.subset)

x <- myCovEst$.filterEstim$eigDens$x
y <- myCovEst$.filterEstim$eigDens$y
estimates <- myCovEst$.filterEstim$mpEstimates
Vars <- estimates[1]
Qs <-estimates[2]

plot(x,y, "lines")
lines(x, y = dmp(x, Vars, Qs))
