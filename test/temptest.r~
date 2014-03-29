data(sp500.subset)
testdata <- sp500.subset[1:50, 1:10]
myCovSpec <- CMEspec(smooth="Boudt", 
                     estim = "mle")
testEst <- Estimate(myCovSpec, testdata)
