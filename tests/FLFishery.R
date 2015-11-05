# FLFishery.R - DESC
# FLFishery.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

library(FLFishery)

data(ple4)

# Catch
ca <- as(ple4, 'FLCatch')
catch.q(ca) <- FLPar(q=0.25)

# price
price(ca) <- stock.wt(ple4) * 25
units(price(ca)) <- 'euro'

# effort
ef <- (harvest(ple4)/(catch.q(ca) * catch.sel(ca)))[1,]
dimnames(ef) <- list(quant='all')
units(ef) <- 'days'

# FLFishery
fis <- FLFishery(PLE=ca, effort=ef)

# boats.n
fis@boats.n[] <- 25

revenue(fis)
