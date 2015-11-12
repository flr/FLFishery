# test-FLFishery.R - DESC
# FLFishery/tests/testthat/test-FLFishery.R

# Copyright European Union, 2015 
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) V.1.1.

library(FLCore)
library(FLFishery)

data(ple4)

context("FLFishery constructor")

# Catch
ca <- as(ple4, 'FLCatch')
catch.q(ca) <- FLPar(q=0.25)

# price
price(ca) <- landings.wt(ca) * 25
units(price(ca)) <- 'euro'

# effort
ef <- (harvest(ple4)/(catch.q(ca) * catch.sel(ca)))[1,]
ef <- FLQuant(c(189, 192), dimnames=list(quant=c("day", "night"), year=1957:2008),
  units="day/boat")


fis <- FLFishery(TES=ca, effort=ef)

# capacity
capacity(fis)[] <- 123
units(capacity(fis)) <- 'boat'
units(fis@effort) <- 'day/boat'

# CLASS

# EFFORT
effort(fis)
effort(fis, compute=FALSE)

effort(fis)['day',]
effort(fis)['day',] <- 2000

effort(fis)$day

vcost <- FLQuant(dimnames=list(quant=c("fuel")))

crewshare(fis)

eval((~effort$day*10)[[2]], list(effort=effort(fis)))

# CREATOR

# ACCESSORS

# ECONOMIC METHODS

lrevenue(fis)

vcost(fis)

fcost(fis)

ccost(fis)

cost(fis)

profit(fis)

predict(fis, 'crewshare')
