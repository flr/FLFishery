# FLFishery.R - DESC
# FLFishery/tests/testthat/FLFishery.R

# Copyright 2015 Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) 1.1.
#
# Notes:

library(FLCore)
library(FLFishery)

data(ple4)

context("FLFishery constructor")

test_that("FLFishery() works on all inputs", {
  expect_equal(str_length("a"), 1)
  expect_equal(str_length("ab"), 2)
  expect_equal(str_length("abc"), 3)
})

# Catch
ca <- as(ple4, 'FLCatch')
catch.q(ca) <- FLPar(q=0.25)

# price
price(ca) <- landings.wt(ca) * 25
units(price(ca)) <- 'euro'

# effort
ef <- (harvest(ple4)/(catch.q(ca) * catch.sel(ca)))[1,]

fis <- FLFishery(TES=ca, effort=ef)

# CLASS


vcost <- FLQuant(dimnames=list(quant=c("fuelcost")))


# CREATOR

# ACCESSORS

# ECONOMIC METHODS

lrevenue(fis)

vcost(fis)

fcost(fis)

ccost(fis)

cost(fis)

profit(fis)


evalPredictModel(fis@crewshare, fis)

evalPredictModel('crewshare', fis)
