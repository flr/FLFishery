# test-FLFishery.R - DESC
# FLFishery/tests/testthat/test-FLFishery.R

# Copyright European Union, 2015 
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) V.1.1.

data(ple4)

# EXAMPLE dataset {{{

# catch, from ple4
ca <- as(ple4, 'FLCatch')[,ac(2000:2005)]
catch.q(ca) <- FLPar(q=0.25)

# price, as wt * 150
price(ca) <- (landings.wt(ca) * 15000) / 1000
units(price(ca)) <- 'EUR / t'

# capacity, runif 19-28 boats
cap <- FLQuant(floor(runif(5, 19, 28)), dimnames=list(year=2000:2005),
  units="boat")

# effort, same number of days per year
ef <- FLQuant(c(225, 212), dimnames=list(quant=c("day", "night"), year=2000:2005),
  units="d / boat")

# hperiod, start 1st Jan, end 31st Dec
hp <- FLQuant(c(0,1), dimnames=list(quant=c("start", "end"), year=2000:2005),
  units="")

# vcost
vc <- FLQuant(c(100, 10), dimnames=list(quant=c("fuel", "ice"), year=2000:2005),
  units="EUR / d")

# fcost
fc <- FLQuant(c(1000, 500), dimnames=list(quant=c("license", "dock"),
  year=2000:2005), units="EUR / boat")

# orevenue
or <- FLQuant(200, dimnames=list(quant=c("tourism"), year=2000:2005),
  units="EUR / boat")

# crewshare
cs <- predictModel(model=~fixed + share * lrevenue,
  params=FLPar(fixed=300, share=0.05, units=c("EUR", "")))

# FLFishery
fis <- FLFishery(PLE=ca, SOL=ca, effort=ef, capacity=cap, hperiod=hp, vcost=vc,
  fcost=fc, orevenue=or, crewshare=cs)
# }}}

# FLCatch methods

landings.n(fis[['PLE']])
discards.n(fis[['PLE']])
catch.n(fis[['PLE']])
catch.wt(fis[['PLE']])
catch(fis[['PLE']])
lrevenue(fis[['PLE']])
landings.sel(fis[['PLE']])
discards.sel(fis[['PLE']])
discards.ratio(fis[['PLE']])

# FLFishery ACCESSORS

# DIRECT
capacity(fis)
hperiod(fis)

# COMPUTING
effort(fis)
vcost(fis)
fcost(fis)
orevenue(fis)
crewshare(fis)

# FLFishery METHODS

landings(fis)
landings.n(fis)
discards(fis)
catch(fis)
catches(fis)
lrevenue(fis)
revenue(fis)
cost(fis)
ccost(fis)
profit(fis)




# CREATOR

# ACCESSORS

# TEST THAT accessors exist and work
test_that("FLFishery accessors exist", {
 
  # EXPECT accessors return FLQuant
  expect_is(capacity(fis), "FLQuant")
  expect_is(effort(fis), "FLQuant")
  expect_is(hperiod(fis), "FLQuant")
  expect_is(vcost(fis), "FLQuant")
  expect_is(fcost(fis), "FLQuant")
  expect_is(orevenue(fis), "FLQuant")


})
