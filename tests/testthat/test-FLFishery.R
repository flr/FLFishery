# test-FLFishery.R - DESC
# FLFishery/tests/testthat/test-FLFishery.R

# Copyright European Union, 2015 
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) V.1.1.


data(ple4)

context("FLFishery constructor")

# catch, from ple4
ca <- as(ple4, 'FLCatch')[,ac(2000:2005)]
catch.q(ca) <- FLPar(q=0.25)

# price, as wt * 25
price(ca) <- landings.wt(ca) * 25
units(price(ca)) <- 'euro / kg'

# capacity, runif 19-28 boats
cap <- FLQuant(floor(runif(5, 19, 28)), dimnames=list(year=2000:2005),
  units="boat")

# effort, same number of days per year
ef <- FLQuant(c(225, 212), dimnames=list(quant=c("day", "night"), year=2000:2005),
  units="day / boat")

# hperiod, start 1st Jan, end 31st Dec
hp <- FLQuant(c(0,1), dimnames=list(quant=c("start", "end"), year=2000:2005))

# vcost
vc <- FLQuant(c(1000, 10), dimnames=list(quant=c("fuel", "ice"), year=2000:2005),
  units="euro / day / boat")

# fcost
fc <- FLQuant(c(10000, 5000), dimnames=list(quant=c("license", "dock"),
  year=2000:2005), units="euro / boat")

# orevenue
or <- FLQuant(200, dimnames=list(quant=c("tourism"), year=2000:2005),
  units="euro / boat")

# crewshare
cs <- predictModel(model=~fixed + share * lrevenue,
  params=FLPar(fixed=300, share=0.05))

# FLFishery
fis <- FLFishery(PLE=ca, SOL=ca, effort=ef, capacity=cap, hperiod=hp, vcost=vc,
  fcost=fc, orevenue=or, crewshare=cs)

fis <- FLFishery(PLE=ca, SOL=ca, effort=ef, capacity=cap, vcost=vc,
  fcost=fc, orevenue=or, crewshare=cs)

# $
#model(crewshare(fis)) <- ~fixed + share * lrevenue - vcost$ice / 100
ccost(fis)
cost(fis)

# CLASS

# EFFORT
effort(fis)
effort(fis, compute=FALSE)

effort(fis)$day

quantSums(effort(fis))

crewshare(fis)

ccost(fis)

# CREATOR

# ACCESSORS

# ECONOMIC METHODS

lrevenue(fis)

vcost(fis)

fcost(fis)

ccost(fis)

cost(fis)

profit(fis)


# predict

# predict(fis, 'crewshare')
