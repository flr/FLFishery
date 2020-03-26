# test-FLFisheries.R - DESC
# /test-FLFisheries.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(FLFishery)

data(ple4)

# FLFishery 1 {{{

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

fiA <- FLFishery(PLE=ca, effort=ef, capacity=cap, hperiod=hp, vcost=vc,
  fcost=fc, orevenue=or, crewshare=cs)

# }}}

# FLFishery 2 {{{

# catch, from ple4
ca <- as(ple4, 'FLCatch')[,ac(2000:2005)]
catch.q(ca) <- FLPar(q=0.32)

# price, as wt * 150
price(ca) <- (landings.wt(ca) * 8000) / 1000
units(price(ca)) <- 'EUR / t'

# capacity, runif 19-28 boats
cap <- FLQuant(floor(runif(5, 110, 200)), dimnames=list(year=2000:2005),
  units="boat")

# effort, same number of days per year
ef <- FLQuant(c(212, 117), dimnames=list(quant=c("day", "night"), year=2000:2005),
  units="d / boat")

# hperiod, start 1st Jan, end 31st Dec
hp <- FLQuant(c(0,1), dimnames=list(quant=c("start", "end"), year=2000:2005),
  units="")

# vcost
vc <- FLQuant(c(75, 20), dimnames=list(quant=c("fuel", "ice"), year=2000:2005),
  units="EUR / d")

# fcost
fc <- FLQuant(c(1000, 500), dimnames=list(quant=c("license", "dock"),
  year=2000:2005), units="EUR / boat")

# orevenue
or <- FLQuant(20, dimnames=list(quant=c("tourism"), year=2000:2005),
  units="EUR / boat")

# crewshare
cs <- predictModel(model=~fixed + share * lrevenue,
  params=FLPar(fixed=300, share=0.05, units=c("EUR", "")))

fiB <- FLFishery(PLE=ca, effort=ef, capacity=cap, hperiod=hp, vcost=vc,
  fcost=fc, orevenue=or, crewshare=cs)

# }}}

flfs <- FLFisheries(A=fiA, B=fiB)

# EXTRACT landings.n for a single FLFishery

landings.n(flfs[[1]])

# EXTRACT landings.n by FLFishery

lapply(flfs, landings.n, 1)

# EXTRACT total landings.n by FLFishery

Reduce('+', lapply(flfs, landings.n, 1))

# WORKS if catches have different ages

Reduce('%+%', lapply(flfs, landings.n, 1))
