# FLFishery.R - DESC
# FLFishery.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# XX {{{
# }}}

library(FLFishery)

load('./LP_ID30_AR_RC_SELF_UR0_TS60-1.RData')

# Biol
bio <- as(tes, 'FLBiol')

# Catch
ca <- as(tes, 'FLCatch')
catch.q(ca) <- FLPar(q=0.25)

# price

ef <- (harvest(tes)/(catch.q(ca) * catch.sel(ca)))[1,]
dimnames(ef)$age <- 'all'

fis <- FLFishery(TES=ca, effort=ef)

fis <- FLFishery(new('FLCatches', list(TES=ca)), effort=ef)

fis <- FLFishery(new('FLCatches', list(TES=ca)))

fis <- FLFishery(FLCatches(TES=ca))


all.equal(harvest(tes), harvest(fis)[[1]])

