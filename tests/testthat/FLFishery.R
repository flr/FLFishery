# FLFishery.R - DESC
# FLFishery/tests/testthat/FLFishery.R

# Copyright 2015 Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) 1.1.
#
# Notes:

# XX {{{
# }}}

library(FLFishery)

load('../LP_ID30_AR_RC_SELF_UR0_TS60-1.RData')

# Catch
ca <- as(tes, 'FLCatch')
catch.q(ca) <- FLPar(q=0.25)

# price

ef <- (harvest(tes)/(catch.q(ca) * catch.sel(ca)))[1,]
dimnames(ef)$age <- 'all'

fis <- FLFishery(TES=ca, effort=ef)

# CLASS

fca <- as(ple4, "FLCatch")

# CREATOR

# ACCESSORS



