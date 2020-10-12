# sim.R - DESC
# /sim.R

# Copyright Iago MOSQUEIRA (WMR), 2019
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the GPL 3.0


library(FLasher)


# 2 FLFishery w/ 2 FLCatch each

data(mixed_fishery_example_om)

names(flfs[[1]]) <- c("ple", "sol")
names(flfs[[2]]) <- c("ple", "sol")

years <- 2:20

fcb <- matrix(c(1,1,1,1,2,2,2,1,1,2,2,2), byrow=TRUE, ncol=3,
  dimnames=list(1:4,c("F","C","B")))

sole_catch_target <- 8000
plaice_bt_gn_catch_relative <- 1.2

flasher_ctrl <- fwdControl(
    list(year=years, quant="catch",biol="sol", value=sole_catch_target),
    list(year=years, quant="catch", relYear=2:20, fishery="bt", catch="ple",
      relFishery="gn", relCatch="ple", value=plaice_bt_gn_catch_relative),
  FCB=fcb)

run <- fwd(object=biols, fishery=flfs, control=flasher_ctrl)

ple <- run$biols[['ple']]
sol <- run$biols[['sol']]

nsfleet <- run$fisheries

save(nsfleet, ple, sol, file="../data/nsfishery.RData", compress="xz")
