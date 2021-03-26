# test-accessors.R - DESC
# /test-accessors.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


data(nsfishery)

# FLCatch

# FLFisheries, FLQuants total catch by fleet

# by catch, !sum: [[ple]][bt, gn] [[sol]][bt,gn]
# by catch, sum: [[ple, sol]]

# by fishery, !sum [[bt]][ple, sol] [[gn]][ple, sol]
# by fishery, sum [bt, gn]


catch(nsfleet, by="fishery", sum=TRUE)
catch(nsfleet, by="fishery", sum=FALSE)

catch(nsfleet, by="catch", sum=TRUE)
catch(nsfleet, by="catch", sum=FALSE)

# FLFishery, FLQuants catch by fleet
catch(nsfleet[[1]])

# FLFisheries, catch by fleet single catch

lapply(nsfleet, catch, 1)

landings.n(nsfleet[[1]][[1]])

# FLFishery
landings.n(nsfleet[[1]])


landings.n(nsfleet, 1)
