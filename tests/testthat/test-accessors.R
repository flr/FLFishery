# test-accessors.R - DESC
# /test-accessors.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


data(nsfishery)

# FLCatch

landings.n(nsfleet[[1]][[1]])

# FLFishery
landings.n(nsfleet[[1]])


landings.n(nsfleet, 1)
