# test-computation.R - DESC
# /test-computation.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# nsfleet, ple, sol

data(nsfishery)

# catch, catches

# catch.n

names(catch.n(nsfleet))
is(catch.n(nsfleet)[[1]])
