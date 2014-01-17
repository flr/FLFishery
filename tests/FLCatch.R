# classes.R - DESC
# classes.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

library(FLFishery)

# Example data

data(ple4)

# FLCatch ()

FLCatch()

# Accessors

# landings, discards

# catch, catch.n, catch.wt

# landings.sel, discards.sel

# discards.ratio

# cost

# revenue

# harvest(FLFishery)
setMethod("harvest", signature(object="FLFishery"),
	function(object, alpha=1, beta=1) {

	res <- lapply(object, function(x) catch.q(x) * catch.sel(x) %*% effort(object))

	return(res)
	}
)

# harvest(FLBiol, FLFishery)
#  harvest(FLBiol, catch.n, ini.f=)
