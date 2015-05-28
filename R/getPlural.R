# getPlural.R - DESC
# getPlural.R

# Copyright 2003-2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:


# FLCatch -> FLCatchs
setMethod('getPlural', signature(object='FLCatch'),
	function(object) {
		return('FLCatches')})

# FLFishery -> FLFisheries
setMethod('getPlural', signature(object='FLFishery'),
	function(object) {
		return('FLFisheries')})
