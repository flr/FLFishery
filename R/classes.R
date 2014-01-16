# FLFishery.R - DESC
# FLFishery.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# FLCatch {{{

# validity
# - catch.q must have params='q'
# -

setClass("FLCatch",
	representation(
		"FLComp",
		landings.n = "FLQuant",
		landings.wt = "FLQuant",
		discards.n = "FLQuant",
		discards.wt = "FLQuant",
		catch.sel = "FLQuant",
		price = "FLQuant",
		catch.q = "FLPar"),
	prototype(
		name = character(0),
		desc = character(0),
	  range = as.numeric(c(min=NA, max=NA, plusgroup=NA,
			minyear=NA, maxyear=NA)),
		landings.n = FLQuant(),
		landings.wt = FLQuant(),
		discards.n = FLQuant(),
		discards.wt = FLQuant(),
		catch.sel = FLQuant(),
		price = FLQuant(),
		catch.q = FLPar(q=NA))
) # }}}

# FLCatches {{{
setClass("FLCatches", contains=c("FLlst"))
# }}}

# FLFishery {{{
setClass("FLFishery",
	representation(
		"FLCatches",
		effort="FLQuant",
		vcost="FLQuant",
		fcost="FLQuant"),
	prototype(
		effort=FLQuant(),
		vcost=FLQuant(),
		fcost=FLQuant())
) # }}}
