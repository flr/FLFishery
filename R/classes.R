# FLFishery.R - DESC
# FLFishery.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# FLCatch {{{
setClass("FLCatch",
	contains='FLComp',
	representation(
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
		catch.q = FLPar(q=NA)),
	
	# VALIDITY
	validity=function(object) {
	
		# dims[1:5]
		# iter 1 or N
		# catch.q params='q' and iter 1 or N
		# catch.q dims equal to flqs
		return(TRUE)
	}
) # }}}

# FLCatches {{{
setClass("FLCatches",
	
	contains=c("FLlst"),

	# VALIDITY
	validity=function(object) {

		# all object are FLCatch
		if(any(!unlist(lapply(object, is, 'FLCatch'))))
			return("Input objects must be of class 'FLCatch'")

		dmns <- lapply(object, dims)

		# quant == 'age'
		qua <- unlist(lapply(dmns, '[', 'quant'))
		if(length(unique(qua)) > 1)
			return("FLCatch objects must have quant='age'")

		# dims [c(2,3,4,5)] must be the same
		dmns <- lapply(object, function(x) dimnames(landings.n(x))[-c(1, 6)])
		if(sum(duplicated(dmns)) != (length(dmns) -1))
			return(paste("All FLCatch objects must share dimensions 2 to 5: ",
				names(dmns)[!duplicated(dmns)][-1]))

		# iters 1 or N

		return(TRUE)
	}
) # }}}

# FLFishery {{{
setClass("FLFishery",
	contains=c('FLComp', 'FLCatches'),
	representation(
		effort="FLQuant",
		vcost="FLQuant",
		fcost="FLQuant"),
	prototype(
		effort=FLQuant(),
		vcost=FLQuant(),
		fcost=FLQuant()),
	
	# VALIDITY
	validity=function(object) {
		
		# dims[2:5] of flqs match dims of flcs

		# iters 1 or N

		# effort, vcost and fcost, age='all'

		return(TRUE)
	}
) # }}}

# FLFisheries {{{
setClass("FLFisheries", contains=c("FLlst"),

	# VALIDITY
	validity=function(object) {

		# all objects are FLFishery
		if(any(!unlist(lapply(object, is, 'FLFishery'))))
			return("Input objects must be of class 'FLFishery'")

		# dmns <- lapply(object, dims)

		# quant == 'age'
		# qua <- unlist(lapply(dmns, '[', 'quant'))
		# if(length(unique(qua)) > 1)
		# 	return("FLFishery objects must have quant='age'")

		# iters 1 or N

		return(TRUE)
	}
) # }}}
