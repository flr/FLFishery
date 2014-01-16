# constructors.R - DESC
# constructors.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# FLCatch() {{{
setMethod("FLCatch", signature(object="FLQuant"),
	function(object, ...) {

		# [...]
		args <- list(...)

		# empty object
		object[] <- as.numeric(NA)
		dmns <- dimnames(object)
		dmn <- dim(object)

		res <- new("FLCatch",
			landings.n=object,
			landings.wt=object,
			discards.n=object,
			discards.wt=object,
			catch.sel=object,
			price=object,
			catch.q=FLPar(q=NA),
			range=c(min=as.numeric(dmns[[1]][1]), max=as.numeric(dmns[[1]][dmn[1]]),
				plusgroup=as.numeric(dmns[[1]][dmn[1]]), minyear=as.numeric(dmns$year[1]),
				maxyear=as.numeric(dmns$year[dmn[2]]))
		)

		for(i in names(args))
			slot(res, i) <- args[[i]]

		return(res)
	}
)

setMethod("FLCatch", signature(object="missing"),
	function(object, ...) {

		# [...]
		args <- list(...)

		# empty object
		idx <- unlist(lapply(args, is, 'FLQuant'))

		# No FLQuant passed
		if(sum(idx) == 0)
			return(FLCatch(object=FLQuant(quant='age'), ...))

		# else
		object <- args[[names(args)[idx][1]]]

		return(FLCatch(object, ...))
	}
) # }}}


# FLFishery()

# list
setMethod("FLFishery", signature(object="list"),
	function(object, ...) {

		cas <- new("FLCatches", object)

		res <- new("FLFishery", cas)

		args <- list(...)

		for (i in names(args))
			slot(res, i) <- args[[i]]

		return(res)
	}
)

# FLCatch
setMethod("FLFishery", signature(object="FLCatch"),
	function(object, ...) {

		cas <- new("FLCatches", list(object))
		return(FLFishery(cas, ...))
	}
)

# missing
setMethod("FLFishery", signature(object="missing"),
	function(object, ...) {

		args <- list(...)

		# FLCatch
		idc <- lapply(args, is, 'FLCatch')

		cas <- new("FLCatches", args[unlist(idc)])

		args <- args[!unlist(idc)]

		return(do.call("FLFishery", c(list(object=cas), args)))
		
	}
)

