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
		dmn <- dims(object)

		res <- new("FLCatch",
			landings.n=object,
			landings.wt=object,
			discards.n=object,
			discards.wt=object,
			catch.sel=object,
			price=object,
			catch.q=FLPar(q=NA),
			range=c(min=dmn$min, max=dmn$max,	plusgroup=dmn$max, minyear=dmn$minyear,
				maxyear=dmn$maxyear)
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
			return(
						 FLCatch(object=FLQuant(quant='age'), ...)
						 )

		# else
		object <- args[[names(args)[idx][1]]]

		return(FLCatch(object, ...))
	}
) # }}}

# FLFishery() {{{

# list
setMethod("FLFishery", signature(object="list"),
	function(object, ...) {

		args <- list(...)

		cas <- new("FLCatches", object)

		# adjust years in cas if needed
		dmns <- lapply(cas, function(x) unlist(dims(x)[c('year', 'minyear', 'maxyear')]))
		dmns <- matrix(unlist(dmns), ncol=3, byrow=T, dimnames=list(names(dmns),
			c('year', 'minyear', 'maxyear')))

		# extend to maxyear
		if(any(dmns[,'maxyear'] < max(dmns[,'maxyear']))) {
			cas <- lapply(cas, function(x) window(x, end=max(dmns[,'maxyear'])))
		}

		# and to minyear
		if(any(dmns[,'minyear'] < min(dmns[,'minyear']))) {
			cas <- lapply(cas, function(x) window(x, start=min(dmns[,'minyear'])))
		}

		# search for FLQs in args
		idq <- unlist(lapply(args, is, 'FLQuant'))
		
		# if any ...
		if(sum(idq) > 0) {
			# ... select first one
			flq <- args[[names(idq)[idq][1]]]
			flq[] <- NA
		} else {
			flq <- FLQuant(dimnames=c(list(age='all'), dimnames(landings.n(cas[[1]]))[-1]))
		}

		# create new object
		res <- new("FLFishery", cas, effort=flq, vcost=flq, fcost=flq)

		# fill slots provided
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
) # }}}
