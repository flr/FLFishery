# accessors.R - DESC
# accessors.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# FLCatch {{{

# landings.n
setMethod("landings.n", signature(object="FLCatch"),
	function(object) {
		return(slot(object, "landings.n"))
	}
)
setReplaceMethod("landings.n", signature(object="FLCatch", value="FLQuant"),
	function(object, value) {
		slot(object, "landings.n") <- value
		return(object)
	}
)

# landings.wt
setMethod("landings.wt", signature(object="FLCatch"),
	function(object) {
		return(slot(object, "landings.wt"))
	}
)
setReplaceMethod("landings.wt", signature(object="FLCatch", value="FLQuant"),
	function(object, value) {
		slot(object, "landings.wt") <- value
		return(object)
	}
)

# discards.n
setMethod("discards.n", signature(object="FLCatch"),
	function(object) {
		return(slot(object, "discards.n"))
	}
)
setReplaceMethod("discards.n", signature(object="FLCatch", value="FLQuant"),
	function(object, value) {
		slot(object, "discards.n") <- value
		return(object)
	}
)

# discards.wt
setMethod("discards.wt", signature(object="FLCatch"),
	function(object) {
		return(slot(object, "discards.wt"))
	}
)
setReplaceMethod("discards.wt", signature(object="FLCatch", value="FLQuant"),
	function(object, value) {
		slot(object, "discards.wt") <- value
		return(object)
	}
)

# catch.sel
setMethod("catch.sel", signature(object="FLCatch"),
	function(object) {
		return(slot(object, "catch.sel"))
	}
)
setReplaceMethod("catch.sel", signature(object="FLCatch", value="FLQuant"),
	function(object, value) {
		slot(object, "catch.sel") <- value
		return(object)
	}
)

# price
setMethod("price", signature(object="FLCatch"),
	function(object) {
		return(slot(object, "price"))
	}
)
setReplaceMethod("price", signature(object="FLCatch", value="FLQuant"),
	function(object, value) {
		slot(object, "price") <- value
		return(object)
	}
)

# catch.q
setMethod("catch.q", signature(object="FLCatch"),
	function(object) {
		return(slot(object, "catch.q"))
	}
)
setReplaceMethod("catch.q", signature(object="FLCatch", value="FLPar"),
	function(object, value) {
		slot(object, "catch.q") <- value
		return(object)
	}
)
# }}}

# FLFishery {{{

# effort
setMethod("effort", signature(object="FLFishery"),
	function(object) {
		return(slot(object, "effort"))
	}
)
setReplaceMethod("effort", signature(object="FLFishery", value="FLQuant"),
	function(object, value) {
		slot(object, "effort") <- value
		return(object)
	}
)

# orevenue
setMethod("orevenue", signature(object="FLFishery"),
	function(object) {
		return(slot(object, "orevenue"))
	}
)
setReplaceMethod("orevenue", signature(object="FLFishery", value="FLQuant"),
	function(object, value) {
		slot(object, "orevenue") <- value
		return(object)
	}
)

# vcost
setMethod("vcost", signature(object="FLFishery"),
	function(object) {
		return(slot(object, "vcost"))
	}
)
setReplaceMethod("vcost", signature(object="FLFishery", value="FLQuant"),
	function(object, value) {
		slot(object, "vcost") <- value
		return(object)
	}
)

# fcost
setMethod("fcost", signature(object="FLFishery"),
	function(object) {
		return(slot(object, "fcost"))
	}
)
setReplaceMethod("fcost", signature(object="FLFishery", value="FLQuant"),
	function(object, value) {
		slot(object, "fcost") <- value
		return(object)
	}
) # }}}

# FLFisheries {{{
setMethod('landings.n', signature(object='FLFisheries'),
	function(object) {
		lapply(object, landings.n)
	}
) # }}}
