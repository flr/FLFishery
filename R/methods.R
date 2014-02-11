# methods.R - DESC
# methods.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# [, [[<- {{{
setMethod('[', signature(x="FLFishery", i="ANY", j="missing"),
	function(x, i) {
		x@.Data <- x@.Data[i]
		return(x)
	}
)

setMethod('[[<-', signature(x="FLFishery", i="ANY", j="missing", value="FLCatch"),
	function(x, i, value) {
		x@.Data[[i]] <- value
		return(x)
	}
)

# }}}

# landings, discards {{{
setMethod("landings", signature(object="FLCatch"),
	function(object) {
		return(quantSums(landings.n(object) * landings.wt(object)))
	}
)

setMethod("landings", signature(object="FLFishery"),
	function(object, stock=missing) {

		if(!missing(stock))
			res <- lapply(object[stock], landings)
		else
			res <- lapply(object, landings)
		
		if(length(res) == 1)
			return(res[[1]])
		else
			return(res)
	}
)

setMethod("discards", signature(object="FLCatch"),
	function(object) {
		return(quantSums(discards.n(object) * discards.wt(object)))
	}
)


# }}}

# catch, catch.n, catch.wt, {{{
setMethod("catch", signature(object="FLCatch"),
	function(object) {
		return(quantSums(landings.n(object) * landings.wt(object) +
			discards.n(object) * discards.wt(object)))
	}
)

setMethod("catch", signature("FLFishery"),
	function(object) {
		catch(object@.Data)
	}
)

setMethod("catch.n", signature(object="FLCatch"),
	function(object) {
		return(landings.n(object) + discards.n(object))
	}
)

setMethod("catch.wt", signature(object="FLCatch"),
	function(object) {
		return((landings.wt(object) * landings.n(object) +
					 discards.wt(object) * discards.n(object)) /
					 (landings.n(object) * discards.n(object)))
	}
) # }}}

# landings.sel, discards.sel {{{
setMethod("landings.sel", signature(object="FLCatch"),
	function(object) {
		res <- catch.sel(object) * (1 - discards.ratio(object))
		return(res %/% apply(res, 2:6, max))
	}
)

setMethod("discards.sel", signature(object="FLCatch"),
	function(object) {
		res <- catch.sel(object) * discards.ratio(object)
		return(res %/% apply(res, 2:6, max))
	}
) # }}}

# discards.ratio {{{
setMethod("discards.ratio", signature(object="FLCatch"),
	function(object) {
		return(discards.n(object) / catch.n(object))
	}
) # }}}

# cost {{{
setMethod("cost", signature("FLFishery"),
	function(object) {
		return(vcost(object) + fcost(object))
	}
) # }}}

# revenue {{{
setMethod("revenue", signature("FLFishery"),
	function(object) {
		return(quantSums(price(object) * landings.n(object)) - cost(object))
	}
) # }}}

# plot {{{
setMethod("plot", signature(x="FLCatch", y="missing"),
	function(x, ...) {

		fqs <- FLQuants(Catch=catch(x), DiscardsRatio=discards.ratio(x), Price=price(x))

		p <- plot(fqs)

		p <- ggplot(data=catch(x), aes(x=year, y=data)) + geom_line() 

		p + geom_bar(data=as.data.frame(discards(x)), aes(x=year, y=data), fill='red', colour='darkred', alpha=0.5, stat='identity')

		plot(fqs)
	})
# {{{
