# methods.R - DESC
# FLFishery/R/methods.R

# Copyright European Union, 2015 
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) V.1.1.

# [, [[<- {{{
setMethod("[", signature(x="FLFishery", i="ANY", j="missing"),
	function(x, i) {
		x@.Data <- x@.Data[i]
		return(x)
	}
)

setMethod("[[<-", signature(x="FLFishery", i="numeric", j="missing", value="FLCatch"),
	function(x, i, value) {
		x@.Data[[i]] <- value
		return(x)
	}
)

setMethod("[[<-", signature(x="FLFishery", i="character", j="missing", value="FLCatch"),
	function(x, i, value) {

    # MATCH with existing,
    idx <- match(i, names(x))
    # otherwise it is new
    if(is.na(idx)) {
      idx <- i
      nms <- c(names(x), i)
		  x@.Data[[idx]] <- value
      names(x) <- nms
    } else {
		  x@.Data[[idx]] <- value
    }
		return(x)
	}
) # }}}

# summary {{{
setMethod("summary", signature(object="FLFishery"),
  function(object) {

    callNextMethod()

    cat("crewshare     ")
    summary(object@crewshare)
  }
) # }}}

# landings {{{
setMethod("landings", signature(object="FLCatch"),
  function(object) {
    return(quantSums(landings.n(object) * landings.wt(object)))
  }
)

setMethod("landings", signature(object="FLFishery"),
  function(object) {
    return(Reduce("%+%", lapply(object@.Data, landings)))
  }
) # }}}

# discards {{{
setMethod("discards", signature(object="FLCatch"),
  function(object) {
    return(quantSums(discards.n(object) * discards.wt(object)))
  }
)

setMethod("discards", signature(object="FLFishery"),
  function(object) {
    return(Reduce("%+%", lapply(object@.Data, discards)))
  }
) # }}}

# catch {{{
setMethod("catch", signature(object="FLCatch"),
  function(object) {
    return(landings(object) + discards(object))
  }
)

setMethod("catches", signature(object="FLFishery"),
  function(object) {
    res <- lapply(object@.Data, catch)
    names(res) <- names(object)
    return(res)
  }
)

setMethod("catch", signature(object="FLFishery"),
  function(object) {
    return(Reduce("%+%", lapply(object@.Data, catch)))
  }
) # }}}

# catch.n {{{
setMethod("catch.n", signature(object="FLCatch"),
  function(object) {
    return(landings.n(object) + discards.n(object))
  }
) # }}}

# catch.wt {{{
setMethod("catch.wt", signature(object="FLCatch"),
  function(object) {
    return((landings.wt(object) * landings.n(object) + discards.wt(object) *
      discards.n(object)) / catch.n(object))
  }
) # }}}

# lrevenue {{{
#' @rdname FLCatch
#' @aliases lrevenue,FLCatch-method
setMethod("lrevenue", signature(object="FLCatch"),
  function(object) {
    return(quantSums(price(object) * (landings.n(object) * landings.wt(object))))
  }
)
#' @rdname FLFishery
#' @aliases lrevenue,FLFishery-method
setMethod("lrevenue", signature(object="FLFishery"),
  function(object) {
    return(Reduce("%+%", lapply(object@.Data, lrevenue)))
  }
) # }}}

# revenue {{{
setMethod("revenue", signature(object="FLFishery"),
  function(object) {
    return(quantSums(replace(lrevenue(object), is.na(lrevenue(object)), 0)) +
      quantSums(replace(orevenue(object), is.na(orevenue(object)), 0)) %*%
        capacity(object))
  }
) # }}}

# cost {{{
setMethod("cost", signature(object="FLFishery"),
  function(object) {
    return(quantSums(vcost(object)) + quantSums(fcost(object)) + quantSums(ccost(object)))
  }
) # }}}

# profit {{{
setMethod("profit", signature(object="FLFishery"),
  function(object) {
    return(quantSums(revenue(object)) - quantSums(cost(object)))
  }
) # }}}

# ccost {{{
setMethod("ccost", signature(object="FLFishery"),
  function(object) {
    return(quantSums(crewshare(object)))
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

# propagate {{{
setMethod("propagate", signature(object="FLFishery"),
  function(object, iter, fill.iter=TRUE) {
    
    res <- callNextMethod()
    res@.Data <- lapply(res@.Data, propagate, iter=iter)

    return(res)
  }
)
# }}}
