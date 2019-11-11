# methods.R - DESC
# FLFishery/R/methods.R

# Copyright European Union, 2015 
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) V.1.1.

# [, [[<- {{{

#' @rdname FLFishery
#' @param x Object to be subset
#' @param i Element to be extracted, by name (character) or position (numeric).
setMethod("[", signature(x="FLFishery", i="ANY", j="missing"),
	function(x, i) {
		x@.Data <- x@.Data[i]
		return(x)
	}
)

#' @rdname FLFishery
setMethod("[[<-", signature(x="FLFishery", i="numeric", j="missing", value="FLCatch"),
	function(x, i, value) {
		x@.Data[[i]] <- value
		return(x)
	}
)

#' @rdname FLFishery
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
#' @rdname FLFishery
setMethod("summary", signature(object="FLFishery"),
  function(object) {

    callNextMethod()

    cat("crewshare     ")
    summary(object@crewshare)
  }
) # }}}

# landings {{{
#' @rdname FLCatch
setMethod("landings", signature(object="FLCatch"),
  function(object) {
    return(quantSums(landings.n(object) * landings.wt(object)))
  }
)

#' @rdname FLFishery
setMethod("landings", signature(object="FLFishery"),
  function(object) {
    return(Reduce("%+%", lapply(object@.Data, landings)))
  }
) # }}}

# discards {{{
#' @rdname FLCatch
setMethod("discards", signature(object="FLCatch"),
  function(object) {
    return(quantSums(discards.n(object) * discards.wt(object)))
  }
)

#' @rdname FLFishery
setMethod("discards", signature(object="FLFishery"),
  function(object) {
    return(Reduce("%+%", lapply(object@.Data, discards)))
  }
) # }}}

# catch {{{
#' @rdname FLCatch
setMethod("catch", signature(object="FLCatch"),
  function(object) {
    return(landings(object) + discards(object))
  }
)

#' @rdname FLFishery
setMethod("catches", signature(object="FLFishery"),
  function(object) {
    res <- lapply(object@.Data, catch)
    names(res) <- names(object)
    return(FLQuants(res))
  }
)

#' @rdname FLFishery
setMethod("catch", signature(object="FLFishery"),
  function(object) {
    return(Reduce("%+%", lapply(object@.Data, catch)))
  }
) # }}}

# catch.n {{{
#' @rdname FLCatch
setMethod("catch.n", signature(object="FLCatch"),
  function(object) {
    return(landings.n(object) + discards.n(object))
  }
) # }}}

# catch.wt {{{
#' @rdname FLCatch
setMethod("catch.wt", signature(object="FLCatch"),
  function(object) {
    return(((landings.wt(object) * (landings.n(object) + 1e-16)) +
      (discards.wt(object) * (discards.n(object) + 1e-16))) / 
        (landings.n(object) + discards.n(object) + 1e-16))
  }
) # }}}

# lrevenue {{{
#' @rdname FLCatch
setMethod("lrevenue", signature(object="FLCatch"),
  function(object) {
    return(quantSums(price(object) * (landings.n(object) * landings.wt(object))))
  }
)
#' @rdname FLFishery
setMethod("lrevenue", signature(object="FLFishery"),
  function(object) {
    return(Reduce("%+%", lapply(object@.Data, lrevenue)))
  }
) # }}}

# revenue {{{
#' @rdname FLFishery
setMethod("revenue", signature(object="FLFishery"),
  function(object) {
    return(quantSums(replace(lrevenue(object), is.na(lrevenue(object)), 0)) +
      quantSums(replace(orevenue(object), is.na(orevenue(object)), 0)) %*%
        capacity(object))
  }
) # }}}

# cost {{{
#' @rdname FLFishery
setMethod("cost", signature(object="FLFishery"),
  function(object) {
    return(quantSums(vcost(object)) + quantSums(fcost(object)) + quantSums(ccost(object)))
  }
) # }}}

# profit {{{
#' @rdname FLFishery
setMethod("profit", signature(object="FLFishery"),
  function(object) {
    return(quantSums(revenue(object)) - quantSums(cost(object)))
  }
) # }}}

# ccost {{{
#' @rdname FLFishery
setMethod("ccost", signature(object="FLFishery"),
  function(object) {
    return(quantSums(crewshare(object)))
  }
) # }}}

# landings.sel, discards.sel {{{
#' @rdname FLCatch
setMethod("landings.sel", signature(object="FLCatch"),
	function(object) {
		res <- catch.sel(object) * (1 - discards.ratio(object))
		return(res %/% apply(res, 2:6, max))
	}
)

#' @rdname FLCatch
setMethod("discards.sel", signature(object="FLCatch"),
	function(object) {
		res <- catch.sel(object) * discards.ratio(object)
		return(res %/% apply(res, 2:6, max))
	}
) # }}}

# discards.ratio {{{
#' @rdname FLCatch
setMethod("discards.ratio", signature(object="FLCatch"),
	function(object) {
		return(discards.n(object) / catch.n(object))
	}
) # }}}

# propagate {{{

#' @rdname FLFishery
#' @param iter Position (numeric) or name (character) of the iter(s) to be extracted (iter), or number of iters to be created (propagate).
#' @param fill.iter Should the object content be copied across the new iters, logical.
setMethod("propagate", signature(object="FLFishery"),
  function(object, iter, fill.iter=TRUE) {
    
    res <- callNextMethod()
    res@.Data <- lapply(res@.Data, propagate, iter=iter)

    return(res)
  }
)
# }}}

# iter {{{
#' @rdname FLFishery
#' @param obj Object on which to apply method
setMethod("iter", signature(obj="FLFishery"),
	  function(obj, iter) {

    # SUBSET iter in FLQ slots
    res <- callNextMethod()

    # SUBSET FLCatches
    res[seq(length(obj))] <- lapply(res@.Data, "iter", iter=iter)

    return(res)
	  }
) # }}}

# iter<- {{{
#' @rdname FLFishery
#' @param object Object on which to assign value
#' @param value Object to assign
setMethod("iter<-", signature(object="FLFishery", value="FLFishery"),
	  function(object, iter, value) {

    # FLQs
    for(i in c("capacity", "effort", "hperiod", "vcost", "fcost", "orevenue"))
      slot(object, i)[,,,,,iter] <- slot(value, i)

    # pM
    #iter(slot(object, "crewshare")@params, iter) <-
    #  slot(value, "crewshare")@params

    return(object)
	  }
) # }}}

# npv {{{
npv <- function(object, drate, refYear=dims(object)$minyear) {

  # net revenue
  reven <- window(profit(object), start=as.numeric(refYear))

  t <- dim(reven)[2]

  res <- yearSums(reven * exp(-drate * t))
  dimnames(res)$year <- dims(object)$maxyear

  return(res)
}
# }}}

# verify

# FLCatch

# FLFishery
