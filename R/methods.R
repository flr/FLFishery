# methods.R - DESC
# FLFishery/R/m.R

# Copyright 2015 Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) 1.1.
#
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

# landings {{{
setMethod("landings", signature(object="FLCatch"),
  function(object) {
    return(quantSums(landings.n(object) * landings.wt(object)))
  }
)

setMethod("landings", signature(object="FLFishery"),
  function(object) {
    return(Reduce('%+%', lapply(object@.Data, landings)))
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
    return(Reduce('%+%', lapply(object@.Data, discards)))
  }
) # }}}

# catch {{{
setMethod("catch", signature(object="FLCatch"),
  function(object) {
    return(landings(object) + discards(object))
  }
)

setMethod("catch", signature(object="FLFishery"),
  function(object) {
    return(Reduce('%+%', lapply(object@.Data, catch)))
  }
) # }}}

# lrevenue {{{
setMethod("lrevenue", signature("FLCatch"),
  function(object) {
    return(quantSums(price(object) * landings.n(object)))
  }
)
setMethod("lrevenue", signature("FLFishery"),
  function(object) {
    return(Reduce('%*%', lapply(object@.Data, lrevenue)))
  }
) # }}}

# cost {{{
setMethod("cost", signature("FLFishery"),
  function(object) {
    return(quantSums(vcost(object)) + quantSums(fcost(object)) + quantSums(ccost(object)))
  }
) # }}}

# profit {{{
setMethod("profit", signature("FLFishery"),
  function(object) {
    return(revenue(object) - cost(object))
  }
) # }}}

# ccost {{{
setMethod("ccost", signature(object="FLFishery"),
  function(object) {
    return(evalPredictModel(object, slot='crewshare'))
  }
) # }}}

# ccost {{{
setMethod("ccost", signature(object="FLFishery"),
  function(object) {
    return(evalPredictModel(object, slot='crewshare'))
  }
) # }}}

# vcost * effort * capacity


# ---

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

# plot {{{
setMethod("plot", signature(x="FLCatch", y="missing"),
	function(x, ...) {

		fqs <- FLQuants(Catch=catch(x), DiscardsRatio=discards.ratio(x), Price=price(x))

		p <- plot(fqs)

		p <- ggplot(data=catch(x), aes(x=year, y=data)) + geom_line() 

		p + geom_bar(data=as.data.frame(discards(x)), aes(x=year, y=data), fill='red', colour='darkred', alpha=0.5, stat='identity')
	})
# }}}

# harvest(FLFishery) {{{
setMethod("harvest", signature(object="FLFishery"),
	function(object) {

	res <- lapply(object, function(x) catch.q(x) * catch.sel(x) %*% effort(object))

	return(res)
	}
) # }}}


# ---

setGeneric('evalPredictModel', function(model, data, ...) standardGeneric('evalPredictModel'))

# evalPredictModel {{{
setMethod("evalPredictModel", signature(model="predictModel", data="FLComp"),
  function(model, data) {

    args <- all.names(model@model, functions=FALSE)

    res <- as(model, 'list')

    # MISSING args?
    args <- args[!args %in% names(res)]

    if(length(args) > 0 ) {

      # CALL methods on object (inc. accessors)
      for(i in args) {
        res[[i]] <- do.call(i, list(data))
      }
  }

  # RETURN
  return(eval(model@model[[2]], res))
}) # }}}

# evalPredictModel {{{
setMethod("evalPredictModel", signature(model="character", data="FLComp"),
  function(model, data) {

    model <- slot(data, model)

    args <- all.names(model@model, functions=FALSE)

    res <- as(model, 'list')

    # MISSING args?
    args <- args[!args %in% names(res)]

    if(length(args) > 0 ) {

      # CALL methods on object (inc. accessors)
      for(i in args) {
        res[[i]] <- do.call(i, list(data))
      }
  }

  # RETURN
  return(eval(model@model[[2]], res))
}) # }}}

