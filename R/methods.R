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
)

setMethod('summary', signature(object='FLFisheries'),
  function(object)
  {
	  cat("An object of class \"", class(object), "\"\n\n", sep="")
		cat("Elements:", names(object), "\n")
    cat("\n")
    for(i in seq(1, length(object)))
    {
		  qnames <- getSlotNamesClass(object[[i]], 'FLArray')
      qdims <- dims(object[[i]])

      cat("Name:", name(object[[i]]), "\n")
  		cat("\tDescription:", desc(object[[i]]), "\n")
	  	cat("\tRange:\t", paste(sub('plusgroup', 'pgroup', names(range(object[[i]]))),
        collapse="\t"), "\n")
  		cat("\t", range(object[[i]]), "\n", sep="\t")
	  	cat("\tQuant:", qdims$quant, "\n")
	  	cat("\tdim:", unlist(qdims[c(qdims$quant, 'year', 'unit', 'season', 'area')]
          , use.names=FALSE), "\n")

      cat("\tcatches:", names(object[[i]]), "\n")
   }
  }
)

# }}}

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

# fwdWindow {{{
setMethod("fwdWindow", signature(x="FLFishery", y="missing"),
  function(x, end=dims(x)$maxyear, nsq=3) {

    # CALL window if not to extend
    if(end <= dims(x)$maxyear)
      return(window(x, end=end))

    res <- window(x, end=end)

    # YEARS for mean
    myrs <- tail(dimnames(capacity(x))$year, nsq)
    # NEW years
    nyrs <- ac(seq(dims(x)$maxyear + 1, end))

    # AVERAGES for nsq years
    capacity(res)[, nyrs] <- yearMeans(capacity(res)[, myrs])
    effort(res)[, nyrs] <- yearMeans(effort(res)[, myrs])
    hperiod(res)[, nyrs] <- yearMeans(hperiod(res)[, myrs])
    vcost(res)[, nyrs] <- yearMeans(vcost(res)[, myrs])
    fcost(res)[, nyrs] <- yearMeans(fcost(res)[, myrs])
    orevenue(res)[, nyrs] <- yearMeans(orevenue(res)[, myrs])

    if(length(crewshare(res, FALSE)) > 0) {
      stop("TODO")
    }

    # FLCatches
    res[seq(length(res))] <- lapply(res, fwdWindow, end=end)

    return(res)
  }
)

setMethod("fwdWindow", signature(x="FLCatch", y="missing"),
  function(x, end=dims(x)$maxyear, nsq=3) {
  
    # CALL window if not to extend
    if(end <= dims(x)$maxyear)
      return(window(x, end=end))

    res <- window(x, end=end)

    # YEARS for mean
    myrs <- tail(dimnames(landings.n(x))$year, nsq)
    # NEW years
    nyrs <- ac(seq(dims(x)$maxyear + 1, end))

    # AVERAGES for nsq years, only for landings/discards ratio
    lans <- yearMeans(landings.n(x)[, myrs])
    landings.n(res)[, nyrs] <-  lans %/% (quantSums(lans) + 1e-16)
    dans <- yearMeans(discards.n(x)[, myrs])
    discards.n(res)[, nyrs] <- dans %/% (quantSums(dans) + 1e-16)
    
    # AVERAGES for nsq years
    landings.wt(res)[, nyrs] <- yearMeans(landings.wt(x)[, myrs])
    discards.wt(res)[, nyrs] <- yearMeans(discards.wt(x)[, myrs])
    catch.sel(res)[, nyrs] <- yearMeans(catch.sel(x)[, myrs])
    price(res)[, nyrs] <- yearMeans(price(x)[, myrs])

    return(res)
  }
)
# }}}

# combine {{{

setMethod("combine", signature(x="FLFishery", y="FLFishery"),
  function(x, y, ..., check=FALSE) {

    args <- c(list(x, y), list(...))

    # FLF
    capacity(x) <- do.call(combine, lapply(args, capacity))
    effort(x) <- do.call(combine, lapply(args, effort))
    hperiod(x) <- do.call(combine, lapply(args, hperiod))
    vcost(x) <- do.call(combine, lapply(args, vcost))
    fcost(x) <- do.call(combine, lapply(args, fcost))
    orevenue(x) <- do.call(combine, lapply(args, orevenue))
 
    # x@crewshare@params <- do.call(combine, lapply(args, function(i)
    #   i@crewshare@params))

    # FLCs
    x@.Data <- do.call(Map, c(f=combine, lapply(args, slot, ".Data")))

    return(x)
  }
)
# }}}

# dim {{{
setMethod("dim", signature(x="FLCatch"),
  function(x) {
    return(dim(x@landings.n))
  }
) # }}}
