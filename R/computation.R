# computation.R - Compute quantities aggregated by catch/biol or fishery
# FLFishery/R/computation.R

# Copyright European Union, 2015 
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under terms of the European Union Public Licence (EUPL) V.1.2.

# landings (FLC, FLF, FLFs) {{{

#' @rdname FLCatch
setMethod("landings", signature(object="FLCatch"),
  function(object) {
    return(quantSums(landings.wt(object) * landings.n(object)))
  }
)

#' @rdname FLFishery
setMethod("landings", signature(object="FLFishery"),
  function(object, catch=seq(object)) {
    return(lapply(object, "landings")[catch])
  }
) 

#' @rdname FLFisheries
#' @param by Dimension to aggregate by, "fishery" or "catch".
setMethod("landings", signature(object="FLFisheries"),
  function(object, by=c("fishery", "catch"), sum=TRUE) {
    .parseMetrics(object, metric="landings", by=by[1], sum=sum)
  }
)
# }}}

# discards (FLC, FLF, FLFs) {{{

#' @rdname FLCatch
setMethod("discards", signature(object="FLCatch"),
  function(object) {
    return(quantSums(discards.wt(object) * discards.n(object)))
  }
)

#' @rdname FLFishery
setMethod("discards", signature(object="FLFishery"),
  function(object, catch=seq(object)) {
    return(lapply(object, "discards")[catch])
  }
) 

#' @rdname FLFisheries
setMethod("discards", signature(object="FLFisheries"),
  function(object, by=c("fishery", "catch"), sum=TRUE) {
    .parseMetrics(object, metric="discards", by=by[1], sum=sum)
  }
)
# }}}

# catch (FLC, FLF, FLFs) {{{

#' @rdname FLCatch
setMethod("catch", signature(object="FLCatch"),
  function(object) {
    return(quantSums(catch.wt(object) * catch.n(object)))
  }
)

#' @rdname FLFishery
setMethod("catch", signature(object="FLFishery"),
  function(object, catch=seq(object)) {
    return(lapply(object, "catch")[catch])
  }
) 

#' @rdname FLFisheries
setMethod("catch", signature(object="FLFisheries"),
  function(object, by=c("fishery", "catch"), sum=TRUE) {
    .parseMetrics(object, metric="catch", by=by[1], sum=sum)
  }
) # }}}

# accessor functions {{{

.fsaccessor <- function(slot, object, fishery=names(object),
  pos=unique(unlist(lapply(object, names))), reduce=TRUE) {
  
  # PARSE pos/fishery arguments
  fnms <- names(object)
  pnms <- unique(unlist(lapply(object, names)))

  if(all(fishery %in% pnms)) {
    pos <- fishery
    fishery <- fnms
  }

  # EXTRACT single pos
  if(length(pos) == 1) {
    res <- FLQuants(Map(slot, object[fishery], pos))
  # TODO: MIXMATCH in areas
    if(reduce)
     res <- Reduce("%++%", res)
  } else {

  # OR multiple
  can <- lapply(object[fishery], slot, pos=pos)
    res <- FLQuants(lapply(setNames(nm=pos), function(x)
    Reduce("%++%", lapply(can, function(y) y[[x]]))))
  }
  return(res)
}

# }}}

# .parseMetrics {{{
.parseMetrics <- function(object, metric, by, sum) {

    # EXTRACT metric by FLCatch
    res <- lapply(object, metric)

    # fishery
    if(by == "fishery" & !sum) {
      return(res)
    } else if (by == "fishery" & sum) {
      return(FLQuants(lapply(res, Reduce, f="+")))
    # catch
    } else if(by == "catch" & sum) {

        res <- Reduce("c", res)

        # GET names of FLCatch(es)
        nms <- unique(names(res))

        # ADD by catch name
        return(FLQuants(lapply(setNames(nm=nms), function(x)
          Reduce("+", res[names(res) == x]))))
    } else if (by == "catch" & !sum) {
    
      nms <- unique(unlist(lapply(res, names)))

      return(lapply(setNames(nm=nms), function(x)
        FLQuants(lapply(res, function(y) y[[x]]))))

    } else {
      stop("if given, 'by' must be one of 'fishery' or 'catch'")
    }
} # }}}

# landings.n (FLC, FLF, FLFs) {{{

#' @rdname FLCatch
setMethod("landings.n", signature(object="FLCatch"),
  function(object) {
    return(object@landings.n)
  }
)

#' @rdname FLFishery
setMethod("landings.n", signature(object="FLFishery"),
  function(object, pos=names(object)) {
    if(length(pos) == 1)
      return(lapply(object, "landings.n")[[pos]])
    else
      return(lapply(object, "landings.n")[pos])
  }
)

#' @rdname FLFisheries
setMethod("landings.n", signature(object="FLFisheries"),
  function(object, fishery=names(object),
    pos=unique(unlist(lapply(object, names))), reduce=TRUE) {

    .fsaccessor('landings.n', object, fishery=fishery, pos=pos, reduce=reduce)
  }
)
# }}}

# discards.n (FLC, FLF, FLFs) {{{

#' @rdname FLCatch
setMethod("discards.n", signature(object="FLCatch"),
  function(object) {
    return(object@discards.n)
  }
)

#' @rdname FLFishery
setMethod("discards.n", signature(object="FLFishery"),
  function(object, pos=names(object)) {
    if(length(pos) == 1)
      return(lapply(object, "discards.n")[[pos]])
    else
      return(lapply(object, "discards.n")[pos])
  }
)

#' @rdname FLFisheries
setMethod("discards.n", signature(object="FLFisheries"),
  function(object, fishery=names(object),
    pos=unique(unlist(lapply(object, names))), reduce=TRUE) {

    .fsaccessor('discards.n', object, fishery=fishery, pos=pos, reduce=reduce)
  }
)
# }}}

# catch.n (FLC, FLF, FLFs) {{{

#' @rdname FLCatch
setMethod("catch.n", signature(object="FLCatch"),
  function(object) {
    return(landings.n(object) + discards.n(object))
  }
)

#' @rdname FLFishery
setMethod("catch.n", signature(object="FLFishery"),
  function(object, pos=names(object)) {
    if(length(pos) == 1)
      return(lapply(object, "catch.n")[[pos]])
    else
      return(lapply(object, "catch.n")[pos])
  }
)

#' @rdname FLFisheries
setMethod("catch.n", signature(object="FLFisheries"),
  function(object, fishery=names(object),
    pos=unique(unlist(lapply(object, names))), reduce=TRUE) {

    .fsaccessor('catch.n', object, fishery=fishery, pos=pos, reduce=reduce)
  }
)
# }}}

# landings.wt (FLC, FLF, FLFs) {{{

#' @rdname FLFishery
setMethod("landings.wt", signature(object="FLFishery"),
  function(object, pos=names(object)) {
    if(length(pos) == 1)
      lapply(object, "landings.wt")[[pos]]
    else
      lapply(object, "landings.wt")[pos]
  }
)

#' @rdname FLFisheries
setMethod("landings.wt", signature(object="FLFisheries"),
  function(object, fishery=names(object),
    pos=unique(unlist(lapply(object, names))), reduce=TRUE) {

    .fsaccessor('landings.wt', object, fishery=fishery, pos=pos, reduce=reduce)
  }
)
setMethod("landings.wt", signature(object="FLFisheries"),
  function(object, pos=lapply(object, names)) {
    if(length(pos) == 1)
      FLQuants(Map("landings.wt", object, pos))
    else
      Map("landings.wt", object, pos)
  }
) # }}}

# catch.wt (FLC, FLF, FLFs) {{{

#' @rdname FLCatch
setMethod("catch.wt", signature(object="FLCatch"),
  function(object) {

  landings.wt(object) <- propagate(landings.wt(object),
    dim(landings.n(object))[6])
  discards.wt(object) <- propagate(discards.wt(object),
    dim(discards.n(object))[6])

  # DEAL with NAs
  landings.wt(object)[is.na(landings.n(object))] <- 0
  landings.n(object)[is.na(landings.n(object))] <- 1

  discards.wt(object)[is.na(discards.n(object))] <- 0
  discards.n(object)[is.na(discards.n(object))] <- 1

  # WEIGHTED average (+ 1e-16)
  return(((landings.wt(object) * (landings.n(object) + 1e-16)) +
    (discards.wt(object) * (discards.n(object) + 1e-16))) /
      (landings.n(object) + discards.n(object) + 1e-16))
  }
)

#' @rdname FLFishery
setMethod("catch.wt", signature(object="FLFishery"),
  function(object, pos=names(object)) {
    if(length(pos) == 1)
      lapply(object, "catch.wt")[[pos]]
    else
      lapply(object, "catch.wt")[pos]
  }
)

#' @rdname FLFisheries
setMethod("catch.wt", signature(object="FLFisheries"),
  function(object, pos=unique(unlist(lapply(object, names))), reduce=TRUE) {

    if(length(pos) == 1) {

      res <- FLQuants(Map("catch.wt", object, pos))
    
      # REDUCE across fisheries

      if(reduce) {
        can <- catch.n(object, pos=pos, reduce=FALSE)
        res <- Reduce("+", can * res) / Reduce("+", can)
      }

    } else {
 
      # list(FLQuants), fishery(catch)

      res <- lapply(object, function(x) lapply(x, catch.wt)[pos])

      if(reduce) {
        can <- lapply(object, function(x) lapply(x, catch.n)[pos])
        res <- Map(function(x, y) (x * y) / y, x = res, y = can)


      }

      # reduce, combine as weighted mean by stock across fisheries

      res <- FLQuants(lapply(setNames(nm=pos), function(x) {
        Reduce("%++%", 
               Map(function(x, y) (x * y) / y, x=res, y=can))
    }))
    }

    return(res)
  }
) # }}}
