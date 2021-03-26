# computation.R - DESC
# /computation.R

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

# landings.n (FLF, FLFs) {{{
setMethod("landings.n", signature(object="FLFishery"),
  function(object, pos=names(object)) {
    if(length(pos) == 1)
      lapply(object, "landings.n")[[pos]]
    else
      lapply(object, "landings.n")[pos]
  }
)

setMethod("landings.n", signature(object="FLFisheries"),
  function(object, pos=lapply(object, names)) {
    if(length(pos) == 1)
      FLQuants(mapply("landings.n", object, pos, SIMPLIFY=FALSE))
    else
      mapply("landings.n", object, pos, SIMPLIFY=FALSE)
  }
) # }}}

# discards.n (FLF, FLFs) {{{
setMethod("discards.n", signature(object="FLFishery"),
  function(object, pos=names(object)) {
    if(length(pos) == 1)
      lapply(object, "discards.n")[[pos]]
    else
      lapply(object, "discards.n")[pos]
  }
)

setMethod("discards.n", signature(object="FLFisheries"),
  function(object, pos=lapply(object, names)) {
    if(length(pos) == 1)
      FLQuants(mapply("discards.n", object, pos, SIMPLIFY=FALSE))
    else
      mapply("discards.n", object, pos, SIMPLIFY=FALSE)
  }
) # }}}

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
    return(lapply(object, "catch.n")[pos])
  }
)

#' @rdname FLFisheries
setMethod("catch.n", signature(object="FLFisheries"),
  function(object, pos=lapply(object, names)) {

    if(length(pos) == 1) {
      FLQuants(mapply("catch.n", object, pos, SIMPLIFY=FALSE))
    } else {

      can <- lapply(object, catch.n)
      
      cans <- Reduce(c, can)

      res <- lapply(setNames(nm=unique(names(cans))),
        function(x) Reduce("%++%", cans[x]))

      return(res)
    }
  }
) # }}}

# landings.wt (FLC, FLF, FLFs) {{{

#' @rdname FLCatch
setMethod("landings.wt", signature(object="FLCatch"),
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
  function(object, pos=lapply(object, names)) {
    if(length(pos) == 1)
      FLQuants(mapply("landings.wt", object, pos, SIMPLIFY=FALSE))
    else
      mapply("landings.wt", object, pos, SIMPLIFY=FALSE)
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
  function(object, pos=lapply(object, names)) {
    if(length(pos) == 1)
      FLQuants(mapply("catch.wt", object, pos, SIMPLIFY=FALSE))
    else
      mapply("catch.wt", object, pos, SIMPLIFY=FALSE)
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
  function(object, pos=lapply(object, names)) {
    if(length(pos) == 1)
      FLQuants(mapply("catch.wt", object, pos, SIMPLIFY=FALSE))
    else
      mapply("catch.wt", object, pos, SIMPLIFY=FALSE)
  }
) # }}}

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
