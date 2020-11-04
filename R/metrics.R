# metrics.R - DESC
# FLFishery/R/metrics.R

# Copyright European Union, 2015 
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under terms of the European Union Public Licence (EUPL) V.1.2.


# landings (FLC, FLF) {{{

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
  function(object) {
   
    # EXTRACT landings by FLCatch 
    las <- lapply(object, "landings")

    # CONVERT to flat list
    las <- Reduce("c", las)

    # GET names of FLCatch(es)
    cns <- unique(names(las))

    # ADD by catch name
    return(lapply(setNames(nm=cns), function(x)
      Reduce("+", las[names(las) == x])))
  }
)

# }}}

# discards (FLC, FLF) {{{

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
  function(object) {
   
    # EXTRACT discards by FLCatch 
    dis <- lapply(object, "discards")

    # CONVERT to flat list
    dis <- Reduce("c", dis)

    # GET names of FLCatch(es)
    cns <- unique(names(dis))

    # ADD by catch name
    return(lapply(setNames(nm=cns), function(x)
      Reduce("+", dis[names(dis) == x])))
  }
)

# }}}

# catch (FLC, FLF, FLFs) {{{

#' @rdname FLCatch
setMethod("catch", signature(object="FLCatch"),
  function(object) {
    return(landings(object) + discards(object))
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
  function(object) {
    return(mapply("+", landings(object), discards(object), SIMPLIFY=FALSE))
  }
) # }}}

# landings.n (FLF, FLs) {{{
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

# catch.wt (FLC, FLF, FLFs) {{{

#' @rdname FLCatch
setMethod("catch.wt", signature(object="FLCatch"),
  function(object) {

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
