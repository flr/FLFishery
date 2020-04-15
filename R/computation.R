# computation.R - DESC
# /computation.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

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
      lapply(object, "catch.n")[[pos]]
    else
      lapply(object, "catch.n")[pos]
  }
)

setMethod("catch.n", signature(object="FLFisheries"),
  function(object, pos=lapply(object, names)) {
    if(length(pos) == 1)
      FLQuants(mapply("catch.n", object, pos, SIMPLIFY=FALSE))
    else
      mapply("catch.n", object, pos, SIMPLIFY=FALSE)
  }
)

# }}}

# landings.n (FLF, FLs)
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
)


