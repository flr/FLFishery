# computation.R - DESC
# /computation.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

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
    return(Reduce("%+%", lapply(object, "landings")[catch]))
  }
) # }}}

# discards (FLC, FLF) {{{
#' @rdname FLCatch
setMethod("discards", signature(object="FLCatch"),
  function(object) {
    return(quantSums(discards.n(object) * discards.wt(object)))
  }
)

#' @rdname FLFishery
setMethod("discards", signature(object="FLFishery"),
  function(object, catch=names(object)) {
    return(Reduce("%+%", lapply(object, "discards")[catch]))
  }
) # }}}

# catch (FLC, FLF, FLFs) {{{
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
    return(Reduce("%+%", lapply(object, "catch")))
  }
) 

#' @rdname FLFisheries
setMethod("catch", signature(object="FLFisheries"),
  function(object) {
    return(Reduce("%+%", lapply(object, "catch")))
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
    if(length(pos) == 1)
      lapply(object, "catch.n")[[pos]]
    else
      lapply(object, "catch.n")[pos]
  }
)

#' @rdname FLFisheries
setMethod("catch.n", signature(object="FLFisheries"),
  function(object, pos=lapply(object, names)) {
    if(length(pos) == 1)
      FLQuants(mapply("catch.n", object, pos, SIMPLIFY=FALSE))
    else
      mapply("catch.n", object, pos, SIMPLIFY=FALSE)
  }
) # }}}

# catch.wt (FLC, FLF, FLFs) {{{

#' @rdname FLCatch
setMethod("catch.wt", signature(object="FLCatch"),
  function(object) {
    return(landings.n(object) + discards.n(object))
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


