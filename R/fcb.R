# fcb.R - DESC
# /home/mosquia/Projects/FLR/pkgs/mine/FLFishery/R/fcb.R

# Copyright (c) WUR, 2022.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# guessfcb {{{

#' Generate an FCB matrix from FLBiols and FLFisheries
#'
#' Tries to generate FCB matrix based on names.
#' Internal function. Ignore.
#' @param biols The FLBiols.
#' @param fisheries The FLFisheries.

guessfcb <- function(biols, fisheries) {

  # GET names
  nmf <- names(fisheries)
  nmc <- lapply(fisheries, names)
  nmb <- names(biols)

  fc <- do.call(rbind, lapply(names(nmc), function(x) unlist(cbind(x, nmc[[x]]))))
  b <- nmb[match(fc[,2], nmb)]

  fcb <- cbind(fc[!is.na(b),, drop=FALSE], b[!is.na(b)])
  colnames(fcb) <- c("f", "c", "b")
  rownames(fcb) <- seq(nrow(fcb))

  return(fcb)
}

# fcb2int(fcb, biols, fisheries)

#' fcb2int function
#'
#' Internal function not for public consumption
#' @param fcb The FCB matrix
#' @param biols The biols
#' @param fisheries The fisheries

fcb2int <- function(fcb, biols, fisheries) {
  
  # GET names
  nmf <- names(fisheries)
  nmc <- lapply(fisheries, names)
  nmb <- names(biols)

  fcbint <- array(NA, dim=dim(fcb), dimnames=dimnames(fcb))

  fcbint[,"f"] <- as.integer(match(fcb[,"f"], nmf))
  fcbint[,"b"] <- as.integer(match(fcb[,"b"], nmb))

  for(i in names(nmc))
    fcbint[fcb[,"f"] == i, "c"] <- match(fcb[fcb[,"f"] == i, "c"], nmc[[i]])

  return(fcbint)
} # }}}

setGeneric("FCB", function(object, ...) standardGeneric("FCB"))

# FCB {{{

#' Matrix of Fishery - Catch - Biol relationships
#'
#' The relationships between a fishery, its catch elements and the biological
#' populations that catch is taken from, is part of the `fwdControl` class. When
#' a single `FLBiol` and `FLFishery`, or just an `FLStock`, are projected
#' forward, this structure is constructed on the fly. Even when multiple biols
#' and fisheries are used, a guess is made based on name matching.
#'
#' But when a more complex structure is employed, the `FCB` matrix can be given
#' to the `fwdControl()` constructor method. Elements in this matrix can be
#' names or numbers.
#' @param object A list or vector containing a row for the matrix.
#' @param ... Further vectors to be merged into the matrix.
#' @rdname FCB
#' @examples
#' # 1 fishery with catches from 2 biols
#' FCB(c(f=1, c=1, b=2), c(f=1, c=2, b=2))
#' # 2 fisheries with caches from 3 biols
#' FCB(c(f=1, c=1, b=1), c(f=1, c=2, b=2),
#'   c(f=2, c=1, b=2), c(f=2, c=2, b=2),
#'   c(f=2, c=3, b=3))

setMethod("FCB", signature(object="ANY"),
  function(object, ...) {
    
    args <- c(list(object), list(...))

    # OUTLIST list
    if(length(args) == 1 & is(args[[1]], "list")){
      args <- args[[1]]
      x <- t(do.call(rbind, args))
    
    # USE matrix
    } else if(length(args) == 1 & is(args[[1]], "matrix"))
      x <- args[[1]]
  
    # CREATE matrix
    else
      x <- do.call(rbind, args)

    # CHECK dims
    if(dim(x)[2] != 3)
      stop("FCB matrix can only have 3 columns")

    dimnames(x) <- list(seq(1, dim(x)[1]), c("F", "C", "B"))

  return(x)
  }
) 

#' @rdname FCB

setMethod("FCB", signature(object="missing"),
  function(...) {
    object <- list(...)
    return(FCB(object))
  }
) 

#' @rdname FCB
#' @details If `FLBiols` and `FLFisheries` objects are passed to `FCB`, a guess is made
#' at constructing the matrix based on the names of the various list elements.
#' @param fisheries An `FLFisheries` object to extract fishery and catch names from.

setMethod("FCB", signature(object="FLBiols"),
  function(object, fisheries) {
    return(FCB(fcb2int(guessfcb(object, fisheries), object, fisheries)))
  }
)

#' @rdname FCB
#' @param biols An `FLBiols` object to match with `FLCatch` elements in `object`.

setMethod("FCB", signature(object="FLFisheries"),
  function(object, biols) {
    return(FCB(fcb2int(guessfcb(biols, object), biols, object)))
  }
)
# }}}
