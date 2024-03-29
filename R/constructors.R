# constructors.R - DESC
# constructors.R

# Copyright European Union, 2015 
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) V.1.1.

# FLCatch() {{{
#' @rdname FLCatch
#' @aliases FLCatch,FLQuant-method
setMethod("FLCatch", signature(object="FLQuant"),
  function(object, ...) {
    
    # [...]
    args <- list(...)

    # empty object
    object[] <- as.numeric(NA)
    dmns <- dimnames(object)
    dmn <- dims(object)
    units(object) <- "NA"

    res <- new("FLCatch",
      landings.n=object,
      landings.wt=object,
      discards.n=object,
      discards.wt=object,
      catch.sel=object,
      price=object,
      catch.q=FLPar(alpha=1, beta=0),
      range=c(min=dmn$min, max=dmn$max,  plusgroup=dmn$max, minyear=dmn$minyear,
        maxyear=dmn$maxyear)
    )

    for(i in names(args))
      slot(res, i) <- args[[i]]

    return(res)
  }
)

#' @rdname FLCatch
#' @aliases FLCatch,missing-method
setMethod("FLCatch", signature(object="missing"),
  function(object, ...) {

    # [...]
    args <- list(...)

    # empty object
    idx <- unlist(lapply(args, is, 'FLQuant'))

    # No FLQuant passed
    if(sum(idx) == 0)
      return(FLCatch(object=FLQuant(quant='age'), ...))

    # else
    object <- args[[names(args)[idx][1]]]

    return(FLCatch(object, ...))
  }
) # }}}

# FLCatches() {{{
#' @rdname FLCatches
#' @aliases FLCatches,list-method
setMethod('FLCatches', signature(object='list'),
  function(object) {

    return(new('FLCatches', object))
  }
) 

#' @rdname FLCatches
#' @aliases FLCatches,missing-method
setMethod('FLCatches', signature(object='missing'),
  function(...) {

    args <- list(...)

    return(FLCatches(args))
  }
)

# }}}

# FLFishery() {{{

# list
#' @rdname FLFishery
#' @aliases FLFishery,list-method
setMethod("FLFishery", signature(object="list"),
  function(object, ...) {
    
    args <- list(...)

    cas <- new("FLCatches", object)

    # adjust years in cas if needed
    dmns <- lapply(cas, function(x)
      unlist(dims(x)[c('year', 'minyear', 'maxyear')]))
    dmns <- matrix(unlist(dmns), ncol=3, byrow=T, dimnames=list(names(dmns),
      c('year', 'minyear', 'maxyear')))

    # extend to maxyear
    if(any(dmns[,'maxyear'] < max(dmns[,'maxyear']))) {
      cas <- lapply(cas, function(x) window(x, end=max(dmns[,'maxyear'])))
    }

    # and to minyear
    if(any(dmns[,'minyear'] < min(dmns[,'minyear']))) {
      cas <- lapply(cas, function(x) window(x, start=min(dmns[,'minyear'])))
    }

    # search for FLQs in args
    idq <- unlist(lapply(args, is, 'FLQuant'))
    
    # if any ...
    if(sum(idq) > 0) {
      # ... select first one
      flq <- args[[names(idq)[idq][1]]]
      flq[] <- NA
      units(flq) <- ""
    } else {
      flq <- FLQuant(dimnames=c(list(quant='all'),
        dimnames(landings.n(cas[[1]]))[-1]), units="")
    }

    # hperiod
    hper <- FLQuant(c(0,1), dimnames=c(list(quant=c("start", "end")),
      dimnames(flq)[-1]), units="")

    # create new object
    res <- new("FLFishery", cas, capacity=FLQuant(1, dimnames=dimnames(flq)),
      effort=flq, hperiod=hper, vcost=flq, fcost=flq, orevenue=flq)

    # Fill desc with something, anything
    res@desc <- ""

    # fill slots provided
    for (i in names(args))
      slot(res, i) <- args[[i]]

    return(res)
  }
)

# FLCatch
#' @rdname FLFishery
#' @aliases FLFishery,FLCatch-method
setMethod("FLFishery", signature(object="FLCatch"),
  function(object, ...) {

    cas <- new("FLCatches", list(object))
    return(FLFishery(cas, ...))
  }
)

# missing
#' @rdname FLFishery
#' @aliases FLFishery,missing-method
setMethod("FLFishery", signature(object="missing"),
  function(object, ...) {

    args <- list(...)

    # NO inputs
    if(length(args) == 0)
      return(new("FLFishery"))

    # FIND FLCatch objects
    idc <- unlist(lapply(args, is, 'FLCatch'))

    # NONE
    if(sum(idc) == 0)
      stop("At least one 'FLCatch' or 'FLCatches' object must be provided")
 
    cas <- new("FLCatches", args[idc])
    args <- args[!idc]

    return(do.call("FLFishery", c(list(object=cas), args)))
    
  }
) # }}}

# FLFisheries() {{{

#' @rdname FLFisheries
#' @param desc Description
#' @aliases FLFisheries,list-method
setMethod('FLFisheries', signature(object='list'),
  function(object, desc=character(1)) {

    return(new('FLFisheries', object, desc=desc))
  }
) 

#' @rdname FLFisheries
#' @aliases FLFisheries,missing-method
setMethod('FLFisheries', signature(object='missing'),
  function(...) {

    args <- list(...)

    return(FLFisheries(args))
  }
) # }}}
