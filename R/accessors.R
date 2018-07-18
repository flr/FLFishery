# accessors.R - DESC
# accessors.R

# Copyright European Union, 2015 
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) V.1.1.

# FLCatch {{{

# landings.n
#' @rdname FLCatch
#' @aliases landings.n,FLCatch-method
setMethod("landings.n", signature(object="FLCatch"),
  function(object) {
    return(slot(object, "landings.n"))
  }
)
setReplaceMethod("landings.n", signature(object="FLCatch", value="FLQuant"),
  function(object, value) {
    slot(object, "landings.n") <- value
    return(object)
  }
)

#' @rdname FLCatch
#' @aliases landings.n<-,FLCatch,numeric-method
setReplaceMethod("landings.n", signature(object="FLCatch", value="numeric"),
  function(object, value) {
    slot(object, "landings.n")[] <- value
    return(object)
  }
)

# landings.wt
setMethod("landings.wt", signature(object="FLCatch"),
  function(object) {
    return(slot(object, "landings.wt"))
  }
)
setReplaceMethod("landings.wt", signature(object="FLCatch", value="FLQuant"),
  function(object, value) {
    slot(object, "landings.wt") <- value
    return(object)
  }
)

#' @rdname FLCatch
#' @aliases landings.n<-,FLCatch,numeric-method
setReplaceMethod("landings.wt", signature(object="FLCatch", value="numeric"),
  function(object, value) {
    slot(object, "landings.wt")[] <- value
    return(object)
  }
)

# discards.n
setMethod("discards.n", signature(object="FLCatch"),
  function(object) {
    return(slot(object, "discards.n"))
  }
)
setReplaceMethod("discards.n", signature(object="FLCatch", value="FLQuant"),
  function(object, value) {
    slot(object, "discards.n") <- value
    return(object)
  }
)

#' @rdname FLCatch
#' @aliases discards.n<-,FLCatch,numeric-method
setReplaceMethod("discards.n", signature(object="FLCatch", value="numeric"),
  function(object, value) {
    slot(object, "discards.n")[] <- value
    return(object)
  }
)

# discards.wt
setMethod("discards.wt", signature(object="FLCatch"),
  function(object) {
    return(slot(object, "discards.wt"))
  }
)
setReplaceMethod("discards.wt", signature(object="FLCatch", value="FLQuant"),
  function(object, value) {
    slot(object, "discards.wt") <- value
    return(object)
  }
)

#' @rdname FLCatch
#' @aliases discards.wt<-,FLCatch,numeric-method
setReplaceMethod("discards.wt", signature(object="FLCatch", value="numeric"),
  function(object, value) {
    slot(object, "discards.wt")[] <- value
    return(object)
  }
)

# catch.sel
setMethod("catch.sel", signature(object="FLCatch"),
  function(object) {
    return(slot(object, "catch.sel"))
  }
)
setReplaceMethod("catch.sel", signature(object="FLCatch", value="FLQuant"),
  function(object, value) {
    slot(object, "catch.sel") <- value
    return(object)
  }
)

#' @rdname FLCatch
#' @aliases catch.q<-,FLCatch,numeric-method
setReplaceMethod("catch.sel", signature(object="FLCatch", value="numeric"),
  function(object, value) {
    slot(object, "catch.sel")[] <- value
    return(object)
  }
)

# price
setMethod("price", signature(object="FLCatch"),
  function(object) {
    return(slot(object, "price"))
  }
)
setReplaceMethod("price", signature(object="FLCatch", value="FLQuant"),
  function(object, value) {
    slot(object, "price") <- value
    return(object)
  }
)

#' @rdname FLCatch
#' @aliases price<-,FLCatch,numeric-method
setReplaceMethod("price", signature(object="FLCatch", value="numeric"),
  function(object, value) {
    slot(object, "price")[] <- value
    return(object)
  }
)

# catch.q
setMethod("catch.q", signature(object="FLCatch"),
  function(object) {
    return(slot(object, "catch.q"))
  }
)
setReplaceMethod("catch.q", signature(object="FLCatch", value="FLPar"),
  function(object, value) {
    slot(object, "catch.q") <- value
    return(object)
  }
)
#' @rdname FLCatch
#' @aliases catch.q<-,FLCatch,numeric-method
setReplaceMethod("catch.q", signature(object="FLCatch", value="numeric"),
  function(object, value) {
    slot(object, "catch.q")[] <- value
    return(object)
  }
)
# }}}

# FLFishery {{{

# Direct accesors

# capacity
#' @rdname FLFishery
setMethod("capacity", signature(object="FLFishery"),
  function(object) {
    return(slot(object, "capacity"))
  }
)
#' @rdname FLFishery
setReplaceMethod("capacity", signature(object="FLFishery", value="FLQuant"),
  function(object, value) {
    slot(object, "capacity") <- value
    return(object)
  }
)
#' @rdname FLFishery
setReplaceMethod("capacity", signature(object="FLFishery", value="numeric"),
  function(object, value) {
    slot(object, "capacity")[] <- value
    return(object)
  }
)

# hperiod
#' @rdname FLFishery
setMethod("hperiod", signature(object="FLFishery"),
  function(object) {
    return(slot(object, "hperiod"))
  }
)
#' @rdname FLFishery
setReplaceMethod("hperiod", signature(object="FLFishery", value="FLQuant"),
  function(object, value) {
    slot(object, "hperiod") <- value
    return(object)
  }
)
#' @rdname FLFishery
setReplaceMethod("hperiod", signature(object="FLFishery", value="numeric"),
  function(object, value) {
    slot(object, "hperiod")[] <- value
    return(object)
  }
)

# Computation

# effort
#' @rdname FLFishery
#' @param compute Carry out formnula calculation (TRUE) or return full slot (FALSE).
setMethod("effort", signature(object="FLFishery"),
  function(object, compute=TRUE) {
    if(compute) {
      return(slot(object, "effort") %*% slot(object, "capacity"))
    } else {
      return(slot(object, "effort"))
    }
  }
)
#' @rdname FLFishery
setReplaceMethod("effort", signature(object="FLFishery", value="FLQuant"),
  function(object, value) {
    slot(object, "effort") <- value
  return(object)
  }
)

# vcost
#' @rdname FLFishery
setMethod("vcost", signature(object="FLFishery"),
  function(object, compute=TRUE) {
    if(compute) {
      return(slot(object, "vcost") %*% effort(object))
    } else {
      return(slot(object, "vcost"))
    }
  }
)
#' @rdname FLFishery
setReplaceMethod("vcost", signature(object="FLFishery", value="FLQuant"),
  function(object, value) {
    slot(object, "vcost") <- value
  return(object)
  }
)

# fcost
#' @rdname FLFishery
setMethod("fcost", signature(object="FLFishery"),
  function(object, compute=TRUE) {
    if(compute) {
      return(slot(object, "fcost") %*% capacity(object))
    } else {
      return(slot(object, "fcost"))
    }
  }
)
#' @rdname FLFishery
setReplaceMethod("fcost", signature(object="FLFishery", value="FLQuant"),
  function(object, value) {
    slot(object, "fcost") <- value
  return(object)
  }
)

# orevenue
#' @rdname FLFishery
setMethod("orevenue", signature(object="FLFishery"),
  function(object) {
    return(slot(object, "orevenue") * capacity(object))
  }
)
#' @rdname FLFishery
setReplaceMethod("orevenue", signature(object="FLFishery", value="FLQuant"),
  function(object, value) {
    slot(object, "orevenue") <- value
    return(object)
  }
)
#' @rdname FLFishery
setReplaceMethod("orevenue", signature(object="FLFishery", value="numeric"),
  function(object, value) {
    slot(object, "orevenue")[] <- value
    return(object)
  }
)



# crewshare
setMethod("crewshare", signature(object="FLFishery"),
  function(object, compute=TRUE) {
    if(compute)
      return(evalPredictModel(object, slot=object@crewshare))
    else
      return(object@crewshare)
  }
)
setReplaceMethod("crewshare", signature(object="FLFishery", value="predictModel"),
  function(object, value) {
       slot(object, "crewshare") <- value
    return(object)
  }
)
# }}}

# FLFisherycpp {{{

# Direct accesors

# capacity
#' @rdname FLFisherycpp
#' @param object Object to extract or operate on
setMethod("capacity", signature(object="FLFisherycpp"),
  function(object) {
    return(slot(object, "capacity"))
  }
)
#' @rdname FLFisherycpp
#' @param value Replacement value
setReplaceMethod("capacity", signature(object="FLFisherycpp", value="FLQuant"),
  function(object, value) {
    slot(object, "capacity") <- value
    return(object)
  }
)
#' @rdname FLFisherycpp
setReplaceMethod("capacity", signature(object="FLFisherycpp", value="numeric"),
  function(object, value) {
    slot(object, "capacity")[] <- value
    return(object)
  }
)

# hperiod
#' @rdname FLFisherycpp
setMethod("hperiod", signature(object="FLFisherycpp"),
  function(object) {
    return(slot(object, "hperiod"))
  }
)
#' @rdname FLFisherycpp
setReplaceMethod("hperiod", signature(object="FLFisherycpp", value="FLQuant"),
  function(object, value) {
    slot(object, "hperiod") <- value
    return(object)
  }
)
#' @rdname FLFisherycpp
setReplaceMethod("hperiod", signature(object="FLFisherycpp", value="numeric"),
  function(object, value) {
    slot(object, "hperiod")[] <- value
    return(object)
  }
)

# orevenue
#' @rdname FLFisherycpp
setMethod("orevenue", signature(object="FLFisherycpp"),
  function(object) {
    return(slot(object, "orevenue"))
  }
)
#' @rdname FLFisherycpp
setReplaceMethod("orevenue", signature(object="FLFisherycpp", value="FLQuant"),
  function(object, value) {
    slot(object, "orevenue") <- value
    return(object)
  }
)
#' @rdname FLFisherycpp
setReplaceMethod("orevenue", signature(object="FLFisherycpp", value="numeric"),
  function(object, value) {
    slot(object, "orevenue")[] <- value
    return(object)
  }
)

# effort
#' @rdname FLFisherycpp
setMethod("effort", signature(object="FLFisherycpp"),
  function(object) {
    return(slot(object, "effort"))
  }
)
#' @rdname FLFisherycpp
setReplaceMethod("effort", signature(object="FLFisherycpp", value="FLQuant"),
  function(object, value) {
    slot(object, "effort") <- value
    return(object)
  }
)
#' @rdname FLFisherycpp
setReplaceMethod("effort", signature(object="FLFisherycpp", value="numeric"),
  function(object, value) {
    slot(object, "effort")[] <- value
    return(object)
  }
)

# vcost
#' @rdname FLFisherycpp
setMethod("vcost", signature(object="FLFisherycpp"),
  function(object) {
    return(slot(object, "vcost"))
  }
)
#' @rdname FLFisherycpp
setReplaceMethod("vcost", signature(object="FLFisherycpp", value="FLQuant"),
  function(object, value) {
    slot(object, "vcost") <- value
    return(object)
  }
)
#' @rdname FLFisherycpp
setReplaceMethod("vcost", signature(object="FLFisherycpp", value="numeric"),
  function(object, value) {
    slot(object, "vcost")[] <- value
    return(object)
  }
)

# fcost
#' @rdname FLFisherycpp
setMethod("fcost", signature(object="FLFisherycpp"),
  function(object) {
    return(slot(object, "fcost"))
  }
)
#' @rdname FLFisherycpp
setReplaceMethod("fcost", signature(object="FLFisherycpp", value="FLQuant"),
  function(object, value) {
    slot(object, "fcost") <- value
    return(object)
  }
)
#' @rdname FLFisherycpp
setReplaceMethod("fcost", signature(object="FLFisherycpp", value="numeric"),
  function(object, value) {
    slot(object, "fcost")[] <- value
    return(object)
  }
)

# crewshare
#' @rdname FLFisherycpp
setMethod("crewshare", signature(object="FLFisherycpp"),
  function(object) {
    return(slot(object, "crewshare"))
  }
)
#' @rdname FLFisherycpp
setReplaceMethod("crewshare", signature(object="FLFisherycpp", value="FLQuant"),
  function(object, value) {
    slot(object, "crewshare") <- value
    return(object)
  }
)
#' @rdname FLFisherycpp
setReplaceMethod("crewshare", signature(object="FLFisherycpp", value="numeric"),
  function(object, value) {
    slot(object, "crewshare")[] <- value
    return(object)
  }
)
# }}}

# FLFisheries {{{

#' @rdname FLFisheries
#' @aliases landings,FLFisheries-method
setMethod('landings', signature(object='FLFisheries'),
  function(object) {
    lapply(object, landings)
  }
) 

#' @rdname FLFisheries
#' @aliases discards,FLFisheries-method
setMethod('discards', signature(object='FLFisheries'),
  function(object) {
    lapply(object, discards)
  }
)

#' @rdname FLFisheries
#' @aliases catch,FLFisheries-method
setMethod('catch', signature(object='FLFisheries'),
  function(object) {
    lapply(object, catch)
  }
)

#' @rdname FLFisheries
#' @aliases lrevenue,FLFisheries-method
setMethod('lrevenue', signature(object='FLFisheries'),
  function(object) {
    lapply(object, lrevenue)
  }
)

#' @rdname FLFisheries
#' @aliases revenue,FLFisheries-method
setMethod('revenue', signature(object='FLFisheries'),
  function(object) {
    lapply(object, revenue)
  }
)

#' @rdname FLFisheries
#' @aliases cost,FLFisheries-method
setMethod('cost', signature(object='FLFisheries'),
  function(object) {
    lapply(object, cost)
  }
)

#' @rdname FLFisheries
#' @aliases ccost,FLFisheries-method
setMethod('ccost', signature(object='FLFisheries'),
  function(object) {
    lapply(object, ccost)
  }
)

#' @rdname FLFisheries
#' @aliases vcost,FLFisheries-method
setMethod('vcost', signature(object='FLFisheries'),
  function(object) {
    lapply(object, vcost)
  }
)

#' @rdname FLFisheries
#' @aliases fcost,FLFisheries-method
setMethod('fcost', signature(object='FLFisheries'),
  function(object) {
    lapply(object, fcost)
  }
)

#' @rdname FLFisheries
#' @aliases profit,FLFisheries-method
setMethod('profit', signature(object='FLFisheries'),
  function(object) {
    lapply(object, profit)
  }
)

# }}}
