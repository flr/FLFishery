# accessors.R - DESC
# accessors.R

# Copyright European Union, 2015 
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) V.1.1.

# $
setMethod("$", signature(x="FLQuant"),           
  function(x, name) {
    return(x[name,])
  }
)

# FLCatch {{{

# landings.n
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

# catch.sel {{{
setMethod('catch.sel', signature('FLCatch'),
  function(object, compute=TRUE) {
    if(compute)
      return(evalPredictModel(object, slot='catch.sel'))
    else
      return(object@catch.sel)
  }
)

# catch.sel<- predictModel
setReplaceMethod('catch.sel', signature(object='FLCatch', value='predictModel'),
  function(object, value) {
    object@catch.sel <- value
    return(object)
  }
)

# catch.sel<- FLQuant: change to catch.sel@.Data['catch.sel']
setReplaceMethod('catch.sel', signature(object='FLCatch', value='FLQuant'),
  function(object, value) {
    object@catch.sel@.Data <- FLQuants(catch.sel=value)
    return(object)
  }
)

# catch.sel<- FLQuants: assign to @.Data
setReplaceMethod('catch.sel', signature(object='FLCatch', value='FLQuants'),
  function(object, value) {
    object@catch.sel@.Data <- value
    return(object)
  }
)

# catch.sel<- formula:
setReplaceMethod('catch.sel', signature(object='FLCatch', value='formula'),
  function(object, ..., value) {
    object@catch.sel@model <- value
    return(object)
  }
)

# catch.sel<- params:
setReplaceMethod('catch.sel', signature(object='FLCatch', value='FLPar'),
  function(object, value) {
    object@catch.sel@params <- value
    return(object)
  }
) 

# catch.sel<- list:
setReplaceMethod('catch.sel', signature(object='FLCatch', value='list'),
  function(object, value) {
    
    # FLQuants
    idx <- unlist(lapply(value, is, 'FLQuants'))
    if(sum(idx) > 1)
      stop("More than one element in the list is of class 'FLQuants'")
    
    object@catch.sel@.Data <- value[idx][[1]]
    object@catch.sel@names <- names(value[idx][[1]])

    # params & modes
    idx <- !idx

    if(sum(idx) > 0)
    for(i in names(value[idx]))
      slot(object@catch.sel, i) <- value[[i]]

    return(object)
  }
) # }}}

# price {{{
setMethod('price', signature('FLCatch'),
  function(object, compute=TRUE) {
    if(compute)
      return(evalPredictModel(object, slot='price'))
    else
      return(object@price)
  }
)

# price<- predictModel
setReplaceMethod('price', signature(object='FLCatch', value='predictModel'),
  function(object, value) {
    object@price <- value
    return(object)
  }
)

# price<- FLQuant: change to price@.Data['price']
setReplaceMethod('price', signature(object='FLCatch', value='FLQuant'),
  function(object, value) {
    object@price@.Data <- FLQuants(price=value)
    return(object)
  }
)

# price<- FLQuants: assign to @.Data
setReplaceMethod('price', signature(object='FLCatch', value='FLQuants'),
  function(object, value) {
    object@price@.Data <- value
    return(object)
  }
)

# price<- formula:
setReplaceMethod('price', signature(object='FLCatch', value='formula'),
  function(object, ..., value) {
    object@price@model <- value
    return(object)
  }
)

# price<- params:
setReplaceMethod('price', signature(object='FLCatch', value='FLPar'),
  function(object, value) {
    object@price@params <- value
    return(object)
  }
) 

# price<- list:
setReplaceMethod('price', signature(object='FLCatch', value='list'),
  function(object, value) {
    
    # FLQuants
    idx <- unlist(lapply(value, is, 'FLQuants'))
    if(sum(idx) > 1)
      stop("More than one element in the list is of class 'FLQuants'")
    
    object@price@.Data <- value[idx][[1]]
    object@price@names <- names(value[idx][[1]])

    # params & modes
    idx <- !idx

    if(sum(idx) > 0)
    for(i in names(value[idx]))
      slot(object@price, i) <- value[[i]]

    return(object)
  }
) # }}}

# }}}

# FLFishery {{{

# Direct accesors

# capacity
setMethod("capacity", signature(object="FLFishery"),
  function(object) {
    return(slot(object, "capacity"))
  }
)
setReplaceMethod("capacity", signature(object="FLFishery", value="FLQuant"),
  function(object, value) {
    slot(object, "capacity") <- value
    return(object)
  }
)

# hperiod
setMethod("hperiod", signature(object="FLFishery"),
  function(object) {
    return(slot(object, "hperiod"))
  }
)
setReplaceMethod("hperiod", signature(object="FLFishery", value="FLQuant"),
  function(object, value) {
    slot(object, "hperiod") <- value
    return(object)
  }
)

# orevenue
setMethod("orevenue", signature(object="FLFishery"),
  function(object) {
    return(slot(object, "orevenue"))
  }
)
setReplaceMethod("orevenue", signature(object="FLFishery", value="FLQuant"),
  function(object, value) {
    slot(object, "orevenue") <- value
    return(object)
  }
)

# Computation

# effort
setMethod("effort", signature(object="FLFishery"),
  function(object, compute=TRUE) {
    if(compute) {
      return(slot(object, "effort") %*% slot(object, "capacity"))
    } else {
      return(slot(object, "effort"))
    }
  }
)
setReplaceMethod("effort", signature(object="FLFishery", value="FLQuant"),
  function(object, value) {
    slot(object, "effort") <- value
  return(object)
  }
)

# vcost
setMethod("vcost", signature(object="FLFishery"),
  function(object, compute=TRUE) {
    if(compute) {
      return(slot(object, "vcost") %*% effort(object))
    } else {
      return(slot(object, "vcost"))
    }
  }
)
setReplaceMethod("vcost", signature(object="FLFishery", value="FLQuant"),
  function(object, value) {
    slot(object, "vcost") <- value
  return(object)
  }
)

# fcost
setMethod("fcost", signature(object="FLFishery"),
  function(object, compute=TRUE) {
    if(compute) {
      return(slot(object, "fcost") %*% capacity(object))
    } else {
      return(slot(object, "fcost"))
    }
  }
)
setReplaceMethod("fcost", signature(object="FLFishery", value="FLQuant"),
  function(object, value) {
    slot(object, "fcost") <- value
  return(object)
  }
)

# crewshare
setMethod("crewshare", signature(object="FLFishery"),
  function(object) {
    return(slot(object, "crewshare"))
  }
)
setReplaceMethod("crewshare", signature(object="FLFishery", value="predictModel"),
  function(object, value) {
       slot(object, "crewshare") <- value
    return(object)
  }
)

# }}}

# FLFisheries {{{
setMethod('landings.n', signature(object='FLFisheries'),
  function(object) {
    lapply(object, landings.n)
  }
) # }}}
