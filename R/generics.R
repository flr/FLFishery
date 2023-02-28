# generics.R - DESC
# FLFishery/R/generics.R

# Copyright European Union, 2015-2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) V.1.1.

# FLCatch() {{{

#' FLCatch constructor
#'
#' Make an FLCatch object.
#'
#' Make an FLCatch object.
#'
#' @param object Either an FLQuant (to determine the size of the FLQuant slots) or missing
#' @param ... Other things
#'
#' @return An FLCatch object
#'
#' @name FLCatch
#' @rdname FLCatch
#' @aliases FLCatch-method
#'
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords classes
setGeneric("FLCatch", function(object, ...) standardGeneric("FLCatch")) # }}}

# FLCatches() {{{

#' FLCatches constructor
#'
#' Make an FLCatches object.
#'
#' Make an FLCatches object.
#'
#' @param object Either a list of FLCatch objects or missing
#' @param ... Other things
#'
#' @return An FLCatches object
#'
#' @name FLCatches
#' @rdname FLCatches
#' @aliases FLCatches-methods
#'
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords classes
setGeneric("FLCatches", function(object, ...) standardGeneric("FLCatches")) # }}}

# FLFishery() {{{

#' @rdname FLFishery
#' @aliases FLFishery FLFishery-method
#' @param object An object
setGeneric("FLFishery", function(object, ...) standardGeneric("FLFishery")) # }}}

# FLFisheries {{{

#' FLFisheries constructor
#'
#' Make an FLFisheries object.
#'
#' Make an FLFisheries object.
#'
#' @param object Either a list of FLFishery objects or missing
#' @param ... Other things
#'
#' @return An FLFisheries object
#'
#' @name FLFisheries
#' @rdname FLFisheries
#' @aliases FLFisheries-methods
#'
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords classes
setGeneric("FLFisheries", function(object, ...) standardGeneric("FLFisheries")) # }}}

# ACCESSORS {{{

#' @rdname FLFishery
#' @aliases capacity capacity-method
setGeneric("capacity", function(object, ...) standardGeneric("capacity"))

#' @rdname FLFishery
#' @param value Replacement value
#' @param ... Other things
#' @aliases capacity<- capacity<--method
setGeneric("capacity<-", function(object, ..., value) standardGeneric("capacity<-"))

#' @rdname FLFishery
#' @aliases crewshare crewshare-method
setGeneric("crewshare", function(object, ...) standardGeneric("crewshare"))

#' @rdname FLFishery
#' @aliases crewshare<- crewshare<--method
setGeneric("crewshare<-", function(object, ..., value) standardGeneric("crewshare<-"))

#' @rdname FLFishery
#' @aliases hperiod hperiod-method
setGeneric("hperiod", function(object, ...) standardGeneric("hperiod"))

#' @rdname FLFishery
#' @aliases hperiod<- hperiod<--method
setGeneric("hperiod<-", function(object, ..., value) standardGeneric("hperiod<-"))

#' @rdname FLFishery
#' @aliases orevenue orevenue-method
setGeneric("orevenue", function(object, ...) standardGeneric("orevenue"))

#' @rdname FLFishery
#' @aliases orevenue<- orevenue<--method
setGeneric("orevenue<-", function(object, ..., value) standardGeneric("orevenue<-"))
# }}}

# METHODS {{{

#' @rdname FLCatch
#' @param value Replacement value
#' @aliases catch.sel<- catch.sel<--method
setGeneric("catch.sel<-", function(object, ..., value) standardGeneric("catch.sel<-"))

#' @rdname FLCatch
#' @aliases lrevenue lrevenue-method
setGeneric("lrevenue", function(object, ...) standardGeneric("lrevenue"))

#' @rdname harvest
#' @aliases harvests harvests-method
#' @param ... Other things
setGeneric("harvests", function(object, catches, ...) standardGeneric("harvests"))
# }}}
