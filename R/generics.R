# generics.R - DESC
# FLFishery/R/generics.R

# Copyright European Union, 2015 
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) V.1.1.

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
#' @genericMethods
#' 
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords classes
setGeneric("FLCatch", function(object, ...) standardGeneric("FLCatch"))

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
#' @genericMethods
#' 
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords classes
setGeneric("FLCatches", function(object, ...) standardGeneric("FLCatches"))

setGeneric("FLFishery", function(object, ...) standardGeneric("FLFishery"))

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
#' @genericMethods
#' 
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords classes
setGeneric("FLFisheries", function(object, ...) standardGeneric("FLFisheries"))

# ACCESSORS

setGeneric("capacity", function(object, ...) standardGeneric("capacity"))
setGeneric("capacity<-", function(object, ..., value) standardGeneric("capacity<-"))

#' @rdname FLFishery
#' @aliases crewshare crewshare-method
setGeneric("crewshare", function(object, ...) standardGeneric("crewshare"))
#' @rdname FLFishery
#' @param value Replacement value
#' @param ... Other things
#' @param object An object
#' @aliases crewshare<- crewshare<--method
setGeneric("crewshare<-", function(object, ..., value) standardGeneric("crewshare<-"))

setGeneric("fcost", function(object, ...) standardGeneric("fcost"))
setGeneric("fcost<-", function(object, ..., value) standardGeneric("fcost<-"))

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

setGeneric("price", function(object, ...) standardGeneric("price"))
setGeneric("price<-", function(object, ..., value) standardGeneric("price<-"))

#' @rdname FLFishery
#' @aliases vcost vcost-method
setGeneric("vcost", function(object, ...) standardGeneric("vcost"))
#' @rdname FLFishery
#' @aliases vcost<- vcost<--method
setGeneric("vcost<-", function(object, ..., value) standardGeneric("vcost<-"))

# METHODS

#' @rdname FLCatch
#' @aliases catch.sel catch.sel-method
setGeneric("catch.sel", function(object, ...) standardGeneric("catch.sel"))

#' @rdname FLCatch
#' @param value Replacement value
#' @aliases catch.sel<- catch.sel<--method
setGeneric("catch.sel<-", function(object, ..., value) standardGeneric("catch.sel<-"))

#' @rdname FLCatch
#' @aliases discards.ratio discards.ratio-method
setGeneric("discards.ratio", function(object, ...) standardGeneric("discards.ratio"))

setGeneric("cost", function(object, ...) standardGeneric("cost"))

setGeneric("ccost", function(object, ...) standardGeneric("ccost"))

setGeneric("profit", function(object, ...) standardGeneric("profit"))

#' @rdname FLCatch
#' @aliases lrevenue lrevenue-method
setGeneric("lrevenue", function(object, ...) standardGeneric("lrevenue"))

setGeneric("revenue", function(object, ...) standardGeneric("revenue"))
