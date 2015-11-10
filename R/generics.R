# generics.R - DESC
# FLFishery/R/generics.R

# Copyright 2015 Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) 1.1.
#
# Notes:

setGeneric("FLCatch", function(object, ...) standardGeneric("FLCatch"))

setGeneric("FLCatches", function(object, ...) standardGeneric("FLCatches"))

setGeneric("FLFishery", function(object, ...) standardGeneric("FLFishery"))

setGeneric("FLFisheries", function(object, ...) standardGeneric("FLFisheries"))

setGeneric("catch.sel", function(object, ...) standardGeneric("catch.sel"))

setGeneric("catch.sel<-", function(object, ..., value) standardGeneric("catch.sel<-"))

setGeneric("discards.ratio", function(object, ...) standardGeneric("discards.ratio"))

setGeneric("cost", function(object, ...) standardGeneric("cost"))

setGeneric("ccost", function(object, ...) standardGeneric("ccost"))

setGeneric("profit", function(object, ...) standardGeneric("profit"))

setGeneric("lrevenue", function(object, ...) standardGeneric("lrevenue"))

setGeneric("predictModel", function(object, ...) standardGeneric("predictModel"))
