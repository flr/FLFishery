# generics.R - DESC
# FLFishery/R/generics.R

# Copyright 2015 Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) 1.1.

# CONSTRUCTORS

setGeneric("FLCatch", function(object, ...) standardGeneric("FLCatch"))

setGeneric("FLCatches", function(object, ...) standardGeneric("FLCatches"))

setGeneric("FLFishery", function(object, ...) standardGeneric("FLFishery"))

setGeneric("FLFisheries", function(object, ...) standardGeneric("FLFisheries"))

# ACCESSORS

setGeneric("capacity", function(object, ...) standardGeneric("capacity"))
setGeneric("capacity<-", function(object, ..., value) standardGeneric("capacity<-"))

setGeneric("hperiod", function(object, ...) standardGeneric("hperiod"))
setGeneric("hperiod<-", function(object, ..., value) standardGeneric("hperiod<-"))

setGeneric("vcost", function(object, ...) standardGeneric("vcost"))
setGeneric("vcost<-", function(object, ..., value) standardGeneric("vcost<-"))

setGeneric("fcost", function(object, ...) standardGeneric("fcost"))
setGeneric("fcost<-", function(object, ..., value) standardGeneric("fcost<-"))

setGeneric("orevenue", function(object, ...) standardGeneric("orevenue"))
setGeneric("orevenue<-", function(object, ..., value) standardGeneric("orevenue<-"))

setGeneric("crewshare", function(object, ...) standardGeneric("crewshare"))
setGeneric("crewshare<-", function(object, ..., value) standardGeneric("crewshare<-"))


# METHODS

setGeneric("catch.sel", function(object, ...) standardGeneric("catch.sel"))

setGeneric("catch.sel<-", function(object, ..., value) standardGeneric("catch.sel<-"))

setGeneric("discards.ratio", function(object, ...) standardGeneric("discards.ratio"))

setGeneric("cost", function(object, ...) standardGeneric("cost"))

setGeneric("ccost", function(object, ...) standardGeneric("ccost"))

setGeneric("profit", function(object, ...) standardGeneric("profit"))

setGeneric("lrevenue", function(object, ...) standardGeneric("lrevenue"))

