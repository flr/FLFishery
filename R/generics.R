# generics.R - DESC
# generics.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

setGeneric('FLCatch', function(object, ...) standardGeneric('FLCatch'))

setGeneric('FLFishery', function(object, ...) standardGeneric('FLFishery'))

setGeneric('catch.sel', function(object, ...) standardGeneric('catch.sel'))

setGeneric('catch.sel<-', function(object, ..., value) standardGeneric('catch.sel<-'))

setGeneric('discards.ratio', function(object, ...) standardGeneric('discards.ratio'))

setGeneric('cost', function(object, ...) standardGeneric('cost'))
