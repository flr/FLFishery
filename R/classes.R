# classes.R - The FLFishery classes
# FLFishery/R/classes.R

# Copyright European Union, 2015 
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) V.1.1.

# FLCatch {{{

#' Class for FLFishery catch data
#'
#' Catch data for a single species or stock unit is handled by the
#' \code{FLCatch} class. Data is separated as landings and discards by age, in
#' numbers, with the corresponding mean weights at age.
#' 
#' This is class is used inside \code{FLFishery} to store the catches of
#' a single stock species caught obtained by that fleet.
#' 
#' @name FLCatch
#' @rdname FLCatch
#' @docType class
#' @aliases FLCatch FLCatch-class FLCatch-methods FLCatch,FLQuant-method FLCatch,missing-method landings.n,FLCatch-method name,FLCatch-method desc,FLCatch-method range,FLCatch-method landings.n,FLCatch-method landings.wt,FLCatch-method discards.n,FLCatch-method discards.wt,FLCatch-method catch.sel,FLCatch-method price,FLCatch-method catch.q,FLCatch-method name<-,FLCatch,character-method desc<-,FLCatch,character-method range<-,FLCatch,numeric-method landings.n<-,FLCatch,FLQuant-method landings.wt<-,FLCatch,FLQuant-method discards.n<-,FLCatch,FLQuant-method discards.wt<-,FLCatch,FLQuant-method catch.sel<-,FLCatch,FLQuant-method price<-,FLCatch,FLQuant-method catch.q<-,FLCatch,FLPar-method discards.sel,FLCatch-method landings.sel,FLCatch-method discards,FLCatch-method catch,FLCatch-method catch.n,FLCatch-method catch.wt,FLCatch-method landings,FLCatch-method discards.ratio,FLCatch-method
#'
#' @section Slots:
#'     \describe{
#'     \item{catch.q}{Parameters of the catchability function, (\code{FLPar}).}
#'     \item{catch.sel}{Selectivity at age as proportions over fully-selected
#'       ages, (\code{FLQuant}).}
#'     \item{desc}{Description of the data contents and origin, (\code{character}).}
#'     \item{discards.n}{Discards at age in numbers, (\code{FLQuant}).}
#'     \item{discards.wt}{Mean weight-at-age in the discards, (\code{FLQuant}).}
#'     \item{name}{Name of the object, e.g. species or stock code, (\code{character}).}
#'     \item{landings.n}{Landings at age in numbers, (\code{FLQuant}).}
#'     \item{landings.wt}{Mean weight-at-age in the landings, (\code{FLQuant}).}
#'     \item{price}{Mean price by age per unit of weight, (\code{FLQuant}).}
#'     \item{range}{Ranges of age and years, plusgroup, (\code{numeric}).}
#' }
#'
#' @section Validity:
#'
#'   \describe{
#'     \item{Length of dimensions 2:5}{All \code{FLQuant} slots must share
#'       dimensions 2 to 5.}
#'     \item{\code{iter} dim of length 1 or N}{The 6th dimension in all
#'       \code{FLQuant} and \code{FLPar} slots must be 1 or N, where N is the
#'       same value for the whole object.}
#'     \item{Length of dimensions 2:5}{All \code{FLQuant} slots must share
#'       dimensions 2 to 5.}
#'   }
#'
#' You can inspect the class validity function by using
#'    \code{getValidity(getClassDef('FLCatch'))}
#'
#' @section Accessors:
#' All slots in the class have accessor and replacement methods defined that
#' allow retrieving and substituting individual slots.
#'
#' The values passed for replacement need to be of the class of that slot.
#' A numeric vector can also be used when replacing FLQuant slots, and the
#' vector will be used to substitute the values in the slot, but not its other
#' attributes.
#'
#' @section Constructor:
#' A construction method exists for this class that can take named arguments for
#' any of its slots. All slots are then created to match the requirements of the
#' class validity. If an unnamed \code{FLQuant} object is provided, this is used
#' for sizing but not stored in any slot.
#'
#' @section Methods:
#' Methods exist for various calculations based on values stored in the class:
#'
#' \describe{
#'  \item{\code{landings}}{Total landings as sum on 'age' of \code{landings.n}
#'    times \code{landings.wt}.}
#'  \item{\code{discards}}{Total discards as sum on 'age' of \code{discards.n}
#'    times \code{discards.wt}.}
#'     \item{landings.sel:}{Selectivity at age in the landings as proportions
#'      over fully-selected ages, (\code{FLQuant}).}
#'     \item{discards.sel:}{Selectivity at age in the discards as proportions
#'      over fully-selected ages, (\code{FLQuant}).}
#'  \item{\code{catch.n}}{Catch at age in numbers as \code{landings.n} plus
#'    \code{discards.n}.}
#'  \item{\code{catch.wt}}{Weighted average of \code{landings.wt} and
#'    \code{discards.wt}.}
#'  \item{\code{catch}}{Total catch as sum of \code{landings} and
#'    \code{discards}.}
#'  \item{\code{discards.ratio}}{Proportion at age of discards in catch.}
#'  \item{\code{plot}}{Standard plot for the FLCatch class.}
#' }
#'
#' @author Iago Mosqueira, EC JRC.
#' @seealso \link{FLCatches}, \link{FLFishery}
#' @keywords classes
#' @examples
#'
#' data(ple4)
#' 
#' # EXTRACT data from FLCore ple4, fake prices
#' fca <- FLCatch(name='PLE', desc='All NS PLE catches',
#'   landings.n=landings.n(ple4), landings.wt=landings.wt(ple4),
#'   discards.n=discards.n(ple4), discards.wt=discards.wt(ple4),
#'   price=landings.wt(ple4) * 23, catch.q=FLPar(q=1),
#'   catch.sel=catch.sel(ple4))
#'
#' # Calculations
#' landings(fca)
#' 
#' catch.n(fca)
#' catch.wt(fca)
#' 

setClass("FLCatch",
  contains="FLComp",
  representation(
    landings.n = "FLQuant",
    landings.wt = "FLQuant",
    discards.n = "FLQuant",
    discards.wt = "FLQuant",
    catch.sel = "FLQuant",
    price = "FLQuant",
    catch.q = "FLPar"),
  prototype(
    name = character(0),
    desc = character(0),
    range = as.numeric(c(min=NA, max=NA, plusgroup=NA,
      minyear=NA, maxyear=NA)),
    landings.n = FLQuant(),
    landings.wt = FLQuant(),
    discards.n = FLQuant(),
    discards.wt = FLQuant(),
    catch.sel = FLQuant(),
    price = FLQuant(),
    catch.q = FLPar(a=NA, b=NA)),

  # VALIDITY
  validity=function(object) {

    # dims[1:5]
    # iter 1 or N
    # catch.q iter 1 or N
    # catch.q dims equal to flqs
    return(TRUE)
  }
) # }}}

# FLCatches {{{
setClass("FLCatches",
  
  contains=c("FLlst"),

  # VALIDITY
  validity=function(object) {

    # all object are FLCatch
    if(any(!unlist(lapply(object, is, 'FLCatch'))))
      return("Input objects must be of class 'FLCatch'")

    dmns <- lapply(object, dims)

    # quant == 'age'
    qua <- unlist(lapply(dmns, '[', 'quant'))
    if(length(unique(qua)) > 1)
      return("FLCatch objects must have quant='age'")

    # dims [c(2,3,4,5)] must be the same
    dmns <- lapply(object, function(x) dimnames(landings.n(x))[-c(1, 6)])
    if(sum(duplicated(dmns)) != (length(dmns) -1))
      return(paste("All FLCatch objects must share dimensions 2 to 5: ",
        names(dmns)[!duplicated(dmns)][-1]))

    # iters 1 or N

    return(TRUE)
  }
) # }}}

# FLFishery {{{
setClass("FLFishery",
  contains=c("FLComp", "FLCatches"),
  representation(
    capacity="FLQuant",
    effort="FLQuant",
    hperiod="FLQuant",
    vcost="FLQuant",
    fcost="FLQuant",
    orevenue="FLQuant",
    crewshare="predictModel"),
  prototype(
    capacity=FLQuant(dimnames=list(quant=c("all"))),
    effort=FLQuant(dimnames=list(quant=c("all"))),
    hperiod=FLQuant(dimnames=list(quant=c("start", "end"))),
    vcost=FLQuant(dimnames=list(quant=c("all"))),
    fcost=FLQuant(dimnames=list(quant=c("all"))),
    orevenue=FLQuant(dimnames=list(quant=c("all"))),
    crewshare=predictModel(model=~fixed * crew + share * lrevenue,
      params=FLPar(fixed=0, crew=1, share=0.0))),
  # VALIDITY
  validity=function(object) {

    # dims[2:5] of flqs match dims of flcs

    # iters 1 or N FLCs vs FLQs

    # FLQuant slots quant='age'

    return(TRUE)
  }
) # }}}

# FLFisheryFQ {{{
setClass("FLFisheryFQ",
  contains=c("FLComp", "FLCatches"),
  representation(
    capacity="FLQuant",
    effort="FLQuant",
    hperiod="FLQuant",
    vcost="FLQuant",
    fcost="FLQuant",
    orevenue="FLQuant",
    ccost="FLQuant"),
  prototype(
    capacity=FLQuant(dimnames=list(quant=c("all"))),
    effort=FLQuant(dimnames=list(quant=c("all"))),
    hperiod=FLQuant(dimnames=list(quant=c("start", "end"))),
    vcost=FLQuant(dimnames=list(quant=c("all"))),
    fcost=FLQuant(dimnames=list(quant=c("all"))),
    orevenue=FLQuant(dimnames=list(quant=c("all"))),
    ccost=FLQuant(dimnames=list(quant=c("all"))),
      params=FLPar(fixed=0, crew=1, share=0.0)),
  # VALIDITY
  validity=function(object) {

    # dims[2:5] of flqs match dims of flcs

    # iters 1 or N FLCs vs FLQs

    # FLQuant slots quant='age'

    return(TRUE)
  }
) # }}}

# FLFisheries {{{
setClass("FLFisheries", contains=c("FLlst"),

  # VALIDITY
  validity=function(object) {

    # all objects are FLFishery
    if(any(!unlist(lapply(object, is, 'FLFishery'))))
      return("Input objects must be of class 'FLFishery'")

    # dmns <- lapply(object, dims)

    # quant == 'age'
    # qua <- unlist(lapply(dmns, '[', 'quant'))
    # if(length(unique(qua)) > 1)
    #   return("FLFishery objects must have quant='age'")

    # iters 1 or N

    return(TRUE)
  }
) # }}}
