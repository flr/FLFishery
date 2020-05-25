
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
#' a single stock or species caught by that fleet.
#' 
#' @name FLCatch
#' @rdname FLCatch
#' @docType class
#' @aliases FLCatch FLCatch-class FLCatch-methods landings.n,FLCatch-method name,FLCatch-method desc,FLCatch-method range,FLCatch-method landings.n,FLCatch-method landings.wt,FLCatch-method discards.n,FLCatch-method discards.wt,FLCatch-method catch.sel,FLCatch-method price,FLCatch-method catch.q,FLCatch-method name<-,FLCatch,character-method desc<-,FLCatch,character-method range<-,FLCatch,numeric-method landings.n<-,FLCatch,FLQuant-method landings.wt<-,FLCatch,FLQuant-method discards.n<-,FLCatch,FLQuant-method discards.wt<-,FLCatch,FLQuant-method catch.sel<-,FLCatch,FLQuant-method price<-,FLCatch,FLQuant-method catch.q<-,FLCatch,FLPar-method discards.sel,FLCatch-method landings.sel,FLCatch-method discards,FLCatch-method catch,FLCatch-method catch.n,FLCatch-method catch.wt,FLCatch-method landings,FLCatch-method discards.ratio,FLCatch-method
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
#'  \item{landings}{Total landings as sum on 'age' of \code{landings.n}
#'    times \code{landings.wt}.}
#'  \item{discards}{Total discards as sum on 'age' of \code{discards.n}
#'    times \code{discards.wt}.}
#'  \item{landings.sel}{Selectivity at age in the landings as proportions
#'      over fully-selected ages, (\code{FLQuant}).}
#'  \item{discards.sel}{Selectivity at age in the discards as proportions
#'      over fully-selected ages, (\code{FLQuant}).}
#'  \item{catch.n}{Catch at age in numbers as \code{landings.n} plus
#'    \code{discards.n}.}
#'  \item{catch.wt}{Weighted average of \code{landings.wt} and
#'    \code{discards.wt}.}
#'  \item{catch}{Total catch as sum of \code{landings} and
#'    \code{discards}.}
#'  \item{discards.ratio}{Proportion at age of discards in catch.}
#'  \item{plot}{Standard plot for the FLCatch class.}
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
    # TODO price as predictModel
    price = "FLQuant",
    catch.q = "FLPar"),
  prototype(
    name = character(1),
    desc = character(1),
    range = as.numeric(c(min=NA, max=NA, plusgroup=NA,
      minyear=NA, maxyear=NA)),
    landings.n = FLQuant(),
    landings.wt = FLQuant(),
    discards.n = FLQuant(),
    discards.wt = FLQuant(),
    catch.sel = FLQuant(),
    price = FLQuant(),
    catch.q = FLPar(alpha=1, beta=0)),

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

#' List class for FLFishery catch data
#'
#' This is class is used inside \code{FLFishery} to store the catches of
#' all species caught by that fleet. It is not meant to be used directly.
#' 
#' @name FLCatches
#' @rdname FLCatches
#' @docType class
#' @aliases FLCatches FLCatches-class FLCatches-methods
#'
#' @section Validity:
#'
#'   \describe{
#'     \item{Length of dimensions 2:5}{All elements must be of class \code{FLCatch}}
#'     \item{Length of dimensions 2, 4 and 5}{All \code{FLQuant} slots must share
#'       dimensions 2, 4 and 5 (year, season and area).}
#'     \item{quant must 'age'}{The 1st dimension in elements must be 'age'.}
#'   }
#'
#' You can inspect the class validity function by using
#'    \code{getValidity(getClassDef('FLCatches'))}
#'
#' @section Accessors:
#' Elements in the classes can be extracted and replaced using the list subset
#' operators,'[', '[<-', '[[' and '[[<-'.
#'
#' The values passed for replacement need to be of the class FLCatch.
#'
#' @section Constructor:
#' A construction method exists for this class that can take named arguments for
#' any of its elements. 
#'
#' @section Methods:
#' Methods exist for various operations with elements stored in the class:
#'
#' \describe{
#'  \item{plot}{Standard plot for the FLCatches class.}
#' }
#'
#' @author Iago Mosqueira, EC JRC.
#' @seealso \link{FLCatch}, \link{FLFishery}
#' @keywords classes

setClass("FLCatches",
  contains=c("FLlst"),

  # VALIDITY
  validity=function(object) {

    # all object are FLCatch
    if(any(!unlist(lapply(object, is, "FLCatch"))))
      return("Input objects must be of class 'FLCatch'")

    dmns <- lapply(object, dims)

    # quant == 'age'
    qua <- unlist(lapply(dmns, "[", "quant"))
    if(length(unique(qua)) > 1)
      return("FLCatch objects must all share the same quant")

    # dims [c(2,4,5)] must be the same - units can be different
    dmns <- lapply(object, function(x) dimnames(landings.n(x))[-c(1,3,6)])
    if(sum(duplicated(dmns)) != (length(dmns) - 1))
      return(paste("All FLCatch objects must share dimensions 2, 4 and 5: ",
        names(dmns)[!duplicated(dmns)][-1]))

    # iters 1 or N

    return(TRUE)
  }
) # }}}

# FLFishery {{{

#' A class for homogeneous fishing fleets
#'
#' Fishing fleets consisting of a number of boats operating homogeneously can be
#' modelled using the \code{FLFishery} class. All boats in the fleet must have a
#' common gear configuration during each time step and area (no \emph{metiers}).
#'
#' What do you say what I can ever do for you
#' What are we gonna do to pass the time
#' What do you care when you find that life's unfair
#' Equality is just a state of mind
#' Believe whatever is right what's right for you tonight
#' You know where to draw the line 
#'
#' @name FLFishery
#' @rdname FLFishery
#' @docType class
#' @aliases FLFishery FLFishery-methods FLFishery-class vcost,FLFishery-method vcost<-,FLFishery,FLQuant-method catch.n,FLFishery-method catch,FLFishery-method ccost,FLFishery-method cost,FLFishery-method crewshare,FLFishery-method crewshare<-,FLFishery,predictModel-method profit,FLFishery-method orevenue,FLFishery-method orevenue<-,FLFishery,FLQuant-method orevenue<-,FLFishery,numeric-method lrevenue,FLFishery-method landings,FLFishery-method hperiod,FLFishery-method hperiod<-,FLFishery,FLQuant-method hperiod<-,FLFishery,numeric-method fcost<-,FLFishery,FLQuant-method fcost,FLFishery-method effort<-,FLFishery,FLQuant-method effort,FLFishery,ANY-method discards,FLFishery-method [[<-,FLFishery,character,missing,FLCatch-method [[<-,FLFishery,numeric,missing,FLCatch-method [,FLFishery,ANY,missing,ANY-method
#'
#' @section Slots:
#'
#' \code{FLFishery} objects inherit from \code{FLCatches} with a number of slots
#' added.
#'     \describe{
#'     \item{.Data}{The list of \code{FLCatch} object with catch data per stock,
#'       (\code{FLCatches}).}
#'     \item{name}{Name of the object, e.g. species or stock code, (\code{character}).}
#'     \item{desc}{Description of the data contents and origin, (\code{character}).}
#'     \item{range}{Ranges of age and years, plusgroup, (\code{numeric}).}
#'     \item{capacity}{Number of boats in the fleet, (\code{FLQuant}).}
#'     \item{effort}{Mean effort per boat applied by the fleet, (\code{FLQuant}).}
#'     \item{hperiod}{Start and end of fishing within each time step, as
#'       proportions. An \code{FLQuant} object with dimnames
#'       `quant=c('start', 'end')` in the first dimension.}
#'     \item{vcost}{Variable costs per unit of effort, (\code{FLQuant}).}
#'     \item{fcost}{Variable costs per unit of effort, (\code{FLQuant}).}
#'     \item{orevenue}{Revenues obtained from sources other than landings, (\code{FLQuant}).}
#'     \item{crewshare}{Formula, parameter values and inputs to calculate the
#'       crew costs, (\code{predictModel}).}
#' }
#'
#' @section Validity:
#'
#' You can inspect the class validity function by using
#'    \code{getValidity(getClassDef('FLFishery'))}
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
#'   \item{ccost}{Calculate the total crew costs by evaluating the formula in
#'     \code{crewshare}.}
#'   \item{cost}{Total costs, calculated.}
#'   \item{lrevenue}{.}
#'   \item{revenue}{.}
#'   \item{profit}{.}
#'   \item{landings}{.}
#'   \item{discards}{.}
#'   \item{catch}{.}
#'   \item{catch.n}{.}
#'   \item{catch.wt}{.}
#'   \item{harvest}{.}
#' }
#'
#' @author Iago Mosqueira, EC JRC.
#' @seealso \link{FLCatches}
#' @keywords classes
#' @examples
#'
#' data(ple4)
#' FLFishery(PLE=as(ple4, 'FLCatch'))

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
    capacity=FLQuant(1, dimnames=list(quant=c("all")), units="boat"),
    effort=FLQuant(NA, dimnames=list(quant=c("all")), units="d"),
    hperiod=FLQuant(c(0, 1), dimnames=list(quant=c("start", "end")), units=""),
    # vcost = effort * landings * Z
    vcost=FLQuant(NA, dimnames=list(quant=c("all")), units="EUR/d"),
    # fcost = capacity * X
    fcost=FLQuant(NA, dimnames=list(quant=c("all")), units="EUR/boat"),
    orevenue=FLQuant(NA, dimnames=list(quant=c("all")), units="EUR/boat"),
    crewshare=predictModel(model=~fixed * crew + share * lrevenue,
      params=FLPar(fixed=0, crew=1, share=0.0, units=c("EUR", "", "")))),
  # VALIDITY
  validity=function(object) {

    # hperiod quant=c("start", "end")
    if(!identical(dimnames(object@hperiod)[['quant']], c("start", "end")))
      return("dimnames[['quant']] of @hperiod must be c('start', 'end')")

    # effort must have no units
    if(dim(object@effort)[3] > 1)
      stop("effort slot must have only one 'unit'")

    # dims[2:5] of flqs match dims of flcs

    # iters 1 or N FLCs vs FLQs

    # FLQuant slots quant='age'

    return(TRUE)
  }
) # }}}

# FLFisherycpp {{{

#' An internal class for homogeneous fishing fleets
#'
#' The same as the FLFishery class but all predictModel slots have been turned into FLQuant objects.
#' The class is used for passing to C++ FLR objects.
#'
#'What have you done what's in your mind what do you need
#' Where shall we go to let it out
#' What have you seen, we don't know where you've been
#' Life so often blows your candle out
#' Believe in what is right, what's right for you tonight
#' Who knows what the fuck it's all about 
#'
#' @name FLFisherycpp
#' @rdname FLFisherycpp
#' @docType class
#' @aliases FLFisherycpp FLFisherycpp-methods FLFisherycpp-class vcost,FLFisherycpp-method vcost<-,FLFisherycpp,FLQuant-method vcost<-,FLFisherycpp,numeric-method crewshare,FLFisherycpp-method crewshare<-,FLFisherycpp,FLQuant-method crewshare<-,FLFisherycpp,numeric-method hperiod,FLFisherycpp-method hperiod<-,FLFisherycpp,FLQuant-method hperiod<-,FLFisherycpp,numeric-method fcost<-,FLFisherycpp,FLQuant-method fcost<-,FLFisherycpp,numeric-method fcost,FLFisherycpp-method orevenue<-,FLFisherycpp,FLQuant-method orevenue<-,FLFisherycpp,numeric-method orevenue,FLFisherycpp-method effort,FLFisherycpp,ANY-method effort<-,FLFisherycpp,FLQuant-method effort<-,FLFisherycpp,numeric-method
#'
#' @section Slots:
#'
#' \code{FLFisherycpp} objects inherit from \code{FLCatches} woth a number of slots
#' added.
#'     \describe{
#'     \item{.Data}{The list of \code{FLCatch} object with catch data per stock,
#'       (\code{FLCatches}).}
#'     \item{name}{Name of the object, e.g. species or stock code, (\code{character}).}
#'     \item{desc}{Description of the data contents and origin, (\code{character}).}
#'     \item{range}{Ranges of age and years, plusgroup, (\code{numeric}).}
#'     \item{capacity}{Number of boats in the fleet, (\code{FLQuant}).}
#'     \item{effort}{Mean effort per boat applied by the fleet, (\code{FLQuant}).}
#'     \item{hperiod}{Start and end of fishing within each time step, as
#'       proportions. An \code{FLQuant} object with dimnames
#'       `quant=c('start', 'end')` in the first dimension.}
#'     \item{vcost}{Variable costs per unit of effort, (\code{FLQuant}).}
#'     \item{fcost}{Variable costs per unit of effort, (\code{FLQuant}).}
#'     \item{orevenue}{Revenues obtained from sources other than landings, (\code{FLQuant}).}
#'     \item{crewshare}{Formula, parameter values and inputs to calculate the
#'       crew costs, (\code{FLQuant}).}
#' }
#'
#' @section Validity:
#'
#' You can inspect the class validity function by using
#'    \code{getValidity(getClassDef('FLFishery'))}
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
#'   \item{ccost}{Calculate the total crew costs by evaluating the formula in
#'     \code{crewshare}.}
#'   \item{cost}{Total costs, calculated.}
#'   \item{lrevenue}{.}
#'   \item{revenue}{.}
#'   \item{profit}{.}
#'   \item{landings}{.}
#'   \item{discards}{.}
#'   \item{catch}{.}
#'   \item{catch.n}{.}
#'   \item{catch.wt}{.}
#'   \item{harvest}{.}
#' }
#'
#' @author Iago Mosqueira, EC JRC.
#' @seealso \link{FLCatches}
#' @keywords classes
#' @examples
#'
#' data(ple4)
#' FLFishery(PLE=as(ple4, 'FLCatch'))
setClass("FLFisherycpp",
  contains=c("FLComp", "FLCatches"),
  representation(
    capacity="FLQuant",
    effort="FLQuant",
    hperiod="FLQuant",
    vcost="FLQuant",
    fcost="FLQuant",
    orevenue="FLQuant",
    crewshare="FLQuant"),
  prototype(
    capacity=FLQuant(1, dimnames=list(quant=c("all"))),
    effort=FLQuant(NA, dimnames=list(quant=c("all"))),
    hperiod=FLQuant(c(0, 1), dimnames=list(quant=c("start", "end"))),
    vcost=FLQuant(NA, dimnames=list(quant=c("all"))),
    fcost=FLQuant(NA, dimnames=list(quant=c("all"))),
    orevenue=FLQuant(NA, dimnames=list(quant=c("all"))),
    crewshare=FLQuant(NA, dimnames=list(quant=c("all")))),
  # VALIDITY
  validity=function(object) {

    # hperiod quant=c("start", "end")
    if(!identical(dimnames(object@hperiod)[['quant']], c("start", "end")))
      return("dimnames[['quant']] of @hperiod must be c('start', 'end')")

    # dims[2:5] of flqs match dims of flcs

    # iters 1 or N FLCs vs FLQs

    # FLQuant slots quant='age'

    return(TRUE)
  }
) # }}}

# FLFisheries {{{

#' List class for FLFishery objects
#'
#' This is a containuer class for \code{FLFishery} objects.
#' 
#' @name FLFisheries
#' @rdname FLFisheries
#' @docType class
#' @aliases FLFisheries FLFisheries-class FLFisheries-methods
#'
#' @section Accessors:
#' Elements in the classes can be extracted and replaced using the list subset
#' operators,'[', '[<-', '[[' and '[[<-'.
#'
#' The values passed for replacement need to be of the class FLFishery.
#'
#' @section Constructor:
#' A construction method exists for this class that can take named arguments for
#' any of its elements. 
#'
#' @author Finlay Scott, EC JRC.
#' @seealso \link{FLCatch}, \link{FLFishery}
#' @keywords classes

setClass("FLFisheries", contains=c("FLlst"),
  # VALIDITY
  validity=function(object) {

    # all objects are FLFishery
    if(any(!unlist(lapply(object, function(x) is(x, "FLFishery") |
      is(x, "FLFisherycpp")))))
      return("Input objects must be of class 'FLFishery'")

    return(TRUE)
  }
) # }}}
