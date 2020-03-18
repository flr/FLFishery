# harvest.R - DESC
# FLFishery/R/harvest.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


#' Methods to calculate fishing mortalities
#'
#' Fishing mortalities and harvest rates can be calculated for a combination of
#' FLBiol and FLFishery/FLFisheries using the harvest() method.
#'
#' The calculated fishing mortalities, or havest rates, are returned, in the
#' case of *harvest*, disaggregated by fishery, as an *FLQuants* list. For a
#' single *FLFishery* object, a single *FLQuant* is obtained.
#'
#' @param object Object containing population abundances, of class *FLBiol*.
#' @param catch Object containing catches in number of the population represented by object, class *FLFishery*.
#' @param catches Object containing catches in number of the population represented by object, class *FLFisheries*.
#' @param fcb A vector indicating the correspondance, by position of name, between *object* and the *FLCatch* elements inside *catches*, and of the same length.
#' @param units Should output be in terms of fishing mortaloty ('f') or harvest rate ('hr').
#'
#' @return An object of class *FLQuant* or *FLQuants*.
#'
#' @name harvest
#' @rdname harvest
#'
#' @author The FLR Team
#' @seealso [FLCore::harvest()]
#' @keywords methods
#' @md
NULL

# harvests(FLBiol, FLFisheries) {{{
#' @rdname harvest
setMethod("harvests", signature(object="FLBiol", catches="FLFisheries"),
  function(object, catches, fcb=rep(1, length(catches)), units=c("f", "hr")) {
 
    res <- vector("list", length=length(catches))

    if(units[1] == "f") {
      for(fl in seq(length(catches)))
        res[[fl]] <- harvest(object, catches[[fl]], fcb=fcb[fl])
    } else if (units[1] == "hr") {
      for(fl in seq(length(catches)))
        res[[fl]] <- harvest(object, catches[[fl]], fcb=fcb[fl])
    }

      names(res) <- names(catches)
      res <- FLQuants(res)
      res <- lapply(res, "units<-", "f")
    
    return(res)
  }
) # }}}

# harvest(FLBiol, FLFishery) {{{
#' @rdname harvest
setMethod("harvest", signature(object="FLBiol", catch="FLFishery"),
  function(object, catch, fcb=1) {

    return(harvest(n(object), catch.n(catch[[fcb]]), m(object)))

    # TODO
  
    # F = alpha * effort * (n * wt) ^ (-1 * beta) * catch.sel
    res <- ((catch.q(catch[[fcb]])$alpha *
      quantSums(n(object) * wt(object)) ^
      (- 1 * catch.q(catch[[fcb]])$beta)) * effort(catch)) %*%
      catch.sel(catch[[fcb]])
    
    units(res) <- "f"
    return(res)
  }
) # }}}

# harvest(FLBiol, FLFisheries) {{{
#' @rdname harvest
setMethod("harvest", signature(object="FLBiol", catch="FLFisheries"),
  function(object, catch, fcb=1) {

    return(harvest(n(object),
      # GET catch.n of fcbs FLCatches across all fisheries
      Reduce('+', mapply(function(x, y)
        catch.n(x[[y]]), catch, fcb, SIMPLIFY = FALSE)), 
      m(object)))
  }
) # }}}

# harvest(FLBiol, FLCatch {{{
#' @rdname harvest
setMethod("harvest", signature(object="FLBiol", catch="FLCatch"),
  function(object, catch) {
    return(harvest(n(object), catch.n(catch), m(object)))
  }
) # }}}

# TODO fbar(FLBiol, FLFisheries)
setMethod("fbar", signature(object="FLBiol"),
  function(object, fisheries, range=c(minfbar, maxfbar),
    minfbar=range[1], maxfbar=range[2]) {

    if(!is.null(names(range)))
     range <- range[pmatch(c("min", "max"), names(range))]

    harvest <- harvest(object, fisheries)

    return(quantMeans(harvest[seq(ac(range[1], range[2]))]))
  }
)
