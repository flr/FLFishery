# harvest.R - DESC
# /harvest.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# partialF(FLBiol, FLFisheries) {{{
setMethod("partialF", signature(object="FLBiol", catches="FLFisheries"),
  function(object, catches, fcb=rep(1, length(catches)), ...) {
  
    res <- vector("list", length=length(catches))

    for(fl in seq(length(catches))) {

      res[[fl]] <- harvest(object, catches[[fl]], fcb=fcb[fl])
    }
    names(res) <- names(catches)
    res <- FLQuants(res)
    res <- lapply(res, "units<-", "f")
    
    return(res)
  }
) # }}}

# partialF(FLBiol, FLFisheries) {{{
setMethod("harvest", signature(object="FLBiol", catch="FLFishery"),
  function(object, catch, fcb=1) {
  
    # F = alpha * n * wt ^ (-1 * beta) * catch.sel
    res <- ((catch.q(catch[[fcb]])$alpha *
      quantSums(n(object) * wt(object)) ^
      (- 1 * catch.q(catch[[fcb]])$beta)) * effort(catch)) %*%
      catch.sel(catch[[fcb]])
    
    units(res) <- "f"
    return(res)
  }
) # }}}

# harvest(FLBiol, FLFisheries) {{{
setMethod("harvest", signature(object="FLBiol", catch="FLFisheries"),
	function(object, catch, ...) {

    res <- Reduce("+",  partialF(object, catches, ...))
    units(res) <- "f"
	  return(res)
	}
) # }}}
