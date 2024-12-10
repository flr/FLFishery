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
#' @examples
#' data(nsfishery)
NULL

# harvests(FLBiol, FLFisheries) {{{

#' @rdname harvest
#' @examples
#' harvests(ple, nsfleet)

setMethod("harvests", signature(object="FLBiol", catches="FLFisheries"),
  function(object, catches, fcb=rep(1, length(catches)), units=c("f", "hr")) {
    
    # COMPUTE total catch at age
    # tca <- Reduce("+", catch.n(catches))
    tca <- catch.n(catches)
    
    # COMPUTE partial catch at age
    pca <- catch.n(catches, pos=fcb)
    
    # COMPUTE proportion
    pca <- Map(function(x,y) x / y, x=pca, y=tca)

    res <- lapply(pca, "*", e1=harvest(object, catches))

    # SET units to 'f'
    res <- lapply(res, setunits, 'f')

    return(res)
  }
) # }}}

# harvest(FLBiol, FLFishery) {{{

#' @rdname harvest
#' @examples
#' harvest(ple, nsfleet[["bt"]], fcb="ple")

setMethod("harvest", signature(object="FLBiol", catch="FLFishery"),
  function(object, catch, fcb=1) {
    
    caq <- catch.q(catch[[fcb]])

    # F = effort * alpha * sel * biomass ^ -beta
    res <- effort(catch) %*% caq$alpha %*% catch.sel(catch[[fcb]]) %*%
      ((n(object) * wt(object)) %^% -caq$beta)

    quant(res) <- "age"
    units(res) <- "f"

    return(res)
  }
) # }}}

# harvest(FLBiol, FLFisheries) {{{

#' @rdname harvest
#' @examples
#' harvest(ple, nsfleet)

setMethod("harvest", signature(object="FLBiol", catch="FLFisheries"),
  function(object, catch, fcb=1) {

    # SUM of partial Fs
    return(Reduce('+', lapply(catch, harvest, object=object)))
  }
) # }}}

# harvest(FLBiols, FLFisheries) {{{

#' @rdname harvest
#' @examples
#' data(nsfishery)
#' bis <- FLBiols(ple=ple, sol=sol)
#' harvest(bis, nsfleet)

setMethod("harvest", signature(object="FLBiols", catch="FLFisheries"),
  function(object, catch, fcb=FCB(object, catch)) {

  # GO over biols
  lapply(setNames(unique(fcb[, 'B']), nm=names(object)), function(bi) {

    # FCB map for this biol
    ma <- fcb[fcb[,'B'] == bi,, drop=FALSE]

    # ADD partial Fs over fisheries catching the biol
    Reduce("+", Map(function(fi, ca) {
      harvest(object[[bi]], fi, fcb=ca)
    }, fi=catch[c(ma[, 'F'])], ca=ma[, 'C']))
  })
  }
)
# }}}

# harvest(FLBiols, FLFishery) {{{

#' @rdname harvest
#' @examples
#' harvest(bis, nsfleet[[1]])

setMethod("harvest", signature(object="FLBiols", catch="FLFishery"),
  function(object, catch, fcb=FCB(object, FLFisheries(catch))) {
  
    fcb <- FCB(object, FLFisheries(A=catch))

    harvest(object, catch=FLFisheries(A=catch), fcb=fcb)
  }
)
# }}}

# harvest(FLBiol, FLCatch {{{

#' @rdname harvest
#' @examples
#' harvest(ple, nsfleet[["bt"]][["ple"]])

setMethod("harvest", signature(object="FLBiol", catch="FLCatch"),
  function(object, catch) {
    return(harvest(n(object), catch=catch.n(catch), m=m(object)))
  }
) # }}}

# fbar(FLBiol) {{{

#' @rdname harvest
#' @examples
#' fbar(ple, fisheries=nsfleet, minfbar=3, maxfbar=6)
#' fbar(ple, fisheries=nsfleet[["bt"]], minfbar=3, maxfbar=6)

setMethod("fbar", signature(object="FLBiol"),
  function(object, fisheries, range=dims(object)[c("min", "max")],
    minfbar=range$min, maxfbar=range$max, ...) {

    if(!is.null(names(range)))
     range <- range[pmatch(c("min", "max"), names(range))]

    harvest <- harvest(object, fisheries, ...)

    return(quantMeans(harvest[ac(seq(minfbar, maxfbar))]))
  }
) # }}}

# fbar(FLBiols) {{{

#' @rdname harvest
#' @examples
#' fbar(FLBiols(ple=ple, sol=sol), nsfleet)

setMethod("fbar", signature(object="FLBiols"),
  function(object, fisheries, range=lapply(object,
    function(x) dims(x)[c("min", "max")]), ...) {

    har <- harvest(object, fisheries)

    Map(function(x, y) quantMeans(x[ac(seq(y$min, y$max))]),
      x=har, y=lapply(range, 'as.list'))
  }
)
# }}}
