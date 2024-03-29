# coerce.R - Coercion to and from FLFishery classes.
# FLFishery/R/coerce.R

# Copyright European Union, 2015 
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) V.1.1.

# FLStock -> FLCatch {{{

setAs('FLStock', 'FLCatch',
	function(from) {
    
    sel <- sweep(harvest(from), 2:6, apply(harvest(from), 2:6, max), "/")
    sel[is.na(sel)] <- 0
    units(sel) <- ""
		
    out <- FLCatch(name=name(from), desc=desc(from),
      landings.n=landings.n(from), landings.wt=landings.wt(from),
			discards.n=discards.n(from), discards.wt=discards.wt(from),
      catch.sel= sel, catch.q=FLPar(alpha=1, beta=0))
    
    # Empty desc and name slots are a frequent issue, i.e. character(0)
    # So check if empty and if fill with something
    if (identical(character(0), name(out))){
      name(out) <- ""
    }
    if (identical(character(0), desc(out))){
      desc(out) <- ""
    }
    return(out)
	}
)
# }}}

# FLStock -> FLFishery {{{

setAs('FLStock', 'FLFishery',
  function(from) {
    
    res <- FLFishery(as(from, 'FLCatch'), effort=unitSums(fbar(from)) %=% 0)

    # CAPACITY
    capacity(res)[] <- 1

    names(res) <- desc(res) <- name(from)
    
    # EFFORT = F / sel, uses mean to avoid NAs from F=0
    effort(res) <- quantMeans(unitMeans((harvest(from) / catch.sel(from))))
    effort(res)[is.na(effort(res))] <- 1e-12

    # AVOID long strings in units(effort) if stock has not proper uoms.
    units(effort(res)) <- ""

    # hperiod, only age 1
    spw <- unitMeans(m.spwn(from)[1,])
    fpr <- unitMeans(harvest.spwn(from)[1,])

    # IF fpr > spw, hperiod = 0 -- spw + (spw * (1 - fpr))
    hperiod(res)['start',][fpr > spw] <- 0
    hperiod(res)['end',][fpr > spw]  <- (spw / fpr)[fpr > spw]

    # IF fpr < spw, hperiod = spw - (spr * fpr) -- 1
    hperiod(res)['start',][fpr < spw]  <- ((fpr * (1 - spw)) / (1 - fpr))[fpr < spw]
    hperiod(res)['end',][fpr < spw] <- 1

    return(res)
  }
) # }}}

# FLFishery -> FLFisherycpp {{{
setAs("FLFishery", "FLFisherycpp",
  function(from) {
    return(new("FLFisherycpp",
      .Data=from@.Data,
      names=names(from),
      lock=from@lock,
      name=name(from),
      desc=desc(from),
      range=range(from),
      capacity=capacity(from),
      effort=effort(from),
      hperiod=hperiod(from),
      vcost=vcost(from),
      fcost=fcost(from),
      orevenue=orevenue(from),
      crewshare=crewshare(from)))
  }
) # }}}

# FLBiol,FLFisheries -> FLStock {{{


#' Create an `FLStock` object from `FLBiol` and `FLFishery` or `FLFisheries`
#'
#' A coertion method that returns a `FLStock` object by combining biological
#' information contained in `FLBiol` and catch data from one of more 
#' `FLFishery` objects.
#'
#' Details: Aliquam sagittis feugiat felis eget consequat.
#'
#' @param object An object of class `FLBiol`.
#' @param fisheries An object of classes `FLFishery` or `FLFisheries`.
#' @param full Should fishing mortality be computed and added? Logical.
#' @param catch Vector of the same length ad `fisheries`, indicating the
#' position of the `FLCatch` objects that refer to the `FLBiol`. Defaults to 
#' the first one along all elements.
#'
#' @return An object of class `FLStock`.
#'
#' @author The FLR Team
#' @seealso FLCore::FLStock
#' @keywords methods
#' @md
#' @examples
#' data(nsfishery)
#' as.FLStock(sol, nsfleet)

setMethod("as.FLStock", signature(object="FLBiol"),
  function(object, fisheries, full=TRUE, catch=rep(1, length(fisheries)),
    ...) {
  
  # PARSE FLFishery
  if(is(fisheries, "FLFishery"))
    fisheries <- FLFisheries(F=fisheries)

  # CHECK: single FLCatch per FLFishery
  m <- m(object)
  mat <- mat(object)
  spwn <- spwn(object)
  wt <- wt(object)

  # SUM all catches
  ln <- Reduce("+", Map(function(x, y) landings.n(x[[y]]),
    fisheries, catch))
  dn <- Reduce("+", Map(function(x, y) discards.n(x[[y]]),
    fisheries, catch))
  cn <- Reduce("+", Map(function(x, y) catch.n(x[[y]]),
    fisheries, catch))

  # WEIGHTED average of wts
  lw <- weighted.mean(
    FLQuants(Map(function(x, y) landings.wt(x[[y]]), x=fisheries, y=catch)),
    FLQuants(Map(function(x, y) landings.n(x[[y]]), x=fisheries, y=catch)))

  dw <- weighted.mean(
    FLQuants(Map(function(x, y) discards.wt(x[[y]]), x=fisheries, y=catch)),
    FLQuants(Map(function(x, y) discards.n(x[[y]]), x=fisheries, y=catch)))

  cw <- weighted.mean(
    FLQuants(Map(function(x, y) catch.wt(x[[y]]), x=fisheries, y=catch)),
    FLQuants(Map(function(x, y) catch.n(x[[y]]), x=fisheries, y=catch)))

  # SET harvest.spwn as catch-weighted mean of hperiod
  hspwn <- lapply(fisheries,
    function(x) (hperiod(x)['end',] - hperiod(x)['start',]) %*% spwn(object))

  hspwn <- Reduce('+', hspwn) / length(hspwn)
  quant(hspwn) <- "age"
  hspwn <- expand(hspwn, age=dimnames(m)$age)

  mspwn <- expand(spwn(object), age=dimnames(m)$age, fill=TRUE)
  units(hspwn) <- units(mspwn) <- ""

  # BUILD FLStock

  stk <- FLStock(
    name=name(object), desc=desc(object),
    # landings.n, landings.wt
    landings.n=ln, landings.wt=lw,
    # discards.n, discards.wt
    discards.n=dn, discards.wt=dw,
    # catch.n, catch.wt
    catch.n=cn, catch.wt=cw,
    # stock.wt
    stock.wt=wt,
    # m, mat
    m=m, mat=mat,
    # spwn
    m.spwn=mspwn, harvest.spwn=hspwn
    # harvest
  )

  # COMPUTE l, d, c
  landings(stk) <- computeLandings(stk)
  discards(stk) <- computeDiscards(stk)
  catch(stk) <- computeCatch(stk)

  # stock.n & stock
  stock.n(stk) <- n(object)
	stock(stk) <- computeStock(stk)
  
  # COMPUTE harvest
  if(full) {
    harvest(stk) <- harvest(object, fisheries)
  }
  
  units(harvest(stk)) <- "f"

  # ADD extra slots in ...
  args <- list(...)
  if(length(args) > 0)
    for(i in names(args))
      stk <- do.call(paste0(i, "<-"), list(stk, value=args[[i]]))

  return(stk)
})

# }}}
