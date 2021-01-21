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
    
    res <- FLFishery(as(from, 'FLCatch'), effort=unitSums(fbar(from)))

    # CAPACITY
    capacity(res)[] <- 1

    names(res) <- desc(res) <- name(from)
    
    # EFFORT
    fages <- do.call(seq,as.list(unname(range(from)[c("minfbar",
      "maxfbar")])))

    effort(res) <- FLQuant(unitSums(quantMeans((harvest(from) %/%
      catch.sel(from))[ac(fages),])), units="")
    
    # hperiod
    spw <- unitMeans(m.spwn(from)[1,])
    fpr <- unitMeans(harvest.spwn(from)[1,])

    # IF fpr > spw, hperiod = 0 -- spw + (spw * (1 - fpr))
    hperiod(res)['start',][fpr > spw] <- 0
    hperiod(res)['end',][fpr > spw]  <- (spw + spw * (1 - fpr))[fpr > spw]

    # IF fpr < spw, hperiod = spw - (spr * fpr) -- 1
    hperiod(res)['start',][fpr < spw]  <- (spw - spw * fpr)[fpr < spw]
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
setMethod("as.FLStock", signature(object="FLBiol"),
  function(object, fisheries, full=TRUE, catch=rep(1, length(fisheries)), ...) {

  # PARSE FLFishery
  if(is(fisheries, "FLFishery"))
    fisheries <- FLFisheries(F=fisheries)

  # CHECK: single FLCatch per FLFishery
  m <- m(object)
  mat <- mat(object)
  spwn <- spwn(object)
  wt <- wt(object)

  # SUM all catches
  ln <- Reduce("+", mapply(function(x, y) landings.n(x[[y]]), fisheries, catch,
    SIMPLIFY=FALSE))
  dn <- Reduce("+", mapply(function(x, y) discards.n(x[[y]]), fisheries, catch,
    SIMPLIFY=FALSE))
  cn <- Reduce("+", mapply(function(x, y) catch.n(x[[y]]), fisheries, catch,
    SIMPLIFY=FALSE))

  # WEIGHTED average of wts
  lw <- Reduce("+", mapply(function(x, y)
    landings.wt(x[[y]]) * landings.n(x[[y]]) / ln, fisheries, catch, SIMPLIFY=FALSE))
  dw <- Reduce("+", mapply(function(x, y)
    discards.wt(x[[y]]) * discards.n(x[[y]]) / dn, fisheries, catch, SIMPLIFY=FALSE))
  cw <- Reduce("+", mapply(function(x, y)
    catch.wt(x[[y]]) * catch.n(x[[y]]) / cn, fisheries, catch, SIMPLIFY=FALSE))

  # SET harvest.spwn as catch-weighted mean of hperiod
  hspwn <- lapply(fisheries,
    function(x) (hperiod(x)['end',] - hperiod(x)['start',]) * spwn(object))

  hspwn <- Reduce("+", mapply("*", mapply(function(x, y)
    catch(x[[y]]), fisheries, catch, SIMPLIFY=FALSE), hspwn, SIMPLIFY = FALSE)) /
    Reduce("+", mapply(function(x, y) catch(x[[y]]), fisheries, catch,
      SIMPLIFY=FALSE))

  hspwn <- expand(hspwn, age=dimnames(ln)$age)
  mspwn <- expand(spwn(object), age=dimnames(ln)$age, fill=TRUE)
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

  # stock.n & harvest
  stock.n(stk) <- n(object)
	stock(stk) <- computeStock(stk)
  
  if(full) {
    harvest(stk) <- harvest(stock.n(stk), catch.n(stk), m(stk))
    units(harvest(stk)) <- "f"
  }

  # ADD extra slots in ...
  args <- list(...)
  if(length(args) > 0)
    for(i in names(args))
      stk <- do.call(paste0(i, "<-"), list(stk, value=args[[i]]))

  return(stk)
})


# }}}
