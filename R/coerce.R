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

# asFLStock {{{
asFLStock <- function(fbi, fca) {
	
	res <- FLStock(
		# From FLBiol
		stock.n = n(fbi), stock.wt=wt(fbi),
		m=m(fbi), m.spwn=spwn(fbi), mat=fec(fbi),

		# From FLFishery
		landings.n=landings.n(fca), landings.wt=landings.wt(fca),
		discards.n=discards.n(fca), discards.wt=discards.wt(fca)
	)

	#
	stock(res) <- computeStock(res)
	
	catch(res) <- computeCatch(res, 'all')

} 


# }}}
