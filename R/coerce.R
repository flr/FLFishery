# coerce.R - Coercion to and from FLFishery classes.
# FLFishery/R/coerce.R

# Copyright European Union, 2015 
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) V.1.1.


# FLStock  -> FLCatch {{{

setAs('FLStock', 'FLCatch',
	function(from) {

		FLCatch(name=name(from), desc=desc(from),
      landings.n=landings.n(from), landings.wt=landings.wt(from),
			discards.n=discards.n(from), discards.wt=discards.wt(from),
			catch.sel=predictModel(FLQuants(catch.sel=catch.sel(from)), model=~catch.sel),
      # catch.q
      catch.q=FLPar(alpha=c(harvest(from)[1,1] / catch.sel(from)[1,1]), beta=0))
	}
)
# }}}

# FLStock  -> FLFishery {{{

setAs('FLStock', 'FLFishery',
  function(from) {

    res <- FLFishery(as(from, 'FLCatch'))
    
    names(res) <- desc(res) <- name(from)

    effort(res)[] <- c((harvest(from) / (catch.sel(res[[1]])['alpha',] *
      catch.sel(res[[1]])))[1,])
    capacity(res)[] <- 1

    return(res)
  }
) # }}}

# FLFishery -> FLFisheryFQ {{{
setAs("FLFishery", "FLFisheryFQ",
  function(from) {
    return(new("FLFisheryFQ",
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
      ccost=cost(from)))
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
