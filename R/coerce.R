# coerce.R - Coercion to and from FLFishery classes.
# FLFishery/R/coerce.R

# Copyright European Union, 2015 
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under terms of the European Union Public Licence (EUPL) V.1.1.


# FLStock  -> FLCatch {{{

setAs('FLStock', 'FLCatch',
	function(from)
	{
		FLCatch(landings.n=landings.n(from), landings.wt=landings.wt(from),
			discards.n=discards.n(from), discards.wt=discards.wt(from),
			catch.sel=catch.sel(from))
	}
)
# }}}

# FLStock  -> FLFishery {{{

setAs('FLStock', 'FLFishery',
  function(from) {
    FLFishery(as(from, 'FLCatch'), effort=catch(from) / esb(from))
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

