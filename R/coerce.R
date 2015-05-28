# coerce.R - DESC
# coerce.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

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
	function(from)
	{
		FLFishery(as(from, 'FLCatch'), effort=catch(from) / esb(from))
	}
)
# }}}


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

