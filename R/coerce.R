# coerce.R - DESC
# coerce.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# FLStock {{{

setAs('FLStock', 'FLCatch',
	function(from)
	{
		FLCatch(landings.n=landings.n(from), landings.wt=landings.wt(from),
			discards.n=discards.n(from), discards.wt=discards.wt(from),
			catch.sel=harvest(from) %/% apply(harvest(from), 2:6, max))
	}
)
# }}}

