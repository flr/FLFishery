# FLCatches.R - DESC
# FLCatches.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# XX {{{
# }}}

ca <- FLCatch(landings.n=landings.n(ple4), landings.wt=landings.wt(ple4),
	discards.n=discards.n(ple4), discards.wt=discards.wt(ple4),
	catch.sel=harvest(ple4)%/%apply(harvest(ple4), 2:6, max),
	price=catch.n(ple4))

cas <- new("FLCatches", list(PLE=ca, SOL=ca))

## 

object <- FLFishery(cas, effort=catch(ple4))

bi <- as(ple4, 'FLBiol')
