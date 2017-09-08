# plot.R - DESC
# /plot.R

# Copyright European Union, 2015
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# FLCatch {{{
#' @rdname FLCatch
#' @param y whatever
#' @param x whatever
#' @aliases plot,FLCatch,missing-method
setMethod("plot", signature(x="FLCatch", y="missing"),
	function(x, ...) {
        # Stupidness to appease check
        year <- NULL
        data <- NULL

		fqs <- FLQuants(Catch=catch(x), DiscardsRatio=discards.ratio(x), Price=price(x))

		p <- plot(fqs)

		p <- ggplot(data=catch(x), aes(x=year, y=data)) + geom_line() 

		p + geom_bar(data=as.data.frame(discards(x)), aes(x=year, y=data), fill="red", colour="darkred", alpha=0.5, stat="identity")
	})
# }}}
