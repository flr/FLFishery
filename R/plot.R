# plot.R - DESC
# ggplotFL/R/plot.R

# Copyright European Union, 2015-2019
# Author: Iago Mosqueira (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

globalVariables(c("data", "year", "qname", "fishery"))

# FLCatch {{{

#' @rdname FLCatch
#' @param x FLCatch
#' @param y missing

setMethod("plot", signature(x="FLCatch", y="missing"),
	function(x, ...) {
 
		fqs <- FLQuants(Catch=catch(x), DiscardsRatio=discards.ratio(x))

		p <- plot(fqs)

		p <- ggplot(data=catch(x), aes(x=year, y=data)) + geom_line() 

		p + geom_bar(data=as.data.frame(discards(x)), aes(x=year, y=data), fill="red", colour="darkred", alpha=0.5, stat="identity")
	})
# }}}

# FLFishery {{{
# effort, catch by spp, revenue, (profit)

setMethod("plot", signature(x="FLFishery"),
  function(x) {
    
  # GET effort data
  dataE <- cbind(as.data.frame(effort(x), date=TRUE, units=TRUE),
    qname='eff', panel="Effort")
  dataC <- cbind(as.data.frame(lapply(catch(x), unitSums), date=TRUE,
    units=TRUE), panel="Catch")
  names(dataE) <- names(dataC)
  data <- rbind(dataE, dataC)

  labels <- ggplotFL:::format_label_flqs(c(Effort=dataE$units[1], Catch=dataC$units[1]),
    c(Effort="Effort", Catch="Catch"))

  ggplot(data, aes(x=year, y=data, group=qname)) + geom_line() +
    facet_grid(panel~., scales="free_y", labeller=labels) +
    theme() +
    ylim(c(0, NA)) + ylab("") + xlab("")

  }) # }}}

# FLFisheries {{{

setMethod("plot", signature(x="FLFisheries"),
  function(x) {

    # PLOT catch by fleet ~ catch
    
    # GET catch names
    canms <- unique(unlist(lapply(x, names)))

    # EXTRACT FLQuants
    eff <- lapply(x, effort)
    cas <- lapply(x, lapply, catch)

    return(Reduce("/", lapply(canms, function(i) 
      plot(FLQuants(lapply(cas, function(j) lapply(j, unitSums)[[i]]))) +
      ggtitle(i))))

    # GENERATE metrics as catches + eff
    mets <- Map(function(x, y) FLQuants(c(x, list(effort=y))), cas, eff)
    dfs <- lapply(mets, as.data.frame, date=TRUE)

    # DATA with qname (eff, SPP) and fishery
    dat <- do.call(rbind,
      c(mapply(cbind, dfs, fishery=names(mets), SIMPLIFY=FALSE),
      list(make.row.names = FALSE)))

    ggplot(dat, aes(x=date, y=data)) + geom_line(aes(colour=fishery)) +
      facet_grid(qname ~ fishery, scales="free_y") +
      xlab("") + ylab("") + ylim(c(0, NA)) +
      theme(legend.position="none")

  }
) # }}}

# FLBiol, FLFishery
# FLBiol, FLFisheries

setMethod("plot", signature(x="FLBiol", y="FLFishery"),
  function(x, y) {

    # COMPUTE metrics
    re <- n(x)[1,]
    sb <- ssb(x, catch.n=Reduce("+", catch.n(y)))
    c <- catch(y)[[1]]
    f <- fbar(x, y)

    return(plot(FLQuants(Rec=re, SSB=sb, C=c, F=f)) +
      ylim(c(0,NA)))
  }
)

# FLBiols, FLFishery
# FLBiols, FLFisheries
