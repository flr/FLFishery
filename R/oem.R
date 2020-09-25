# oem.R - DESC
# /oem.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# cpue {{{ 
setMethod('cpue', signature(object='FLBiol', index="FLI"),
  function(object, index, harvest=harvest(object), timing=0.5, mass = TRUE) {

    # I = index.q * sum(n * wt * index.sel * exp(-z * timing))

    num <- n(object) * wt(object) * sel.pattern(index) *
      exp(-(harvest * timing) - (m(object) * timing))

    cpue <- quantSums(num) * index.q(index)

  return(cpue)
  }
) # }}}

# survey (FLBiol) {{{

setMethod("survey",   signature(object="FLBiol"),
  function(object, index, catch, timing = 0.5, mass = FALSE) {
  
    # timing MUST BE 0 - 1
    timing <- pmax(pmin(timing, 1.0), 0.0)

    # CORRECT abundances for timing
    stock.n <- stock.n(object) *
      exp(-(harvest(object) * timing - m(object) * timing))
 
    # APPLY survey selectivity
    survey <- stock.n %*% sel

    # SET units as stock.n
    units(survey) <- units(stock.n)
  
    if (mass)
      survey <- survey * stock.wt(object)

    return(survey)
  }
) # }}}

