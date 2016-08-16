# test_FLCatch.R - DESC
# /test_FLCatch.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# FLCatch constructor {{{

context("FLCatch constructor")

# FLCatch(landings.n=flq) works

test_that("FLCatch(landings.n=flq) works", {
  flq <- FLQuant(runif(100, 0.1, 200), dimnames=list(age=1:5, year=1990:2001), units='t')
  flc <- FLCatch(landings.n=flq)
  # object is valid
  expect_true(validObject(flc))
  # object is FLCatch
  expect_s4_class(flc, "FLCatch")
  # dimnames match input FLQuant
  expect_equal(dimnames(discards.n(flc)), dimnames(flq))
})

# FLCatch(flq) works

test_that("FLCatch(flq) works", {
  flq <- FLQuant(runif(100, 0.1, 200), dimnames=list(age=1:5, year=1990:2001))
  flc <- FLCatch(flq)
  # object is valid
  expect_true(validObject(flc))
  # object is FLCatch
  expect_s4_class(flc, "FLCatch")
  # dimnames match input FLQuant
  expect_equal(dimnames(discards.n(flc)), dimnames(flq))
  # all slots are NA
  expect_true(all(is.na(landings.n(flc))))
})
# }}}
