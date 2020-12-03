# test-computation.R - DESC
# /test-computation.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

context("FLC/FLF/FLFs slot computation")

# LOAD nsfleet (FLFs), ple (FLB), sol (FLB)

data(nsfishery)

# TEST landings (FLC / FLF / FLFs){{{

test_that("landings(FLFs/FLF) returns FLQs, landings(FLC) an FLQ", {
  
  # FLFs
  # output is an FLQuants 
  expect_s4_class(landings(nsfleet), "FLQuants")

  # dimnames match input FLQuant
  expect_equal(unique(unlist(lapply(nsfleet, names))), names(landings(nsfleet)))

  # FLF
  expect_s4_class(landings(nsfleet[[1]]), "FLQuants")
  expect_equal(names(nsfleet[[1]]), names(landings(nsfleet[[1]])))

  # FLC
  expect_s4_class(landings(nsfleet[[1]][[1]]), "FLQuant")

}) # }}}

# TEST discards (FLC / FLF / FLFs){{{

test_that("discards(FLFs/FLF) returns FLQs, discards(FLC) an FLQ", {
  
  # FLFs
  # output is an FLQuants 
  expect_s4_class(discards(nsfleet), "FLQuants")

  # dimnames match input FLQuant
  expect_equal(unique(unlist(lapply(nsfleet, names))), names(discards(nsfleet)))

  # FLF
  expect_s4_class(discards(nsfleet[[1]]), "FLQuants")
  expect_equal(names(nsfleet[[1]]), names(discards(nsfleet[[1]])))

  # FLC
  expect_s4_class(discards(nsfleet[[1]][[1]]), "FLQuant")

}) # }}}

# TEST catch (FLC / FLF / FLFs){{{

test_that("catch(FLFs/FLF) returns FLQs, catch(FLC) an FLQ", {
  
  # FLFs
  # output is an FLQuants 
  expect_s4_class(catch(nsfleet), "FLQuants")

  # dimnames match input FLQuant
  expect_equal(unique(unlist(lapply(nsfleet, names))), names(catch(nsfleet)))

  # FLF
  expect_s4_class(catch(nsfleet[[1]]), "FLQuants")
  expect_equal(names(nsfleet[[1]]), names(catch(nsfleet[[1]])))

  # FLC
  expect_s4_class(catch(nsfleet[[1]][[1]]), "FLQuant")

}) # }}}
