obj <- FLFishery(FLCatches(a=FLCatch()))
is(obj["a"], "FLFishery")
is(obj[1], "FLFishery")
is(obj[[1]], "FLCatch")
is(obj[["a"]], "FLCatch")

