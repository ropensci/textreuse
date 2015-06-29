
identical(msmall[1, , drop = FALSE], msmall[3, , drop = FALSE])

microbenchmark(apply(m, 1, function(x) {
  apply(m, 1, function(y) {
    identical(x, y)
  })
}), times = 5)
