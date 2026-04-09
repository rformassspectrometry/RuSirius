# Tests for import helper functions
# These tests do not require the Sirius API but may need RSirius

test_that(".createpeaks creates SimplePeak objects from matrix", {
  mat <- matrix(c(100.5, 1000, 200.3, 500), ncol = 2, byrow = TRUE)
  peaks <- RuSirius:::.createpeaks(mat)

  expect_length(peaks, 2)
  expect_equal(peaks[[1]]$mz, 100.5)
  expect_equal(peaks[[1]]$intensity, 1000)
  expect_equal(peaks[[2]]$mz, 200.3)
  expect_equal(peaks[[2]]$intensity, 500)
})

test_that(".createpeaks handles single row matrix", {
  mat <- matrix(c(150.0, 2000), ncol = 2)
  peaks <- RuSirius:::.createpeaks(mat)

  expect_length(peaks, 1)
  expect_equal(peaks[[1]]$mz, 150.0)
  expect_equal(peaks[[1]]$intensity, 2000)
})
