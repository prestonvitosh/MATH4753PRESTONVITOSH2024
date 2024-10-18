test_that("returned vector has correct length", {
  # Test 1:
  n <- 10
  iter <- 10000
  a <- 10
  b <- 20
  w <- mycltu(n, iter, a, b)
  expect_equal(length(w), iter)

  # Test 2:
  n <- 40
  iter <- 2000
  a <- 5
  b <- 10
  w <- mycltu(n, iter, a, b)
  expect_equal(length(w), iter)
})
