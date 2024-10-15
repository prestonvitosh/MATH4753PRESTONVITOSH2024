test_that("correct probability returned", {
  # Test 1:
  y <- 10
  r <- 3
  p <- 0.4
  expect_equal(mybin(y, r, p), choose(y-1,r-1)*p^r*(1-p)^(y-r))

  # Test 2:
  y <- 5
  r <- 4
  p <- 0.7
  expect_equal(mybin(y, r, p), choose(y-1,r-1)*p^r*(1-p)^(y-r))
})
