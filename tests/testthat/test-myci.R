test_that("returned values make sense", {
  # Test 1:
  x <- rnorm(30, 10, 12)
  alpha <- 0.05
  ci <- myci(x, alpha, 4)
  d <- (ci[2] + ci[1]) / 2 - mean(x)
  expect_gte(d, -0.001)
  expect_lte(d, 0.001)

  # Test 2:
  x <- rpois(30, 5)
  alpha <- 0.05
  ci <- myci(x, alpha, 4)
  d <- (ci[2] + ci[1]) / 2 - mean(x)
  expect_gte(d, -0.001)
  expect_lte(d, 0.001)
})
