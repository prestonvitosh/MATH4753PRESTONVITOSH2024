test_that("returned values make sense", {
  # Test 1:
  n <- 30
  x <- rnorm(n, 25, 5)
  test <- "two"
  list <- bootpval(x, mu0 = 22, test = test)
  expect_lt(list$pvalue, 0.05)
  expect_equal(list$n, n)
  expect_identical(list$x, x)
  expect_identical(list$test, test)
  expect_lte(list$ci[1], list$ci[2])

  # Test 2:
  n <- 35
  x <- rnorm(n, 25, 5)
  test <- "two"
  list <- bootpval(x, mu0 = 25, test = test)
  expect_gt(list$pvalue, 0.05)
  expect_equal(list$n, n)
  expect_identical(list$x, x)
  expect_identical(list$test, test)
  expect_lte(list$ci[1], list$ci[2])
})
