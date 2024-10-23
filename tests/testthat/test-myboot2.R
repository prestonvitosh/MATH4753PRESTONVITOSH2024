test_that("returned vector has correct length", {
  # Test 1:
  iter <- 10000
  x <- rnorm(25, mean = 25, sd = 10)
  fun <- "mean"
  alpha <- 0.05
  cx <- 1.5
  rnd <- 2
  list <- myboot2(iter = iter, x = x, fun = fun, alpha = alpha, cx = cx, rnd = rnd)
  expect_equal(length(list$ci), 2)
  expect_identical(list$fun, fun)
  expect_identical(list$x, x)
  expect_equal(length(list$xstat), iter)
  expect_gte(list$pte, list$ci[1])
  expect_lte(list$pte, list$ci[2])

  # Test 2:
  iter <- 10000
  x <- rchisq(20, df = 3)
  fun <- "mean"
  alpha <- 0.05
  cx <- 1.5
  rnd <- 2
  list <- myboot2(iter = iter, x = x, fun = fun, alpha = alpha, cx = cx, rnd = rnd)
  expect_equal(length(list$ci), 2)
  expect_identical(list$fun, fun)
  expect_identical(list$x, x)
  expect_equal(length(list$xstat), iter)
  expect_gte(list$pte, list$ci[1])
  expect_lte(list$pte, list$ci[2])

  # Test 3:
  iter <- 10000
  x <- rgamma(30, shape = 2, scale = 3)
  fun <- "mean"
  alpha <- 0.05
  cx <- 1.5
  rnd <- 2
  list <- myboot2(iter = iter, x = x, fun = fun, alpha = alpha, cx = cx, rnd = rnd)
  expect_equal(length(list$ci), 2)
  expect_identical(list$fun, fun)
  expect_identical(list$x, x)
  expect_equal(length(list$xstat), iter)
  expect_gte(list$pte, list$ci[1])
  expect_lte(list$pte, list$ci[2])

  # Test 4:
  iter <- 10000
  x <- rbeta(20, shape1 = 3, shape2 = 4)
  fun <- "mean"
  alpha <- 0.05
  cx <- 1.5
  rnd <- 2
  list <- myboot2(iter = iter, x = x, fun = fun, alpha = alpha, cx = cx, rnd = rnd)
  expect_equal(length(list$ci), 2)
  expect_identical(list$fun, fun)
  expect_identical(list$x, x)
  expect_equal(length(list$xstat), iter)
  expect_gte(list$pte, list$ci[1])
  expect_lte(list$pte, list$ci[2])

  # Test 5:
  iter <- 10000
  x <- rnorm(25, mean = 25, sd = 10)
  fun <- "var"
  alpha <- 0.2
  cx <- 1.5
  rnd <- 2
  list <- myboot2(iter = iter, x = x, fun = fun, alpha = alpha, cx = cx, rnd = rnd)
  expect_equal(length(list$ci), 2)
  expect_identical(list$fun, fun)
  expect_identical(list$x, x)
  expect_equal(length(list$xstat), iter)
  expect_gte(list$pte, list$ci[1])
  expect_lte(list$pte, list$ci[2])

  # Test 6:
  iter <- 10000
  x <- rchisq(20, df = 3)
  fun <- "var"
  alpha <- 0.2
  cx <- 1.5
  rnd <- 2
  list <- myboot2(iter = iter, x = x, fun = fun, alpha = alpha, cx = cx, rnd = rnd)
  expect_equal(length(list$ci), 2)
  expect_identical(list$fun, fun)
  expect_identical(list$x, x)
  expect_equal(length(list$xstat), iter)
  expect_gte(list$pte, list$ci[1])
  expect_lte(list$pte, list$ci[2])

  # Test 7:
  iter <- 10000
  x <- rgamma(30, shape = 2, scale = 3)
  fun <- "var"
  alpha <- 0.2
  cx <- 1.5
  rnd <- 2
  list <- myboot2(iter = iter, x = x, fun = fun, alpha = alpha, cx = cx, rnd = rnd)
  expect_equal(length(list$ci), 2)
  expect_identical(list$fun, fun)
  expect_identical(list$x, x)
  expect_equal(length(list$xstat), iter)
  expect_gte(list$pte, list$ci[1])
  expect_lte(list$pte, list$ci[2])

  # Test 8:
  iter <- 10000
  x <- rbeta(20, shape1 = 3, shape2 = 4)
  fun <- "var"
  alpha <- 0.2
  cx <- 1.5
  rnd <- 3
  list <- myboot2(iter = iter, x = x, fun = fun, alpha = alpha, cx = cx, rnd = rnd)
  expect_equal(length(list$ci), 2)
  expect_identical(list$fun, fun)
  expect_identical(list$x, x)
  expect_equal(length(list$xstat), iter)
  expect_gte(list$pte, list$ci[1])
  expect_lte(list$pte, list$ci[2])

  # Test 9:
  iter <- 10000
  x <- c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4)
  fun <- "mean"
  alpha <- 0.05
  cx <- 1.5
  rnd <- 2
  list <- myboot2(iter = iter, x = x, fun = fun, alpha = alpha, cx = cx, rnd = rnd)
  expect_equal(length(list$ci), 2)
  expect_identical(list$fun, fun)
  expect_identical(list$x, x)
  expect_equal(length(list$xstat), iter)
  expect_gte(list$pte, list$ci[1])
  expect_lte(list$pte, list$ci[2])
})
