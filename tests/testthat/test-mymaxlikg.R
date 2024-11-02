test_that("returned correct estimation", {
  # Test 1:
  theta.hat <- mymaxlikg(lfun = function(theta) {log(dbinom(2, 6, theta) * dbinom(4, 10, theta))}, theta = seq(0, 1, length = 10000))
  expect_gte(theta.hat, 0.375 - 0.005)
  expect_lte(theta.hat, 0.375 + 0.005)

  # Test 2:
  theta.hat <- mymaxlikg(lfun = function(theta) {log(dpois(4, theta) * dpois(6, theta) * dpois(7, theta) * dpois(6, theta) * dpois(5, theta))}, theta = seq(4, 7, length = 10000))
  expect_gte(theta.hat, 5.6 - 0.005)
  expect_lte(theta.hat, 5.6 + 0.005)
})
