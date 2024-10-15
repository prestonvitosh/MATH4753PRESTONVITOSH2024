test_that("correct list returned", {
  # Test 1:
  N <- 400
  gamma <- 0.02
  p <- 0.95
  expect_equal(ntickets(N = N, gamma = gamma, p = p)$nd, 412)
  expect_lt(abs(ntickets(N = N, gamma = gamma, p = p)$nc - 412.0152), 0.0005)
  expect_equal(ntickets(N = N, gamma = gamma, p = p)$N, N)
  expect_equal(ntickets(N = N, gamma = gamma, p = p)$p, p)
  expect_equal(ntickets(N = N, gamma = gamma, p = p)$gamma, gamma)

  # Test 2:
  N <- 200
  gamma <- 0.02
  p <- 0.95
  expect_equal(ntickets(N = N, gamma = gamma, p = p)$nd, 204)
  expect_lt(abs(ntickets(N = N, gamma = gamma, p = p)$nc - 204.3178), 0.0005)
  expect_equal(ntickets(N = N, gamma = gamma, p = p)$N, N)
  expect_equal(ntickets(N = N, gamma = gamma, p = p)$p, p)
  expect_equal(ntickets(N = N, gamma = gamma, p = p)$gamma, gamma)
})
