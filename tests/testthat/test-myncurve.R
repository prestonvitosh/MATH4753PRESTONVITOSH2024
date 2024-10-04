test_that("correct list returned", {
  # Test 1:
  mu <- 10
  sigma <- 5
  a <- 4
  expect_equal(myncurve(mu, sigma, a),
               list(mu = mu, sigma = sigma, area = round(pnorm(a, mu, sigma), 4)))

  # Test 2:
  mu <- 1
  sigma <- 2
  a <- 0
  expect_equal(myncurve(mu, sigma, a),
               list(mu = mu, sigma = sigma, area = round(pnorm(a, mu, sigma), 4)))

  # Test 3:
  mu <- -5
  sigma <- 3
  a <- -4
  expect_equal(myncurve(mu, sigma, a),
               list(mu = mu, sigma = sigma, area = round(pnorm(a, mu, sigma), 4)))
})
