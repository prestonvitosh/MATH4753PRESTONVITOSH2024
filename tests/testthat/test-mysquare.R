test_that("square works", {
  # Test 1:
  expect_equal(mysquare(5), 25)

  # Test 2:
  expect_equal(mysquare(1:5), c(1, 4, 9, 16, 25))
})
