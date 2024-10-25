test_that("myread succeeds and returned data frame has positive length", {
  # Test 1:
  csv <- "MTBE.csv"
  df <- myread(csv)
  expect_gt(length(df), 0)

  # Test 2:
  csv <- "SEEDLING.csv"
  df <- myread(csv)
  expect_gt(length(df), 0)
})
